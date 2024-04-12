;;; stage.el --- yet another emacs window session manager

;; Copyright (C) 2024 Tokuya Kameshima

;; Author: Tokuya Kameshima <kametoku at gmail dot com>
;; Keywords: frames convenience
;; Homepage: https://github.com/kametoku/stage

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


(require 'projectile)

;;; Customization
(defgroup stage-mode nil
  "Manage emacs window session easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/kametoku/stage"))

(defcustom stage-keymap-prefix nil
  "Stage keymap prefix."
  :group 'stage
  :type 'string)

(defcustom stage-new-stage-default-buffer "*scratch*"
  "Default buffer to display in newly-created stage."
  :type 'string
  :group 'stage)

(defcustom stage-presets nil
  "List of default stage presets.

Element of the list: (NAME [:keyword option...]...)

:key                the hot key to create/switch to the stage.
:init               commands, functions, files, directories, and/or buffer names
                    to call or open when the stage is created.
                    The parameters could be string, list, command, or function.
:default-directory  the default directory name of the stage.
:after-switch       commands and so on when the stage is switched to.
:command            commands and so on when the stage is created or
                    switched to.
:major-mode         primary major modes of the stage.
                    When the stage is switched to and its major mode
                    is not any of this parameters, the steage is reverted
                    by initializing it (the stage is recreated)."
  :group 'stage
  :type 'list)

(defcustom stage-projectile-switch-project-action #'projectile-dired
  "Action invoked after creating stage with ‘stage-switch-projectile’.

See `projectile-switch-project-action' for more information."
  :group 'stage
  :type 'function)

(defconst stage-mode-line-format
  '(:eval (cond ((not stage-mode) "")
                (stage-current-name (format "[S:%s]" stage-current-name))
                (t "[S]"))))
(put 'stage-mode-line-format 'risky-local-variable t)


;;; Utilities
(defun stage--dir-local-variables (&optional directory)
  "Return alist of directory local variables defined in \".dir-locals.el\"
file of DIRECTORY."
  (with-temp-buffer
    (let* ((default-directory
            (expand-file-name (or directory default-directory)))
           (dir-variables (hack-dir-local--get-variables nil)))
      (when (string-equal (car dir-variables) default-directory)
        (cdr dir-variables)))))

(defun stage--dir-local-variable (var &optional directory)
  "Return the value of variable VAR defined in \".dir-locals.el\" of DIRECTORY."
  (cdr (assq var (stage--dir-local-variables directory))))


;;; Stage
(defvar stage-list nil
  "List of available stages.
Alist of (NAME . (FRAME FRAME-PARAMETERS WINDOW-CONFIGURATION))")

(defvar stage-current-name nil
  "The name of the currently selected stage.")

(defun stage-exists (name)
  "Return non-nil if the spage of NAME exists, otherwise return nil."
  (assoc name stage-list))

(defun stage-current-configuration ()
  "Return the current stage configuration in the form of
(FRAME FRAME-PARAMETERS WINDOW-CONFIGURATION)."
  (let ((frame (selected-frame)))
    (list frame (frame-parameters frame) (current-window-configuration))))

(defun stage-configuration (name)
  "Return the saved stage configuration of NAME.
(FRAME FRAME-PARAMETERS WINDOW-CONFIGURATION)"
  (unless (stage-exists name)
    (error "Stage %s not found." name))
  (cdr (assoc name stage-list)))

(defun stage-save-configuration (name config)
  "Save the stage configuration CONFIG as NAME.
NAME must represent an existing stage."
  (unless (stage-exists name)
    (error "Stage %s not found." name))
  (setcdr (assoc name stage-list) config))

(defun stage-names ()
  "Return the list of all stage names."
  (mapcar 'car stage-list))

(defun stage-restore-configuration (name)
  "Restore the stage configuration of NAME."
  (let* ((config (or (stage-configuration name)
                     (error "Stage %s not found." name)))
         (frame (nth 0 config))
         (window-configuration (nth 2 config)))
    (if (frame-live-p frame)
        (select-frame-set-input-focus frame)
      (let* ((fram-parameters (nth 1 config))
             (frame (make-frame frame-parameters)))
        (setf (nth 0 config) frame)))
    (set-window-configuration window-configuration)))

(defun stage-reset-current-stage (&optional frame)
  (setq stage-current-name nil))

(defun stage-base-name (name)
  "Return name without suffix \"<number>\"."
  (let ((base (and (stringp name)
                   (replace-regexp-in-string "<[0-9]+>\\'" "" name))))
    (if (zerop (length base))
        "*NONAME*"
      base)))

(defun stage-unique-name (name)
  "Return a unique name against existing stage names.
If NAME already exists as a stage name, return NAME with suffix \"<NUMBER>\"."
  (let ((names (stage-names))
        (base (stage-base-name name)))
    (cl-do* ((i 0 (1+ i))
             (unique-name base (format "%s<%d>" base i)))
        ((not (member unique-name names)) unique-name))))

(defun stage-default-stage ()
  (when stage-new-stage-default-buffer
    (delete-other-windows)
    (select-window (split-window))
    (delete-other-windows)
    (switch-to-buffer stage-new-stage-default-buffer)))

(defun stage-read-name (prompt &optional collection predicate require-match)
  (or collection (setq collection (stage-names)))
  (completing-read prompt collection predicate require-match))


;;; Preset
(defun stage-preset (name)
  "Return the stage preset of NAME."
  (cdr (assoc (stage-base-name name) stage-presets)))

(defun stage-preset-names ()
  "Return the list of all preset names."
  (mapcar 'car stage-presets))

(defun stage-preset-nth-name (number)
  "Return the name of NUMBER-th stage preset.
NUMBER counts from zero."
  (nth number (stage-preset-names)))

(defun stage-preset-options (preset keyword)
  "Return options for KEYWORD of stage preset PRESET."
  (seq-take-while (lambda (elt) (not (keywordp elt)))
                  (cdr (memq keyword preset))))

(defun stage-run-command (command)
  "Run COMMAND.

If COMMAND is a string and there exists the directory, open it by `dired'.
If COMMAND is a string and there exists the file, open it by `find-file'.
If COMMAND is a string and there exists tue buffer, open it by `switch-to-buffer'.
If COMMAND is an interactive function, `call-interactively' it.
If COMMAND is a function, `funcall' it.
If COMMAND is a list, `funcall' it in a lambda form.
Ohterwise an error is raised."
  (cond ((and (stringp command) (file-directory-p command))
         (dired command))
        ((and (stringp command) (file-exists-p command))
         (find-file command))
        ((and (stringp command) (get-buffer command))
         (switch-to-buffer command))
        ((stringp command)
         (error "No such file or directory: %s" command))
        ((commandp command) (call-interactively command))
        ((functionp command) (funcall command))
        ((listp command) (funcall `(lambda () ,command)))
        (t (error "Cannot handle command: %s" command))))

(defun stage-preset-run-commands (preset keyword)
  "Run commands defined in parameters of KEYWORD from PRESET."
  (let ((commands (stage-preset-options preset keyword)))
    (mapc #'stage-run-command commands)))

(defun stage-preset-set-default-directory (preset)
  "Set `default-directory' accordingly :default-directory parameter in PRESET."
  (let ((dir (car (stage-preset-options preset :default-directory))))
    (when (listp dir)
      (setq dir (funcall `(lambda () ,dir))))
    (when dir
      (setq default-directory dir))))


;;; Interactive functions
(defun stage-create (name &optional disable-prompt preset)
  "Create a new stage with NAME."
  (interactive (list (let ((names (stage-names)))
                       (completing-read "create stage: " (stage-preset-names)
                                        (lambda (preset)
                                          (not (member preset names)))))))
  (when (zerop (length name))
    (error "No name given."))
  (when (string-equal name "*all*")
    (error "System reserved space name: %s" name))
  (when (or (not (stage-exists name))
            disable-prompt
            (y-or-n-p (format "overwrite existing stage %s? " name)))
    (when stage-current-name
      (stage-save))
    (stage-default-stage)
    (unless (eq preset 'no-preset)
      (let ((preset (or preset (stage-preset name))))
        (stage-preset-run-commands preset :init)
        (stage-preset-set-default-directory preset)
        (stage-preset-run-commands preset :command)))
    ;;
    (setq stage-current-name name)
    (setq stage-list (cons (cons name (stage-current-configuration))
                           (assoc-delete-all name stage-list)))
    (message "Created stage %s" stage-current-name)))

(defun stage-duplicate ()
  "Create new stage with the current window configuration."
  (interactive)
  (when (y-or-n-p (format "[%s] duplicate stage?" (or stage-current-name "")))
    (let ((name (stage-unique-name stage-current-name))
          (stage-new-stage-default-buffer nil))
      (stage-create name nil 'no-preset))))

(defun stage-revert (&optional disable-prompt)
  "Revert the current stage by initializing it."
  (interactive "P")
  (unless stage-current-name
    (error "No stage selected."))
  (when (or disable-prompt
            (y-or-n-p (format "[%s] revert current stage? "
                              stage-current-name)))
    (stage-create stage-current-name t)
    (message "Reverted stage %s" stage-current-name)))

(defun stage-revert-maybe ()
  (let* ((preset (stage-preset stage-current-name))
         (major-modes (stage-preset-options preset :major-mode)))
    (when (and major-modes
               (not (memq major-mode major-modes)))
      (stage-revert t)
      t)))

(defvar stage-rename-hist nil)

(defun stage-rename (name)
  "Rename the current stage to NAME."
  (interactive (if (null stage-current-name)
                   (error "No stage created or selected.")
                 (list (read-string (format "[%s] rename stage: "
                                            stage-current-name)
                                    nil 'stage-rename-hist))))
  (cond ((zerop (length name))
         (error "Invalid stage name."))
        ((string-equal name stage-current-name)
         (error "Cannot rename to the same name."))
        ((y-or-n-p (format "Rename %s to %s? " stage-current-name name))
         (setcar (assoc stage-current-name stage-list) name)
         (setq stage-current-name name)
         (stage-save)
         (message "Renamed stage to %s" stage-current-name))))

(defun stage-save ()
  "Save the configuration for the current stage."
  (interactive)
  (if (null stage-current-name)
      (message "Stage not created.")
    (stage-save-configuration stage-current-name (stage-current-configuration))
    (message "Saved stage %s" stage-current-name)))

(defun stage-kill (name)
  "Kill the stage of NAME."
  (interactive (list (stage-read-name "kill stage: "
                                      (or (stage-names) (error "No stage."))
                                      nil t)))
  (cond ((not (stage-exists name))
         (error "[%s] stage does not exist." name))
        ((string-equal name stage-current-name)
         (setq stage-current-name nil)
         (setq stage-list (assoc-delete-all name stage-list))
         (if stage-list
             (stage-switch-last)
           (stage-default-stage)))
        (t
         (setq stage-list (assoc-delete-all name stage-list))))
    (message "[%s] stage Killed" name))

(defun stage-kill-all (&optional disable-prompt)
  "Kill all stages. Prompt the user to confirm if DISABLE-PROMPT is nil."
  (interactive)
  (cond ((null stage-list)
         (message "No stage."))
        ((or disable-prompt (y-or-n-p "Kill all stages? "))
         (stage-default-stage)
         (setq stage-list nil)
         (setq stage-current-name nil)
         (message "Killed all stages."))))

(defun stage-switch (name &optional disable-prompt preset)
  "Switch to the stage of NAME.

Before the stage is switched, the current window configuration is
saved in the current stage."
  (interactive (list (stage-read-name
                      "switch to stage: "
                      (let ((names (cl-remove-duplicates
                                    (append (stage-names) (stage-preset-names))
                                    :test #'string-equal :from-end t)))
                        (if stage-current-name
                            (append (cl-remove stage-current-name names)
                                    (list stage-current-name))
                          names)))))
  (when (zerop (length name))
    (error "No name given."))
  (stage-save)
  (let ((preset (or preset (stage-preset name))))
    (if (not (stage-exists name))
        (stage-create name disable-prompt preset)
      (unless (string-equal name stage-current-name)
        (setq stage-list (cons (assoc name stage-list)
                               (assoc-delete-all name stage-list)))
        (setq stage-current-name name)
        (stage-restore-configuration name))
      (unless (stage-revert-maybe)
        (stage-preset-run-commands preset :after-switch)
        (stage-preset-run-commands preset :command)
        (stage-save))
      (message "Switched to stage %s" stage-current-name))))

(defun stage-switch-preset (number)
  "Switch to the stage defined in NUMBER-th stage preset.
NUMBER counts from zero."
  (interactive (list (read-number "preset number: ")))
  (let ((name (stage-preset-nth-name number)))
    (unless name
      (error "No %dth preset presents." number))
    (stage-switch name)))

(defmacro stage-defun-stage-switch-reset-nth (number)
  `(defun ,(intern (format "stage-switch-preset-%d" number)) ()
     ,(format "Swith to %dth preset stage." number)
     (interactive)
     (stage-switch-preset ,number)))

(stage-defun-stage-switch-reset-nth 0)
(stage-defun-stage-switch-reset-nth 1)
(stage-defun-stage-switch-reset-nth 2)
(stage-defun-stage-switch-reset-nth 3)
(stage-defun-stage-switch-reset-nth 4)
(stage-defun-stage-switch-reset-nth 5)
(stage-defun-stage-switch-reset-nth 6)
(stage-defun-stage-switch-reset-nth 7)
(stage-defun-stage-switch-reset-nth 8)
(stage-defun-stage-switch-reset-nth 9)

(defun stage-restore ()
  "Reload the current stage's saved configuration."
  (interactive)
  (unless stage-current-name
    (error "No stage selected."))
  (stage-restore-configuration stage-current-name)
  (message "Restore stage %s" stage-current-name))

(defun stage-switch-last (&optional arg)
  "Switch to the last visited stage.
With prefix argument, switch to the least-recently visited stage."
  (interactive "P")
  (let* ((names (stage-names))
         (name (cond (arg (car (last names)))
                     (stage-current-name (nth 1 names))
                     (t (nth 0 names)))))
    (if (or (null name)
            (string-equal name stage-current-name))
        (message "No stage previously visited.")
      (stage-switch name))))

(defun stage-switch-least (arg)
  "Switch to the least-recently visited stage.
With prefix argument, switch to the last visited stage."
  (interactive "P")
  (stage-switch-last (not arg)))

(defun stage-show ()
  "Show the current stage in the minibuffer."
  (interactive)
  (cond (stage-current-name (message "stage: %s" stage-current-name))
        (stage-list (message "no stage selected"))
        (t (message "no stage created"))))


;;; Projectile integration
(defun stage-projectile-project-name (&optional project)
  "Return project name of projectile PROJECT."
  (let ((project (or project (projectile-project-root))))
    (or (stage--dir-local-variable 'projectile-project-name project)
        (let ((projectile-project-name nil))
          ;; The variable `projectile-project-name' may not be set for
          ;; PROJECT at this moment.
          (projectile-project-name project)))))

(defun stage-switch-projectile (project &optional disable-prompt preset)
  "Switch to stage of projectile PROJECT."
  (interactive (list (projectile-completing-read "switch to project stage: "
                                                 projectile-known-projects)))
  (let* ((name (stage-projectile-project-name project))
         (preset (or preset
                     (stage-preset name)
                     `(:init (let ((projectile-switch-project-action
                                    stage-projectile-switch-project-action))
                               (projectile-switch-project-by-name ,project)))))
         (stage-new-stage-default-buffer nil))
    (stage-create name disable-prompt preset)))

(defun stage-switch-projectile-after (func project &rest args)
  "Switch to stage of projectile PROJECT after invoking FUNC with PROJECT and ARGS."
  (stage-switch-projectile project t
                           `(:command (lambda () (apply ,func ',(cons project args))))))

(defun stage-switch-dwim (&optional arg)
  "Switch to stage.
With a `\\[universal-argument]' prefix argument, call `stage-switch-projectile'.
With a `\\[universal-argument] `\\[universal-argument]' prefix argument, call `stage-create'.
Otherwise, call `stage-switch'."
  (interactive "P")
  (let ((func (cond ((equal arg '(16)) #'stage-create)
                    (arg #'stage-switch-projectile)
                    (t #'stage-switch))))
    (call-interactively func)))


;;; Stage Minor Mode
(defvar stage-command-map
  (let ((map (make-sparse-keymap)))
    (when stage-keymap-prefix
      (define-key map (kbd stage-keymap-prefix) #'stage-switch-last))
    (define-key map (kbd "c") #'stage-create)
    (define-key map (kbd "d") #'stage-duplicate)
    (define-key map (kbd "g") #'stage-switch)
    (define-key map (kbd "G") #'stage-switch-preset)
    (define-key map (kbd "k") #'stage-kill)
    (define-key map (kbd "K") #'stage-kill-all)
    (define-key map (kbd "l") #'stage-switch-last)
    (define-key map (kbd "n") #'stage-switch-least)
    (define-key map (kbd "p") #'stage-switch-projectile)
    (define-key map (kbd "r") #'stage-rename)
    (define-key map (kbd "s") #'stage-save)
    (define-key map (kbd "u") #'stage-restore)
    (define-key map (kbd "v") #'stage-revert)
    (define-key map (kbd "w") #'stage-show)
    (define-key map (kbd ";") #'stage-switch-dwim)
    (define-key map (kbd "0") #'stage-switch-preset-0)
    (define-key map (kbd "1") #'stage-switch-preset-1)
    (define-key map (kbd "2") #'stage-switch-preset-2)
    (define-key map (kbd "3") #'stage-switch-preset-3)
    (define-key map (kbd "4") #'stage-switch-preset-4)
    (define-key map (kbd "5") #'stage-switch-preset-5)
    (define-key map (kbd "6") #'stage-switch-preset-6)
    (define-key map (kbd "7") #'stage-switch-preset-7)
    (define-key map (kbd "8") #'stage-switch-preset-8)
    (define-key map (kbd "9") #'stage-switch-preset-9)
    map)
  "Keymap for Stage commands after `stage-keymap-prefix'.")
(fset 'stage-command-map stage-command-map)

(defvar stage-mode-map
  (let ((map (make-sparse-keymap)))
    (when stage-keymap-prefix
      (define-key map (kbd stage-keymap-prefix) 'stage-command-map))
    map)
  "Keymap for `stage-mode'.")

(defun stage-setup-preset-key (name keys)
  "Define keys in stage presets."
  (mapc (lambda (key)
          (define-key stage-command-map (kbd key)
                      (cons (format "stage-switch [%s]" name)
                            `(lambda ()
                               (interactive)
                               (stage-switch ,name)))))
        keys))

(defun stage-setup-preset ()
  "Setup stage presets.
Define keys in stage presets."
  (interactive)
  (mapc (lambda (name)
          (let* ((preset (stage-preset name))
                 (keys (delete nil (stage-preset-options preset :key))))
            (stage-setup-preset-key name keys)))
        (stage-preset-names)))

(defun stage-setup ()
  (setq stage-list nil)
  (setq stage-current-name nil)
  (stage-setup-preset)
  (add-hook 'after-make-frame-functions 'stage-reset-current-stage)
  (advice-add 'projectile-switch-project-by-name :around
              #'stage-switch-projectile-after)
  (advice-add 'counsel-projectile-switch-project-by-name :around
              #'stage-switch-projectile-after))

(defun stage-cleanup ()
  (stage-kill-all t)
  (remove-hook 'after-make-frame-functions 'stage-reset-current-stage)
  (advice-remove 'projectile-switch-project-by-name
                 #'stage-switch-projectile-after)
  (advice-remove 'counsel-projectile-switch-project-by-name
              #'stage-switch-projectile-after))

(define-minor-mode stage-mode
  "Minor mode to manage emacs window sessions.

\\{stage-mode-map}"
  :keymap stage-mode-map
  :group 'stage
  :require 'stage
  :global t
  (if stage-mode
      (stage-setup)
    (stage-cleanup)))

(provide 'stage)

;; stage.el ends here
