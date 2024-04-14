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
Each preset defines the behaviors when the stage of NAME is
created or selected.

Element of the list: (NAME [:keyword option...]...)

:key                the hot key to create/switch to the stage.
:init               commands, functions, files, directories, and/or buffer names
                    to be called or opened in order when the stage is created.
                    The parameters could be string, list, command, or function.
                    See also `stage-run-command'.
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
                ((stage-current-name) (format "[S:%s]" (stage-current-name)))
                (t "[S]")))
  "Mode line format for Stage mode.")
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
(defsubst stage-list (&optional frame)
  "Return list of available stages in FRAME.
Alist of (NAME . (WINDOW-CONFIGURATION MARKER))"
  (frame-parameter frame 'stage-list))

(defsubst set-stage-list (stage-list &optional frame)
  "Set the stage list in FRAME to STAGE-LIST."
  (set-frame-parameter frame 'stage-list stage-list))

(defsubst stage-current-name (&optional frame)
  "Return the name of the currently selected stage in FRAME"
  (frame-parameter frame 'stage-current-name))

(defsubst set-stage-current-name (name &optional frame)
  "Set the name of the currently selected stage in FRAME to NAME"
  (set-frame-parameter frame 'stage-current-name name))

(defun stage-exists (name)
  "Return non-nil if the spage of NAME exists, otherwise return nil."
  (assoc name (stage-list)))

(defun stage-current-configuration ()
  "Return the current stage configuration in the form of
(WINDOW-CONFIGURATION MARKER)."
  (list (current-window-configuration) (point-marker)))

(defun stage-configuration (name)
  "Return the saved stage configuration of NAME.
(WINDOW-CONFIGURATION MARKER)"
  (unless (stage-exists name)
    (error "Stage %s not found." name))
  (cdr (assoc name (stage-list))))

(defun stage-save-configuration (name config)
  "Save the stage configuration CONFIG as NAME.
NAME must represent an existing stage."
  (unless (stage-exists name)
    (error "Stage %s not found." name))
  (setcdr (assoc name (stage-list)) config))

(defun stage-names ()
  "Return the list of all stage names."
  (mapcar 'car (stage-list)))

(defun stage-restore-configuration (name)
  "Restore the stage configuration of NAME."
  (let ((config (or (stage-configuration name)
                    (error "Stage %s not found." name))))
    (set-window-configuration (nth 0 config))
    (goto-char (nth 1 config))))

(defun stage-base-name (name)
  "Return name without suffix \"<suffix>\".
The suffix could be a arbitrary string."
  (let ((base (and (stringp name)
                   (replace-regexp-in-string "<.*>\\'" "" name))))
    (if (zerop (length base))
        "*NONAME*"
      base)))

(defun stage-unique-name (name)
  "Return a unique name against existing stage names.
If NAME already exists as a stage name, return NAME with a suffix \"<NUMBER>\"."
  (let ((names (stage-names))
        (base (stage-base-name name)))
    (cl-do* ((i 0 (1+ i))
             (unique-name base (format "%s<%d>" base i)))
        ((not (member unique-name names)) unique-name))))

(defun stage-default-stage ()
  "Build the window configuration and the buffer as the default stage.
Make the current frame the single window frame and switch the current buffer
to `stage-new-stage-default-buffer'."
  (when stage-new-stage-default-buffer
    (delete-other-windows)
    (select-window (split-window))
    (delete-other-windows)
    (switch-to-buffer stage-new-stage-default-buffer)))

(defun stage-read-name (prompt &optional collection predicate require-match)
  "Read a stage name in the minibuffer with completion."
  (or collection (setq collection (stage-names)))
  (completing-read prompt collection predicate require-match))


;;; Preset
(defun stage-preset (name)
  "Return the stage preset of NAME.
See `stage-presets' for the detail of return value."
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
  "Create a new stage with NAME.

If the stage already exists and DISABLE-PROMPT is non-nil,
the user is prompted to overwrite existing stage or not.

The stage is initialized with PRESET.
If PRESET is nil and the preset for NAME is defined in `stage-presets' list,
the preset in the list is applied to initialize the stage.
If PRESET is 'no-preset or the preset for NAME is not found in `stage-presets',
the stage is initialized by calling `stage-default-stage'."
  (interactive (list (let ((names (stage-names)))
                       (completing-read "create stage: " (stage-preset-names)
                                        (lambda (preset)
                                          (not (member preset names)))))))
  (when (zerop (length name))
    (error "No name given."))
  (when (or (not (stage-exists name))
            disable-prompt
            (y-or-n-p (format "overwrite existing stage %s? " name)))
    (when (stage-current-name)
      (stage-save))
    (let ((preset (cond ((eq preset 'no-preset) nil)
                        ((or preset
                             (stage-preset name))))))
      (if preset
          (with-temp-buffer
            (stage-preset-set-default-directory preset)
            (stage-preset-run-commands preset :init)
            (stage-preset-run-commands preset :command)
            (delete-other-windows))
        (stage-default-stage)))
    (set-stage-current-name name)
    (set-stage-list (cons (cons name (stage-current-configuration))
                          (assoc-delete-all name (stage-list))))
    (message "Created stage %s" (stage-current-name))))

(defun stage-duplicate ()
  "Create new stage with the current window configuration."
  (interactive)
  (when (y-or-n-p (format "[%s] duplicate stage?" (or (stage-current-name) "")))
    (let ((name (stage-unique-name (stage-current-name)))
          (stage-new-stage-default-buffer nil))
      (stage-create name nil 'no-preset))))

(defun stage-revert (name)
  "Revert the stage of NAME by initializing it."
  (interactive (list (completing-read "revert stage: " (stage-names) nil t)))
  (unless name
    (error "No stage selected."))
  (stage-create name t)
  (message "Reverted stage %s" name))

(defun stage-revert-maybe ()
  "Revert the current stage if any conditions match."
  (let* ((preset (stage-preset (stage-current-name)))
         (major-modes (stage-preset-options preset :major-mode)))
    (when (and major-modes
               (not (memq major-mode major-modes)))
      (stage-revert (stage-current-name))
      t)))

(defvar stage-rename-hist nil)

(defun stage-rename (name)
  "Rename the current stage to NAME."
  (interactive (if (null (stage-current-name))
                   (error "No stage created or selected.")
                 (list (read-string (format "[%s] rename stage: "
                                            (stage-current-name))
                                    (stage-current-name) 'stage-rename-hist))))
  (cond ((zerop (length name))
         (error "Invalid stage name."))
        ((string-equal name (stage-current-name))
         (stage-save))
        ((y-or-n-p (format "Rename %s to %s? " (stage-current-name) name))
         (setcar (assoc (stage-current-name) (stage-list)) name)
         (set-stage-current-name name)
         (stage-save)
         (message "Renamed stage to %s" (stage-current-name)))))

(defun stage-save ()
  "Save the configuration for the current stage."
  (interactive)
  (if (null (stage-current-name))
      (message "No stage selected.")
    (stage-save-configuration (stage-current-name) (stage-current-configuration))
    (message "Saved stage %s" (stage-current-name))))

(defun stage-kill (name)
  "Kill the stage of NAME."
  (interactive (list (stage-read-name "kill stage: "
                                      (or (stage-names) (error "No stage."))
                                      nil t)))
  (cond ((not (stage-exists name))
         (error "[%s] stage does not exist." name))
        ((string-equal name (stage-current-name))
         (set-stage-current-name nil)
         (set-stage-list (assoc-delete-all name (stage-list)))
         (if (stage-list)
             (stage-switch-last)
           (stage-default-stage)))
        (t
         (set-stage-list (assoc-delete-all name (stage-list)))))
    (message "[%s] stage Killed" name))

(defun stage-kill-all (&optional disable-prompt)
  "Kill all stages. Prompt the user to confirm if DISABLE-PROMPT is nil."
  (interactive)
  (cond ((null (stage-list))
         (message "No stage."))
        ((or disable-prompt (y-or-n-p "Kill all stages? "))
         (stage-default-stage)
         (set-stage-list nil)
         (set-stage-current-name nil)
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
                        (if (stage-current-name)
                            (append (cl-remove (stage-current-name) names
                                               :test #'string-equal)
                                    (list (stage-current-name)))
                          names)))))
  (when (zerop (length name))
    (error "No name given."))
  (stage-save)
  (let ((preset (or preset (stage-preset name))))
    (if (not (stage-exists name))
        (stage-create name disable-prompt preset)
      (unless (string-equal name (stage-current-name))
        (set-stage-current-name name)
        (set-stage-list (cons (assoc name (stage-list))
                              (assoc-delete-all name (stage-list))))
        (stage-restore-configuration name))
      (unless (stage-revert-maybe)
        (stage-preset-run-commands preset :after-switch)
        (stage-preset-run-commands preset :command)
        (stage-save))
      (message "Switched to stage %s" (stage-current-name)))))

(defun stage-switch-preset (number)
  "Switch to the stage defined in NUMBER-th stage preset.
NUMBER counts from zero."
  (interactive (list (read-number "preset number: ")))
  (let ((name (stage-preset-nth-name number)))
    (unless name
      (error "No %dth preset presents." number))
    (stage-switch name)))

(defmacro stage-defun-stage-switch-preset-nth (number)
  "defun a stage-switch-preset-NUMBER function."
  `(defun ,(intern (format "stage-switch-preset-%d" number)) ()
     ,(format "Swith to %dth preset stage." number)
     (interactive)
     (stage-switch-preset ,number)))

(stage-defun-stage-switch-preset-nth 0)
(stage-defun-stage-switch-preset-nth 1)
(stage-defun-stage-switch-preset-nth 2)
(stage-defun-stage-switch-preset-nth 3)
(stage-defun-stage-switch-preset-nth 4)
(stage-defun-stage-switch-preset-nth 5)
(stage-defun-stage-switch-preset-nth 6)
(stage-defun-stage-switch-preset-nth 7)
(stage-defun-stage-switch-preset-nth 8)
(stage-defun-stage-switch-preset-nth 9)

(defun stage-restore ()
  "Reload the current stage's saved configuration."
  (interactive)
  (unless (stage-current-name)
    (error "No stage selected."))
  (stage-restore-configuration (stage-current-name))
  (message "Restore stage %s" (stage-current-name)))

(defun stage-switch-last (&optional arg)
  "Switch to the last selected stage.
With prefix argument, switch to the least-recently selected stage."
  (interactive "P")
  (let* ((names (if arg (reverse (stage-names)) (stage-names)))
         (name (if (string-equal (nth 0 names) (stage-current-name))
                   (nth 1 names)
                 (nth 0 names))))
    (if (null name)
        (message "No stage previously selected.")
      (stage-switch name))))

(defun stage-switch-least (&optional arg)
  "Switch to the least-recently selected stage.
With prefix argument, switch to the last selected stage."
  (interactive "P")
  (stage-switch-last (not arg)))

(defun stage-show ()
  "Show the current stage name in the minibuffer."
  (interactive)
  (cond ((stage-current-name) (message "stage: %s" (stage-current-name)))
        ((stage-list) (message "no stage selected"))
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
    (stage-switch name disable-prompt preset)))

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
  (set-stage-list nil)
  (set-stage-current-name nil)
  (stage-setup-preset)
  (advice-add 'projectile-switch-project-by-name :around
              #'stage-switch-projectile-after)
  (advice-add 'counsel-projectile-switch-project-by-name :around
              #'stage-switch-projectile-after))

(defun stage-cleanup ()
  (stage-kill-all t)
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
