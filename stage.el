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

(defvar stage-list nil
  "List of available stages.
Alist of (NAME . (FRAME FRAME-PARAMETERS WINDOW-CONFIGURATION))")

(defvar stage-current-stage nil
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
    (error "Stage '%S' not found." name))
  (cdr (assoc name stage-list)))

(defun stage-save-configuration (name config)
  "Save the stage configuration CONFIG as NAME.
NAME must represent an existing stage."
  (unless (stage-exists name)
    (error "Stage '%S' not found." name))
  (setcdr (assoc name stage-list) config))

(defun stage-names ()
  "Return the list of all stage names."
  (mapcar 'car stage-list))

(defun stage-restore-configuration (name)
  "Restore the stage configuration of NAME."
  (let* ((config (or (stage-configuration name)
                     (error "Stage '%S' not found." name)))
         (frame (nth 0 config))
         (window-configuration (nth 2 config)))
    (if (frame-live-p frame)
        (select-frame-set-input-focus frame)
      (let* ((fram-parameters (nth 1 config))
             (frame (make-frame frame-parameters)))
        (setf (nth 0 config) frame)))
    (set-window-configuration window-configuration)))

(defun stage-create (&optional name)
  "Create a new stage with NAME."
  (interactive (list (read-string "stage name: ")))
  (when (zerop (length name))
    (error "No name given."))
  (when (string-equal name "*all*")
    (error "System reserved space name '%S'." name))
  (setq stage-list (cons (cons name (stage-current-configuration))
                         (assoc-delete-all name stage-list)))
  (setq stage-current-stage name))

(defun stage-save ()
  "Save the configuration for the current stage."
  (interactive)
  (stage-create stage-current-stage))

(defun stage-kill-all (&optional disable-prompt)
  "Kill all stages. Prompt the user to confirm if DISABLE-PROMPT is nil."
  (interactive)
  (when (and stage-list
             (or disable-prompt (y-or-n-p "Kill all stages? ")))
    (setq stage-list nil)
    (setq stage-current-stage nil)))

(defun stage-read-name (prompt &optional collection predicate require-match)
  (or collection (setq collection (stage-names)))
;;   (ivy-read prompt collection))
  (completing-read prompt collection predicate require-match))

(defun stage-kill (name)
  "Kill the stage given by NAME."
  (interactive (list
                (if (null stage-list)
                    (error "No stage created yet.")
                  (stage-read-name "kill stage: "
                                   (append (stage-names) '("*all*")) nil t))))
  (if (string-equal name "*all*")
      (stage-kill-all)
    (when (string-equal name stage-current-stage)
      (setq stage-current-stage nil))
    (setq stage-list (assoc-delete-all name stage-list))))

(defun stage-switch (name)
  "Switch to the stage of NAME.

Before the stage is switched, the current window configuration is
saved in the current stage.
As a special case, if the stage we're switching to is the current
stage, we reload the current stage's saved configuration."
  (interactive (list (stage-read-name
                      "switch to stage: "
                      (let ((names (stage-names)))
                        (append (delete (car names) names) (list (car names)))))))
  (cond ((zerop (length name))
         (error "No name given."))
        ((null stage-list)
         (stage-create name))
        ((string-equal name stage-current-stage)
         (stage-restore-configuration name))
        (t
         (when stage-current-stage
           (stage-save-configuration stage-current-stage
                                     (stage-current-configuration)))
         (if (stage-exists name)
             (progn
               (setq stage-list (cons (assoc name stage-list)
                                      (assoc-delete-all name stage-list)))
               (setq stage-current-stage name)
               (stage-restore-configuration name))
           (stage-create name)))))

(defun stage-switch-last (arg)
  "Switch to the last visited stage.
With prefix argument, switch to the least-recently visited stage."
  (interactive "P")
  (let ((names (stage-names)))
    (if (< (length names) 2)
        (message "No stage previously visited.")
      (let ((name (if arg (car (last names)) (nth 1 names))))
        (stage-switch name)))))

(defun stage-show ()
  "Show the current stage in the minibuffer."
  (interactive)
  (cond (stage-current-stage (message "%s" stage-current-stage))
        (stage-list (message "no stage selected"))
        (t (message "no stage created"))))

(provide 'stage)

;; stage.el ends here


