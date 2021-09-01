;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;;; cemacs-utility.el --- A collection of useful helper and interactive functions in one tidy package

;;; Code:

;; Constants
(defconst cemacs-universal-argument  '(4)
  "Represents the value a single `universal-argument' call passes.
The value essentially a list with the single value of 4"
  )
(defconst cemacs-universal-argument-double  '(16)
  "Represents the value two `universal-argument' calls passes.
The value essentially a list with the single value of 16"
  )
;; Variables
(defvar cemacs-custom-directory-list nil
  "A list of custom directories being used by this config."
  )
(defvar cemacs-custom-directory-clean-list nil
  "A list of custom config directories in which is safe to delete.
This is without without too much trouble when cleaning up,
 especially after a clean Emacs install.
This primarily targets files in `cemacs-custom-directory-list' but
 it isn't actually a pre-requisite"
  )
(defvar cemacs-buffer-tmp-list nil
  "A list of buffers that is safe to semi-clean up at any point.
This helps with cleaning up a bunch of buffers in bulk, particularly
scratch buffers.")
(defvar cemacs-personal-config-file (concat user-emacs-directory "src/personal.el")
  "A custom file that applies personal or situation dependant configuration.

This is applied late in the init process, or can manually be reloaded.
If you do not want to polluted by upstream changes in personal.el you might
want to change this variable.

Due to the nature of when this file is loaded, it is limited to configuration
is safe to be applied after everything else has laoded."
  )
;; Custom Functions
(defun cemacs-add-multiple (list-symbol &rest entries)
  "Add each item from ENTRIES to LIST-SYMBOL, skipping duplicates.

The returned value is the modified list.

This is just a shorthand function"
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry))
  (symbol-value list-symbol)
  )
(defalias 'cemacs-add-multiple-to-list 'cemacs-add-multiple
  "Obsolete functin, use the shorthand instead.")
(defun cemacs-append-multiple (list-symbol &rest entries)
  "Append each item in ENTRIES to LIST-SYMBOL, skipping duplicates.

This is just a shorthand function."
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry :append))
  )
(defalias 'cemacs-append-multiple-to-list 'cemacs-append-multiple
  "Obsolete function, use the shorthand instead."
  )
(defun cemacs-add-multiple-splicing (list-symbol &rest lists)
  "Add each entry in each list from LISTS to LIST-SYMBOL, skipping duplicates.

The returned value is the modified list.

This is just a shorthand function."
  (interactive)
  (dolist (x-list lists)
    (dolist (x-entry x-list)
      (add-to-list list-symbol x-entry)
      ))
  (symbol-value list-symbol)
  )
(defun cemacs-char-at-pos (&optional pos)
  "Return the char at POS without properties."
  (interactive)
  (let ((position (or pos (point))))
    (buffer-substring-no-properties (point) (+ 1 (point)))
    )
  )
(defun cemacs-org-hide-all-subtrees ()
  "Hide all global subtrees in orgmode."
  (interactive)
  (org-global-cycle 1)
  )
(defun cemacs-excursion (&rest body)
  "Do a `save-excursion' and return the point."
  (save-excursion
    `(eval ,@body)
    (point))
  )
(defun cemacs-startup-time ()
  (float-time
   (time-subtract after-init-time before-init-time))
  )
;; End of helper only functions
(defun cemacs-forward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to a new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-forward " \t\n")
    (skip-chars-forward " \t")
    )
  )
(defun cemacs-backward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to an new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-backward " \t\n")
    (skip-chars-backward " \t")
    )
  )
(defun slay-function ()
  "Kill the function surrounding the point.
Emacs' built in 'mark-defun' is used so that is what determines what is
aconsidered a function,
This function is effectively a shorthand of 'mark-defun' 'kill-region'."
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun slay-whole-buffer ()
  "Kill the buffer in it's entirety."
  (interactive)
  (kill-region (point-min) (point-max))
  (kill-region (region-beginning) (region-end))
  )
(defun cemacs-activate-mark ()
  "Activates the region between the point and mark.
This is a good alternative to dealing with functions that assume
you use transient mark mode since it lets you run without having to
traverse back to set the region again"
  (interactive)
  (activate-mark)
  )
(defun cemacs-delete-word (&optional mult)
  "Delete characters forward until encountering the end of a word.
With argument MULT, repeat this that many times, or perform deletion backwards
if negative.

This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word (or mult 1))
     (point))))
(defun cemacs-delete-word-backwards (&optional mult)
  "Delete characters backward until encountering the beginning of a word.
With argument MULT, repeat this many times.

This command is a reverse of `cemacs-delete-word'"
  (interactive "p")
  (cemacs-delete-word (- (or mult 1)))
  )
(defun cemacs-warn (warning-message)
  "Create a warning event outputting WARNING-MESSAGE duplicate it to the minibuffer."
  (display-warning 'emacs
                   warning-message
                   :debug)
  (message warning-message)
  )
(defun cemacs-find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  ;; Use truename to follow symlinks
  (find-file (file-truename user-init-file))
  )
(defun cemacs-open-files-background (filelist)
  "Open all files in FILELIST in offscreen buffers."
  (dolist (x-file filelist)
    (if (and (file-exists-p x-file ) (file-regular-p x-file))
           (find-file-noselect x-file)
         )))
(defun cemacs-open-files-in-directory (directory-path)
  "Opens all files in a DIRECTORY-PATH in offscreen buffers."
  (if (file-directory-p directory-path)
      (cemacs-open-files-background
       (directory-files directory-path t)
       ))
  )
(defun cemacs-defdir (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-DIR a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for
than then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be
specified whether or not this directory contains LOCAL-ONLY files that aren't
too important if they are lost between computers when LOCAL-ONLY is non-nil"
  (interactive)
  (push new-dir cemacs-custom-directory-list)
  (set var-name new-dir)
  ;; A value is supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir)
    )
  ;; Directory does not already exist
  (if (not (file-exists-p new-dir))
      (make-directory new-dir :recursive)
    (cemacs-warn (concat new-dir " has a special file or directory already present!"))
    )
  )
(defun cemacs-deffile (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-DIR a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for
than then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be
specified whether or not this directory contains LOCAL-ONLY files that aren't
too important if they are lost between computers when LOCAL-ONLY is non-nil"
  (interactive)
  (set var-name new-dir)
  (push new-dir cemacs-custom-directory-list)
  ;; A value supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir)
    )
  (if (not (file-exists-p new-dir))
      (make-empty-file new-dir :recursive)
    (cemacs-warn (concat new-dir " has a special file or directory already present!"))
    )
  )
(defvar cemacs-kill-volatile-buffer-pre-hook nil)
(defun cemacs-buffer-kill-volatile ()
  "Kill the current buffer unconditionally.

This function should be used with care, since it will NOT ask to save work.
This config uses a modified `backup-each-save' to make sure there are always
very recent versions of work kept around.
This means the buffer is saved elsewhere before killing, so work is
relatively safe."
  (interactive)
  (run-hooks 'cemacs-kill-volatile-buffer-pre-hook)
  ;; Slight hack required to bypass user promtps,
  ;; since its not offered by the generic 'kill-buffer' function
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer))
  )
(defun cemacs-scratch-buffer-create (&optional new-major-mode)
  "Create a new scratch buffer optionally with a default NEW-MAJOR-MODE."
  (interactive)
  (let* ((scratch-major-mode (if (fboundp new-major-mode)
                                 new-major-mode
                               'emacs-lisp))
         (new-buffer (generate-new-buffer
                      (concat "*scratch-" (symbol-name scratch-major-mode) "*")
                      )))
    (switch-to-buffer new-buffer)
    (add-to-list 'cemacs-buffer-tmp-list new-buffer)
    (setq-local major-mode 'c++-mode)
    (funcall major-mode)
    )
  )
(defun cemacs-scratch-buffer-create-cpp ()
  "Create a new scratch buffer optionally with the mode 'c++-mode'."
  (interactive)
  (cemacs-scratch-buffer-create 'c++-mode)
  )
(defun cemacs-scratch-buffer-create-c ()
  "Create a new scratch buffer optionally with the default mode 'c-mode'."
  (interactive)
  (cemacs-scratch-buffer-create 'c-mode)
  )
(defun cemacs-scratch-buffer-kill-all ()
  "Kill all scratch buffers created by cemacs."
  (interactive)
  (while (car cemacs-buffer-tmp-list)
    ;; Just have to deal with prompts for saved files here
    (kill-buffer (pop cemacs-buffer-tmp-list)))
  )
(defun cemacs-personal-reload-config ()
  "Reload the bindings found in personal.el"
  (interactive)
  (load cemacs-personal-config-file)
  )
(provide 'cemacs-utility)
;;; cemacs-utility.el ends here
