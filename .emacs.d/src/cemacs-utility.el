;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrent whatsoever.

;;; cemacs-utility.el --- A collection of useful helper and interactive functions in one tidy package

;;; Code:
;; Dependencies
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
(defun cemacs-natural-delete-whitespace ()
  "An alternative to `delete-whitespace-horizontally' which traverses lines."
  (interactive)
  (let ((whitespace-start (cemacs-excursion (cemacs-backward-whitespace :cross-lines)))
        (whitespace-end (cemacs-excursion (cemacs-forward-whitespace :cross-lines)))
        )
    (delete-region whitespace-start whitespace-end)
    )
  )
(defun cemacs-natural-one-space ()
  "An alternative to `just-one-space' which traverses lines."
  (interactive)
  (let ((whitespace-start (cemacs-excursion (cemacs-backward-whitespace :cross-lines)))
        (whitespace-end (cemacs-excursion (cemacs-forward-whitespace :cross-lines)))
        )
    (delete-region whitespace-start whitespace-end)
    (insert-char ?\s)
    )
  )
(defun cemacs-natural-beginning-of-line ()
  "A version of `beginning-of-line' that acknowledges significant stops.

This function will move the point in the order
- beginning of visual line
- beginning of logical line, the real line, jumping to first significnat char
- beginning of the true line

The behaviour for this function is borrowed concept of the package crux
\(a Collcetion of Rediciously Useful eXtensions).
This allowed for jumpping to the first significant character, rather than
whitespace, which was generally more useful.
Optionally jumping to the very beginning of the line, if already on the first
significant character.

However, the problem with this function is it ignored visual linnes, which was
really confusing.
Since truncating long lines is really, really, annoying, and not a good
 alternative.
This fixes that problem and visits the beginning of the visual line first."
  (interactive)
  (let ((original-point (point))
        (original-line (line-number-at-pos (point)))
        ;; Aparent line wrapped line beginning
        (visual-line-beginning (save-excursion (beginning-of-visual-line) (point)))
        ;; First significant character of the true line
        (logical-line-beginning (save-excursion (beginning-of-line)
                                                (cemacs-forward-whitespace) (point)))
        (true-line-beginning (line-beginning-position))
        )
    (cond ((= original-point visual-line-beginning)
           ;; try jumping to the true line beginning, first significant char
           (goto-char logical-line-beginning)
           )
          ((= original-point logical-line-beginning)
           (beginning-of-line)
           )
          ((= original-point true-line-beginning)
           (goto-char logical-line-beginning)
           )
          ;; Default behaviour
          (t (goto-char visual-line-beginning)
             (cemacs-forward-whitespace))
          ))
  )
(defun cemacs-natural-end-of-line ()
  "A version of `end-of-line' that stops at a visual line end.

This function allows the user to choose if they wish to visit the end
of the visual line, or the end of the real line.

Pressing once will try you to the end of the visual line,
pressing twice will always ensure you end up at the end of the real line."
  (interactive)
  (let ((original-point (point))
        ;; Where 'end-of-visual-line' thinks we should land
        (aparent-visual-line-number (save-excursion
                                      (end-of-visual-line)
                                      (line-number-at-pos)))
        ;; Aparent line wrapped line end
        (visual-line-end (save-excursion (end-of-visual-line) (point)))
        (true-line-end (line-end-position))
        )
    (cond ((not (= aparent-visual-line-number (line-number-at-pos)))
           ;; Something went wrong and we changed lines
           ;; It was likely invisible text, do a normal end of line
           (end-of-line)
           )
          ((= original-point visual-line-end)
           (goto-char true-line-end)
           )
          (:default (goto-char visual-line-end))
          ))
  )
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
(defun cemacs-natural-forward-word ()
  "Move the point 1 word block forwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
\(defun cemacs-natural-delete-word ()
  |(interactive))

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-'

Which is both confusing and wholly excessive in nature.

This version will delete all whitespace forwards when 2 or more blank/whitespace
characters are present, or 1 newline, leaving actual characters alone.

A call only 1 character away from a 'word' object  will result in 'normal' EMACS
 word deletion.
In fact, this uses a custom word deletion function, so as to not pollute the
kill ring."
  (interactive)
  (let ((original-point (point)))
    ;; Following two characters are whitespace/blank or end of line
    (cond ((= (line-end-position) (point))
           (cemacs-forward-whitespace :traverse-newlines)
           )
          ;; Following two characters are whitespace/blank
          ((and (string-match "[[:blank:]]" (string (char-after)))
                (string-match "[[:blank:]]" (string (char-after
                                                     (+ (point) 1))
                                                    )))
           ;; traverse all whitespace upto line beginning
           (cemacs-forward-whitespace)
           )
          ;; Normal backward word
          (:default (forward-word 1)
                    )
          )
    ;; Keep the point inside an input field where applicable
    (constrain-to-field nil (point))
    (point)
    )
  )
(defun cemacs-natural-backward-word ()
  "Move the point 1 word block backwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word movement behaviour, where each command stops after whitespace,
so you have a little more granular control over how you traverse words,
rather than skipping across lines haphazardly.
e.g. by default (if we assume '|' is the cursor)
\(defun cemacs-natural-delete-word ()
  |(interactive))

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-'

Which is both confusing and wholly excessive in nature.

This version will move travese all whitespace backwards when 2 or more
blank/whitespace characters are present, or 1 newline, leaving actual
characters alone.

A call only 1 character away from a 'word' object  will result in using
the `backwards-to-word' command.

This will also attempt to prevent traversing read-only prompt text
This will also move the cursor to the beginning of the line if travesing
lines."
  (interactive)
  (let ((original-point (point))
        (original-line (line-number-at-pos (point)))
        )
    ;; Delete whitespace hungrily if at line beginning across lines
    (cond ((= (line-beginning-position) (point))
           (cemacs-backward-whitespace :traverse-newlines)
           )
          ;; Previous two characters are whitespace/blank
          ((and (string-match "[[:blank:]]" (string (char-before)))
                (string-match "[[:blank:]]" (string (char-before
                                                     (- (point) 1))
                                                    )))
           ;; traverse all whitespace upto line beginning
           (cemacs-backward-whitespace)
           )
          ;; Normal backward word
          (:default
           (backward-word 1)
           (if (not (= original-line (line-number-at-pos (point))))
               ;; Try not to move the cursor too far
               (end-of-line)
             ))
          )
    ;; Keep the point inside an input field where applicable
    (constrain-to-field nil original-point)
    (point)
    )
  )
(defun cemacs-natural-delete-word ()
  "Delete a word block backwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
\(defun cemacs-natural-delete-word ()
  |(interactive))

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-'

Which is both confusing and wholly excessive in nature.

This version will delete all whitespace forwards when 2 or more blank/whitespace
characters are present, or 1 newline, leaving actual characters alone.

A call only 1 character away from a 'word' object  will result in 'normal' EMACS
 word deletion.
In fact, this uses a custom word deletion function, so as to not pollute the
kill ring."
  (interactive)
  (let ((original-point (point)))
    ;; Following two characters are whitespace/blank or end of line
    (cemacs-natural-forward-word)
    (delete-region original-point (point))
    )
  )
(defun cemacs-natural-delete-word-backwards ()
  "Delete a word block backwards, treating whitespace blocks as words.

This command uses `cemacs-natural-backward-word' to achieve more sane deletion
 behaviour.
It will delete all whitespace forwards when 2 or more blank/whitespace
characters are present, or 1 newline, leaving actual characters alone.

A call only 1 character away from a 'word' object  will result in close to
EMACS `backward-to-char' movement deleting everything between the new and
 old points."
  (interactive)
  (let ((original-point (point)))
    (cemacs-natural-backward-word)
    (delete-region original-point (point))
    )
  )
(defun cemacs-natural-tab-to-tab-stop ()
  "Move following word block to the next tab stop.

The original `tab-to-tab-stop' command has this confusing quirk where it might
not actually land on a tab stop if there is already indentation in front of it,
and it put the cursor on the tab stop, instead of the text following the whitespace.
The reasoning for this was likely to move the cursor to the tab stop, rather than
the following text.
It might be far more desirable to move text to tab stops, rather than the cursor.

The result is that the command will consistently move the beginning of the text
immediately following the cursor, without the need to move the cursor to the
beginning of the text you want to indent."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (cemacs-forward-whitespace)
  (let ((nexttab (indent-next-tab-stop (current-column))))
    (indent-to nexttab))
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
