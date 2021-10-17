;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;; Hack to speed up package loading time
(setq gc-cons-threshold 64000000
      ;; package-enable-at-startup nil
      )
;; Extra Load Paths
(add-to-list 'load-path '"~/.emacs.d/etc/")
;; Macros
(defmacro WITH_SYSTEM(type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body)
  )
(defmacro WHEN_WINDOWS (&rest body)
  "Evalute BODY when the OS is Windows."
  (declare (indent defun))
  `(when (eq system-type 'windows-nt)
     ,@body)
  )
(defmacro WHEN_LINUX (&rest body)
  "Evalute BODY when the OS is Linux."
  (declare (indent defun))
  (when (eq system-type 'gnu/linux)
    ,@body)
  )
;; Enable Package Repositories
(require 'package)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
;; Melpa
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
;; Custom Functions
(add-to-list 'load-path (concat user-emacs-directory "src"))
(require 'cemacs-utility)
(require 'natural)
;; Configuration
(defun cemacs-cc-mode()
  "Hook function for cc derived modes."
  (interactive)
  (setq-default c-basic-offset 4
                c-electric-flag nil
                )
  (setq tab-width 4
        c-electric-flag nil     ; Disable problematic electric
        ))
(add-hook 'c-mode-common-hook 'cemacs-cc-mode)
(defun cemacs-cpp-mode()
  "Hook function for `c++-mode'."
  (interactive)
  (c-add-style  "stroustrup"
                '((c-basic-offset . 4)
                  (c-comment-only-line-offset . 0)
                  (c-offsets-alist
                   (statement-block-intro . +)
                   (substatement-open . 0)
                   (substatement-label . 0)
                   (label . 0)
                   (brace-list-open . 0)
                   (brace-list-intro
                    first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                   (statement-cont . +)
                   (inline-open . 0)))
                :use-style)
  )
(add-hook 'c++-mode-hook 'cemacs-cpp-mode)
(defun cemacs-c-mode()
  "Hook function for `c-mode'."
  (interactive)
  )
(add-hook 'c-mode-hook 'cemacs-c-mode)
(defun cemacs-elisp-mode()
  "Hook function for `emacs-lisp-mode'."
  (interactive)
  )
(add-hook 'emacs-lisp-mode 'cemacs-elisp-mode)
(defun cemacs-prog-mode()
  "Set up buffer for programming."
  ;; Disable auto-indent, it is inconsistent with incomplete code
  (setq electric-indent-mode nil
        )
  (display-line-numbers-mode)
  )
(add-hook 'prog-mode-hook 'cemacs-prog-mode)
(defun cemacs-markdown-mode()
  "Hook function for `markdown-mode'."
  (interactive)
  (flyspell-mode)
  )
(add-hook 'markdown-mode-hook 'cemacs-markdown-mode)
(defun cemacs-text-mode ()
  "Hook function for `text-mode'."
  (flyspell-mode)                       ; Spell checking
  (auto-fill-mode)                      ; column
  )
(defvar cemacs-init-setup-hook nil
  "A normal hook that runs at the end of init setup."
  )
;; Setup Functions
;; TODO this *supposed* to clean up deprecated files and put them in a trash
;; folder when the config fire replaces it with new ones
;; Although some particular folders like recentf and custom might benefit from
;; adopting the files instead of cleaning them
(defun cemacs-init-local-frame(frame)
  "Set the frame paramaters for FRAME."
  ;; (split-window-horizontally)
  (set-frame-parameter frame 'menu-bar-lines nil)
  (set-frame-parameter frame 'vertical-scroll-bars nil)
  (set-frame-parameter frame 'horizontal-scroll-bars nil)
  (set-frame-parameter frame 'tool-bar-lines nil)
  (set-frame-parameter frame 'width (x-display-pixel-width))
  (set-frame-parameter frame 'height (x-display-pixel-height))
  )
(add-hook 'after-make-frame-functions 'cemacs-init-local-frame)
(defun cemacs-configure-session-decorations()
  "Set the default frame paramaters and aethetics for the whole Emacs session.
Note this assumes that a frame does not already exist, for frame
configuration see `cemacs-init-local-frame'"
  (interactive)
  ;;Set Fonts
  (WITH_SYSTEM gnu/linux
    (add-to-list 'default-frame-alist
                 '(font . "MesloLGS NF-13:style=Regular")))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-13:style=Regular")))
  ;;Enable built-in modes
  (global-hl-line-mode)
  ;; Disable Window Decorations
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame)))
  (setq-default mode-line-format nil
                vertical-scroll-bar nil
                horizontal-scroll-bar nil
                tool-bar-mode nil
                menu-bar-mode nil
                )
  (fringe-mode (cons 0 0))
  ;; Visualize Whitespace
  (setq whitespace-style '(trailing tabs tab-mark))
  )
(defun cemacs-init-setup()
  "Run after-initilization setup."
  (interactive)
  ;;Misc Setup
  (delete-other-windows)
  (setq-default
   indent-tabs-mode nil
   tab-width 4
   c-basic-offset 4
   c-electric-flag nil         ; Disable useless and problematic electric
   parens-require-spaces nil   ; Don't insert space before parenthesis
   )
  (setq
   ;; Performance improvements
   inhibit-compacting-font-caches t
   jit-lock-chunk-size 6000
   jit-lock-defer-time 0                ; Defer fontification when pending input

   ;; Mode Setting
   indent-tabs-mode nil                 ; use spaces for indendation
   transient-mark-mode nil
   global-hl-line-mode t

   ;; Misc
   kill-whole-line t                    ; makes kill-line remove empty lines
   ;; Execute the 'compile-command' for 'project-compile by default
   ;; Can set a new command using the universal argument
   compilation-read-command nil
   compile-command nil                  ; Don't use 'make -k' by default
   )
  (delete-selection-mode t)             ; delete activated region on typing
  (global-subword-mode t)               ; treat delimited words seperately
  (global-whitespace-mode t)            ; visualize tab characters
  (global-visual-line-mode t)           ; make some commands to operate on visual lines
  ;; Backup
  (setq make-backup-files nil
        backup-by-copying t
        auto-save-default nil)
  (setq-default fill-column 80)         ; Change where auto-line wrap fill modes trigger
  ;; Save recentf on every file open
  (add-hook 'find-file-hook 'recentf-save-list)
  (fset 'yes-or-no-p 'y-or-n-p )    ; Make all yes or no prompts consistent
  (fset 'overwrite-mode 'ignore)    ; Disable pain in the arse insert mode
  ;; Re-enable disabled functions
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; Run Functions
  (cemacs-configure-session-decorations)
  (ignore-errors (cemacs-bind-vanilla-keys)) ; Protected from req-package error
  ;; Defer init setup hooks
  (run-at-time "1sec" nil 'run-hooks 'cemacs-init-setup-hook)
  (load cemacs-personal-config-file)
  )
;; Run early setup to prettify the session
(defun cemacs-early-init ()
  "An early setup function which must run be `cemacs-init-setup'."
  (interactive)
  ;; Variables
  (setq warning-minimum-log-level :warning ; Log warnings in a volatile buffer
        ;; Set early to prevent truncation of the recentf
        recentf-max-saved-items 1000
        )
  (cemacs-defdir 'cemacs-var-dir (concat user-emacs-directory "var/"))
  (cemacs-deffile 'cemacs-custom-file (concat cemacs-var-dir "custom.el")
                  'custom-file :local-only)
  (cemacs-defdir 'cemacs-recentf-save-file (concat cemacs-var-dir "recentf")
                 'recentf-save-file :local-only)
  ;; Just prettify the frame whilst waiting for loading
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame)))
  (load custom-file)
  )
(cemacs-early-init)
;; Req Package Setup
;; Attempt to install req-package if it's not alread loaded
(when (not (require 'req-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'req-package)
  )
(require 'req-package)
(setq use-package-always-ensure t       ; Automatically fetch packages
      )
;; Built in Packages
(req-package flyspell
  :after org
  :bind
  ;; Unbind Default keys
  (:map flyspell-mode-map
        ([(control ?\,)] .  nil)
        ([(control ?\.)] .  nil)
        ;; `org-return' is infuriating, so disable it until something better is found
        :map org-mode-map
        ("RET" .            nil))
  :config
  )
(req-package org
  :require
  ;; A cool new package which inserts links
  org-cliplink
  :hook
  (org-mode . cemacs-text-mode)
  :bind
  (("C-M-#" . cemacs-org-stamp-time)
   :map org-mode-map
   ("C-c C-x C-s" . cemacs-org-archive-and-done))
  :config
  (defvar cemacs-org-priority-list
    '(("* *" "top")
      ("ARCHIVE" "bottom")
      ))
  ;; Prevent newlines from seperating headings
  (setq org-cycle-separator-lines 0)
  (org-defkey org-mode-map (kbd "C-,") 'pop-to-mark-command)

  ;; Functions
  ;; NOTE(mallchad): Hardcoded section for personal setup, feel free to
  ;; change.
  (defun cemacs-org-load-files ()
    (interactive)
    (cl-loop for x-folder in
             '("~/org"
               "~/notes"
               "~/userdata/org"
               "~/userdata/notes")
             do (cemacs-open-files-in-directory x-folder)
             ))
  (defun cemacs-org-tagwise-comp-func (taglist-left taglist-right)
    "Decide which tag should go above, biasing ARCHIVE tags to the bottom."
    (interactive)
    (let ((less-than 'no-match)
          (comp-left (list ""))
          (comp-right (list ""))
          (less-than 'no-priority)
          (comp-left-tag-string "")
          (comp-right-tag-string "")
          (left-tag "")
          (right-tag ""))
      ;; Normalize tag strings
      (while (or (car taglist-left) (car taglist-right))
        (setq left-tag (or (pop taglist-left) ""))
        (setq right-tag (or (pop taglist-right) ""))
        (when (stringp left-tag)
          (push (downcase left-tag) comp-left))
        (when (stringp right-tag)
          (push (downcase right-tag) comp-right)))
      ;; This currently only pushes the ARCHIVE tag to the bottom
      ;; Whilst this does the job for general todo lists it is far from ideal
      ;; Here `less-than` actually counter-intuitively means the beginning of the
      ;; buffer, despite it being physically at the top, this is purely down to
      ;; character storing mechanics where in ASCII
      ;; `a == 0 + 97` and  `a == 0 + 122`
      ;; Which leads values closer to a being considered "lower" and being sorted
      ;; closer to the numerically lower buffer position 0, the very top left
      (cond ((member (downcase org-archive-tag) comp-left)
             (setq less-than nil)
             )
            ((member (downcase org-archive-tag) comp-right)
             (setq less-than t)
             )
            ;; Blindly comapre tags alphanumerically
            (:default
             (while (car comp-left)
               (setq comp-left-tag-string
                     (concat comp-left-tag-string (pop comp-left))))
             (while (car comp-right)
               (setq comp-right-tag-string
                     (concat comp-right-tag-string (pop comp-right))))
             (if (eq less-than 'no-priority)
                 (setq less-than (string-collate-lessp comp-left-tag-string
                                                       comp-right-tag-string))))
            )
      less-than)
    )
  (defun cemacs-org-sort-taglist-get ()
    "Get a list of tags for the heading under point."
    (interactive)
    (or (org-get-tags) (list ""))
    )
  (defun cemacs-org-sort-by-tag (&optional reverse)
    "Sort top level org headings by tag alphanumerically, grouping archive tags.

With REVERSE non-nil or with a prefix argument, reverse the sorting order.
This function makes a point of forcing the `org-archive-tag' towards the extreme
top or bottom of the file."
    (interactive "P")
    ;; Can't function by original point, get line and match it again
    (let ((position-along-original-line (- (point) (line-beginning-position)))
          (original-line-contents
           (buffer-substring (line-beginning-position) (line-end-position))))
      (mark-whole-buffer)
      (org-global-cycle 1)                ; Hide all subtrees
      (if reverse
          (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get
                            '(lambda (taglist-left taglist-right)
                               (not (cemacs-org-tagwise-comp-func taglist-left taglist-right))))
        ;; Else
        (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get 'cemacs-org-tagwise-comp-func))
      ;; Try return to the line the command was invoked on
      (search-forward original-line-contents)
      (goto-char (+ position-along-original-line (line-beginning-position))))
    )
  (defun cemacs-org-stamp-date ()
    (interactive)
    (call-interactively
     (org-time-stamp cemacs-universal-argument-double 'inactive))
    )
  (defun cemacs-org-stamp-time ()
    (interactive)
    (call-interactively
     (org-time-stamp cemacs-universal-argument-double 'inactive))
    )
  (defun cemacs-org-archive-and-done ()
    "Cycle the 'TODO' state and toggle the archive tag in one function."
    (interactive)
    (org-todo)
    (org-toggle-archive-tag)
    )
  (defun cemacs-org-folded-p ()
    "Returns non-nil if point is on a folded headline or plain list item."
    (interactive)
    (and (or (org-at-heading-p)
             (org-at-item-p))
         (invisible-p (point-at-eol)))
    )
  (defun cemacs-org-mode ()
    "Hook function for `org-mode'"
    (interactive)
    (org-indent-mode)
    (setq-local org-hide-leading-stars nil)
    )
  (add-hook 'org-mode-hook 'cemacs-org-mode)

  ;; Colour org checkboxes based on state
  ;; This is an absolutely disgusting hack I found online and it needs to go.
  (custom-set-faces '(org-checkbox ((t (:foreground nil :inherit org-todo)))))
  (defface org-checkbox-todo-text
    '((t (:inherit org-todo)))
    "Face for the text part of an unchecked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-todo-text prepend))
   'append)
  (defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
   'append)

  ;; Only cycle node under cursor
  (add-hook 'org-cycle-hook
            (lambda (state)
              (when (eq state 'children)
                (setq org-cycle-subtree-status 'subtree))))
  )
;; External Packages
(req-package async
  :config
  (async-bytecomp-package-mode t)
  )
(req-package color-theme-sanityinc-tomorrow
  :config
  (if (and (boundp 'cemacs-selected-sanityinc-theme)
           (symbol-value 'cemacs-selected-sanityinc-theme))
      (load-theme cemacs-selected-sanityinc-theme)
    (load-theme 'sanityinc-tomorrow-bright t))
  )
(req-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  :config
  (defvar c-aggressive-indent-line-end-prevention-distance 3
    "The distance in characters away from the end of the line
you should be before aggressively auto-indenting")
  (defun c-aggressive-indent-near-line-end ()
    (if (< (- (line-end-position) (point))
           c-aggressive-indent-line-end-prevention-distance)
        :dont-indent
      nil)
    )
  ;; Prevent aggressive indentation after some commands
  ;; This essentially makes the behaviour less "aggressive" and offers
  ;; the user some freedom to move the cursor, before reverting to normal
  ;; behaviour
  (cemacs-add-multiple-to-list
   'aggressive-indent-protected-commands
   'backward-delete-char
   'c-electric-backspace
   'cemacs-delete-word
   'backward-kill-word
   'natural-delete-word
   'natural-delete-word-backwards
   'natural-delete-whitespace
   'natural-one-space
   'delete-char
   'backward-delete-char
   'backward-delete-char
   'hungry-delete-forward
   'tab-to-tab-stop
   'just-one-space
   'delete-horizontal-space
   'natural-tab-to-tab-stop)
  (cemacs-add-multiple-to-list 'aggressive-indent-dont-indent-if
                               `(c-aggressive-indent-near-line-end))
  ;; Reduce the chance of impacting performance by increasing idle time
  (setq aggressive-indent-sit-for-time 0.2)
  )
(req-package all-the-icons
  :config
  (when (not (boundp 'cemacs-all-the-icons-fonts-installed))
    (all-the-icons-install-fonts 'skip)
    (customize-save-variable 'cemacs-all-the-icons-fonts-installed t))
  )
(req-package avy
  :require avy-zap
  :commands
  (avy-goto-char
   avy-pop-mark
   avy-zap-to-char-dwim)
  :bind
  (("C-r" . avy-goto-char)
   ("M-r" . avy-pop-mark)
   ("M-z" . avy-zap-to-char-dwim))
  :config
  (setq avy-highlight-first t
        avy-background t
        ;; Use 2 key combos instead of only single home row
        avy-style 'words
        avy-words nil
        )
  ;; Use all keys for avy instead of just home row
  (cl-loop for char-left from ?a to ?z do
           (cl-loop for char-right from ?a to ?z do
                    (add-to-list 'avy-words
                                 (concat (string char-left) (string char-right))
                                 :append)))
  ;; The colours picked here are designed to maximize readabiltiy
  ;;
  ;; Glaring/agressive and eye-catching are avoided to prevent distraction.
  ;; This makes dark and desaturated colours at the best fit for this, with
  ;; the characters being bright and high contrast, since the character is the
  ;; part meant to be read.
  (set-face-attribute 'avy-lead-face nil :background '"dark red" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "#1d1d62" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "#2a3418" :foreground "white")
  )
(req-package backup-each-save
  :commands
  (backup-each-save)

  :hook
  (after-save . backup-each-save)
  (cemacs-kill-volatile-buffer-pre . backup-each-save)
  (cemacs-init-setup . c-backup-recentf)
  :config
  ;; TODO Stop being lazy and turn this is into a custom function
  ;; Deal with problematic windows drive path rules
  (WHEN_WINDOWS
    (defun backup-each-save-compute-location (filename)
      (let* ((containing-dir
              (replace-regexp-in-string ".:/"
                                        "drive/"         ; Strip colon from drive path
                                        (file-name-directory filename)))
             (basename (file-name-nondirectory filename))
             (backup-container
              (format "%s/%s"
                      backup-each-save-mirror-location
                      containing-dir)))
        (when (not (file-exists-p backup-container))
          (make-directory backup-container t))
        (format "%s/%s-%s" backup-container basename
                (format-time-string backup-each-save-time-format)))
      ))

  (defun backup-each-save ()
    (interactive)
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
        (let ((bfn (buffer-file-name)))
          (when (and (or backup-each-save-remote-files
                         (not (file-remote-p bfn)))
                     (funcall backup-each-save-filter-function bfn)
                     (or (not backup-each-save-size-limit)
                         (<= (buffer-size) backup-each-save-size-limit)))
            (copy-file bfn (backup-each-save-compute-location bfn) t t t))))
    )

  (defun c-backup-recentf ()
    (let* ((recentf-path (file-truename recentf-save-file))
           (recentf-backup-path (backup-each-save-compute-location recentf-path)))
      (file-truename recentf-backup-path)
      (copy-file recentf-path recentf-backup-path
                 :overwrite :perserve-time :preserve-id :preserve-permission))
    )
  )
(req-package beacon
  :hook
  (cemacs-init-setup . beacon-mode)
  (beacon-before-blink . cemacs-beacon-truncate-to-line)
  :config
  (setq cemacs-beacon-size 40
        cemacs-beacon-size-padding 10
        beacon-size cemacs-beacon-size
        beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1    ;; blink if the line changes
        beacon-blink-when-point-moves-horizontally 20
        ;; Don't push the mark when the cursor moves a long distance
        beacon-push-mark nil
        )
  ;; FIX: A placeholder fix to prevent beacon from pushng the text around near
  ;; the end of lines
  (defun cemacs-beacon-truncate-to-line ()
    (let* ((point-along-line (- (point) (line-beginning-position)))
           (buffer-width (window-width))
           (point-window-end-distance (abs (- buffer-width point-along-line)))
           (end-distance-padded (- point-window-end-distance
                                   cemacs-beacon-size-padding)))
      (setq beacon-size end-distance-padded)
      (when (> end-distance-padded cemacs-beacon-size)
        (setq beacon-size cemacs-beacon-size))
      (when (< end-distance-padded 0)
        ;; Oops, just set it to 0 and move on
        (setq beacon-size 0)
        (message "Failed to calculate a non-intrusive beacon-size, Refusing to blink"))
      ))
  )
(req-package bind-key
  :config
  (defun cemacs-bind-vanilla-keys()
    "Set up personal keybinds after initilization."
    (interactive)
    ;; Emacs Control Bindings
    ;; Here we try to keep as many standard editor bindings as possible, to
    ;; make it less jarring if a non-emacs user needs to use it for any reason.
    ;;
    ;; Equally, we try to keep as many vanilla emacs bindings as possible,
    ;; to make unmodified distributions more accessible.
    (bind-keys
     ;; Navigation
     ("C-e" .           natural-end-of-line)
     ("<home>" .        natural-beginning-of-line)
     ("<end>" .         natural-end-of-line)
     ("M-b" .           natural-backward-word)
     ("M-f" .           natural-forward-word)
     ("<C-left>" .      natural-backward-word)
     ("<C-right>" .     natural-forward-word)
     ("C-," .           pop-to-mark-command)

     ;; Editing Commands
     ("<C-backspace>" . natural-delete-word-backwards)
     ("M-d" .           natural-delete-word)
     ("<C-delete>" .    natural-delete-word)
     ("M-\\" .          natural-delete-whitespace)
     ("M-SPC" .         natural-one-space)
     ("C-x r" .         revert-buffer)
     ("M-i" .           natural-tab-to-tab-stop)
     ("M-p" .           kill-whole-line)
     ("C-#" .           cemacs-activate-mark)

     ;; Other
     ("C-x r" .         cemacs-revert-buffer)
     ("C-x k" .         cemacs-buffer-kill-volatile)
     ("M-o" .           ff-find-other-file)
     ("C-x e" .         cemacs-find-user-init-file)
     ("C-x C-#" .        server-edit-abort)

     ;; Unbind Keys
     ("<insert>" .      nil)         ; 'overwrite-mode
     ("<backspace>" .   nil)
     ("C-j" .           join-line)         ; 'electric-newline-and-maybe-indent'
     ;; Get rid of conflicting electric bindings
     :map c++-mode-map
     ("<insertchar>" .  nil)     ; 'overwrite-mode
     ("DEL" .           nil)
     ("C-d" .           nil)
     ("#"   .           nil)
     ("*"   .           nil)
     ("/"   .           nil)
     ("<"   .           nil)
     (">"   .           nil)
     ("("   .           nil)
     (")"   .           nil)
     ("{"   .           nil)
     ("}"   .           nil)
     (":"   .           nil)
     (";"   .           nil)
     (","   .           nil)
     )
    ))
;;; A robust, prettified calender framework
(req-package calfw
  :require calfw-org
  :commands
  (cfw:open-calendar-buffer)
  )
(req-package centaur-tabs
  :hook
  (cemacs-init-setup . centaur-tabs-mode)
  :bind
  (("<C-tab>" . centaur-tabs-forward)
   ("<C-S-tab>" . centaur-tabs-backward)
   ("<C-iso-lefttab>" . centaur-tabs-backward))
  :config
  ;;Misc Settings
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-close-button nil
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "bar"
        centaur-tabs-set-bar t
        centaur-tabs-bar 'over
        centaur-tabs-modified-marker "*"
        )
  ;;Create Uniform Tabbar Appearance
  (centaur-tabs-headline-match)
  ;;Group tabs by projects
  (centaur-tabs-group-by-projectile-project)
  )
(req-package company
  :hook
  (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("M-p" . company-select-previous)
        ("M-n" . company-select-next)
        ;; Make company less disruptive to general editing
        ("C-p" . nil)
        ("C-n" . nil)
        :map company-tng-map
        ("C-p" . nil)
        ("C-n" . nil))
  :config
  ;; Apply company-tng patch
  (company-tng-configure-default)
  (setq company-require-match 'never
        company-idle-delay 0.2      ;; Slow down popup so input blocking is reduced
        )
  )
(req-package crux
  :commands
  (crux-move-beginning-of-line
   crux-top-join-line
   crux-swap-windows
   crux-smart-open-line-above)
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ;; ("C-j" . crux-top-join-line)
   ("C-c C-o" . crux-swap-windows)
   ("C-o" . crux-smart-open-line-above))
  :config
  ;; Fix crux not honouring visual lines
  (setq crux-move-visually t)
  )
(req-package cmake-mode
  )
(req-package csharp-mode
  :require csproj-mode
  :hook
  (csharp-mode . cemacs-cc-mode)
  :config
  (defun cemacs-csharp-mode()
    (interactive)
    (setq tab-width 4)
    (setq-default c-basic-offset 4)
    (c-set-style "c#"))
  (add-hook 'csharp-mode-hook 'cemacs-csharp-mode)
  )
(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )
(req-package fireplace
  :commands
  (fireplace)
  :bind
  (("C-x 3" . cemacs-fireplace-split-window-right)
   ("C-x 2" . cemacs-fireplace-split-window-below))
  :config
  ;; Variable Config
  (setq fireplace-smoke-on t)
  ;; Paramter hack to allow the fireplace to resize properly
  (defun fireplace--update-locals-vars (&optional nil-window)
    "Update `fireplace' local variables."
    (setq fireplace--bkgd-height (- (floor (window-height (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--bkgd-width  (- (round (window-width (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5)))
          fireplace--flame-pos fireplace-flame-pos))
  (defun fireplace--disable-minor-modes ()
    "Disable minor modes that might affect rendering."
    (switch-to-buffer fireplace-buffer-name)
    ;; Use local variables to avoid messing with the actual editing enviornment
    (setq-local truncate-lines t
                cursor-type nil
                show-trailing-whitespace nil
                indicate-empty-lines nil
                transient-mark-mode nil
                hl-line-mode nil
                ;; global-hl-line mode overrides the local hl-line-mode
                ;; *for some reason* and it's still called global-hl-line-mode
                ;; *even though* you can set 'global-hl-line-mode' as a buffer-local.
                global-hl-line-mode nil
                ;; non-standard emacs packages
                beacon-mode nil
                )
    ;; Reference the fireplace buffer in-case the current buffer
    ;; isn't the fireplace, for some reason.
    (buffer-disable-undo fireplace-buffer-name)
    )
  (defun cemacs-fireplace-visit (&optional window)
    (interactive)
    (if (windowp window)
        (select-window window)
      (set 'window (selected-window)))
    (unless (get-buffer "*fireplace*")
      (fireplace))
    (set-window-buffer window (get-buffer "*fireplace*"))
    )
  (defun cemacs-fireplace-split-window-right ()
    (interactive)
    (cemacs-fireplace-visit (split-window-right))
    )
  (defun cemacs-fireplace-split-window-below ()
    (interactive)
    (cemacs-fireplace-visit (split-window-below))
    )
  (add-hook 'after-make-frame-functions 'cemacs-fireplace-visit)
  )
(req-package flycheck
  :require flycheck-inline
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)
  :config
  (setq flycheck-highlighting-mode nil)
  ;; Disable annoying documentation warnings which are too strict
  ;; instead, use 'M-x checkdoc'
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; Fix flycheck not being able to find files in the load path
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )
(req-package free-keys
  :commands
  (free-keys
   free-keys-set-prefix
   free-keys-change-buffer)
  )
(req-package god-mode
  :config
  )
(req-package helm
  :after
  (hydra)
  :require
  helm-flycheck
  helm-projectile
  helm-rg
  helm-swoop
  :commands
  (helm-M-x
   helm-find-files
   helm-buffers-list
   helm-execute-persistent-action
   helm-select-action
   helm-swoop-without-pre-input
   helm-mini
   helm-bookmarks
   helm-projectile
   helm-flycheck
   helm-rg
   helm-projectile-rg
   hydra-query/body)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-q" . hydra-query/body)
   ;; Swap Action and Completion Buttons
   :map helm-map
   ("TAB" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ;; unbind non-emacs conforming bindings
   ("C-w" .     nil)                    ; append word at point
   ("C-SPC" .   nil)                    ; confusing and not useful marking behavour
   ;; Use backward delete word instead of invoking some auto-expansion toggle
   :map helm-find-files-map ("<C-backspace>" . nil)
   :map helm-projectile-find-file-map ("<C-backspace>" . nil)
   )
  :init
  ;; Completley hide helm header
  (fset 'helm-display-mode-line #'ignore)
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))
  (add-hook 'helm-after-initialize-hook
            (defun hide-mode-line-in-helm-buffer ()
              "Hide mode line in `helm-buffer'."
              (with-helm-buffer
                (setq-local mode-line-format nil))
              ))
  :hook
  (cemacs-init-setup . helm-mode)
  :config
  (require 'helm-config)
  ;;Helm minibuffer config
  (setq helm-autoresize-mode t
        helm-display-header-line nil
        helm-header-line-space-before-prompt nil
        helm-autoresize-max-height 30   ;Always take up 30% the screen
        helm-autoresize-min-height 30
        helm-split-window-inside-p t    ;Shows helm window in current buffer
        helm-swoop-split-with-multiple-windows helm-split-window-inside-p
        helm-mode-line-string nil
        helm-use-frame-when-more-than-two-windows nil
        )
  ;; ;Helm minibuffer config
  ;;TODO(mallchad) Need to reduce the size of the space after each helm source
  ;; Don't use helm's own displaying mode line function
  (set-face-attribute 'helm-source-header nil
                      :height 1.1
                      :foreground "dark cyan"
                      )
  ;;Query
  (defhydra hydra-query (:color blue)
    "query for"
    ("s" helm-swoop-without-pre-input "String")
    ("m" helm-mini "Mini")
    ("b" helm-bookmarks "Bookmarks")
    ("f" helm-flycheck "Flycheck")
    ("r" helm-rg "Ripgrep")
    ;; Projectile and lsp specific
    ("p" projectile-find-file "Projectile")
    ("C-p" helm-projectile-rg "Projectile ripgrep")
    ("d" helm-lsp-diagnostics "LSP Diagnostics"))
  )
(req-package highlight-parentheses
  :hook
  (cemacs-init-setup . global-highlight-parentheses-mode)
  :config
  (setq hl-paren-background-colors '("gray")
        hl-paren-colors '("black")
        hl-paren-highlight-adjacent nil
        )
  )
(req-package hl-todo
  :config
  (global-hl-todo-mode)
  )
(req-package hungry-delete
  :config
  ;; (global-hungry-delete-mode)
  ;; The default hungry-delete behaviour deletes all whitespace backwards
  ;; This is annoying since the point of using it was to rmeove extraneous
  ;; whitespace, not to remove all of it
  ;; The new behaviour leaves 1 space for words so its more comfortable and
  ;; similar to other editors
  (setq hungry-delete-join-reluctantly t)
  )
(req-package hydra
  :bind
  ;; (("C-s" . hydra-slayer/body)
  ;; )
  :config
  (defhydra hydra-slayer (:color blue)
    "kill shortcuts"
    ("x" slay-function "function(x)" :color red)
    ("l" kill-whole-line "whole line" :color red)
    ("b" slay-whole-buffer "whole buffer"))
  ;; (defhydra hydra-emacs (:color blue :hint nil))
  )
(req-package lsp-mode
  :after
  company
  flycheck
  :require
  lsp-ui
  :hook
  (c++-mode . lsp)
  (c-mode . lsp)
  (csharp-mode . lsp)
  :bind
  (:map lsp-mode-map
        ("M-#" . lsp-ui-doc-show)
        ("M-o" . lsp-clangd-find-other-file)

        ;; Visual Studio Like Bindings
        ("<f12>" . lsp-find-definition))
  :config
  ;; Fixes
  (defalias 'yas-expand-snippet 'ignore)        ; Prevent company-capf from erroring
  ;; Configuration
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        ;; Disable auto-formatting which is conflusing and conflcits with indenters
        lsp-enable-on-type-formatting nil
        ;; Allow indexing in the background
        lsp-clients-clangd-args '("--header-insertion-decorators=0" "--background-index")
        )
  (setq
   ;; Mode the doc position to the top of the frame
   lsp-ui-doc-position 'top
   ;; Don't show docs on hover, instead use a command/binding
   lsp-ui-doc-enable nil
   ;; Disable lsp info heading
   lsp-headerline-breadcrumb-enable nil
   ;; Disable attention grabbing, screen moving, modeline signature documentation
   lsp-signature-render-documentation nil
   ;; Disable sideline, flycheck-inline is more readable and less intrusive
   lsp-ui-sideline-enable nil
   )
  ;; Performance Tuning
  (setq
   ;; Source for performance improvements https://emacs-lsp.github.io/lsp-mode/page/performance/
   ;; Adjust gc-cons-threshold. The default setting is too low for lsp-mode's
   ;; needs due to the fact that client/server communication generates
   ;; a lot of memory/garbage.
   gc-cons-threshold 100000000
   ;; Increase the amount of data which Emacs reads from the process.
   ;; Again the emacs default is too low 4k considering that the some of the
   ;; language server responses are in 800k - 3M range.
   read-process-output-max (* 1024 1024) ;; 1mb
   )
  )
(req-package lsp-ui
  :config
  )
(req-package magit
  :commands
  (magit-status
   magit-stage
   magit-stage-file
   magit-unstage-file)
  :config
  (setq magit-diff-refine-hunk 'all         ; Show in-hunk differences
        magit-save-repository-buffers nil   ; Don't try to save before commit
        )
  )
;; Allows for quick insertion of licenses from github
(req-package license-templates
  )
(req-package lua-mode
  :hook
  (lua-mode . lsp)
  :config
  )
(req-package multiple-cursors
  :after hydra
  :bind
  (("M-m" . mc/edit-lines)
   )
  :config
  ;; Mark defualt behaviour for commands

  (cemacs-add-multiple-to-list 'mc/cmds-to-run-for-all
                               #'cemacs-delete-word
                               #'cemacs-delete-word-backwards
                               #'natural-delete-word-backward
                               #'natural-delete-word-backwards
                               #'natural-forward-word
                               #'natural-beginning-of-line
                               #'natural-end-of-line
                               #'crux-move-beginning-of-line
                               #'hungry-delete-forward
                               #'indent-for-tab-command
                               #'kill-region
                               #'org-delete-char
                               #'org-self-insert-command
                               #'tab-to-tab-stop
                               )

  (cemacs-add-multiple-to-list 'mc/cmds-to-run-once
                               #'helm-M-x
                               #'helm-confirm-and-exit-minibuffer
                               )
  )
(req-package json-mode
  ;; A minor mode to aid with json editing
  )
(req-package omnisharp
  ;; :hook
  ;; (csharp-mode . omnisharp-mode)
  :commands
  (omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  )
(req-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  )
(req-package origami
  :require lsp-origami
  :commands
  (origami-mode)
  :config
  ;; TODO(mallchad) need to setup keybinds for this package
  )
(req-package flycheck-projectile)
(req-package projectile
  :demand t                             ; Always loaded at startup
  :force t
  :require
  flycheck-projectile
  :commands (projectile-mode)
  :hook
  (cemacs-init-setup . projectile-mode)
  (projectile-find-file . cemacs-projectile-project-hook)
  (after-revert . cemacs-projectile-project-hook) ; redo with buffer revert
  :bind
  (:map projectile-mode-map
        (("C-x p" . projectile-command-map)))
  :config
  (defvar cemacs-projectile-project-functions
    '(())
    )
  (defvar cemacs-projectile-project-locals '(())
    "Sets read-only config variables for a project.

This is for setting variables for other functions, and is not
meant to be used to store information for any amount of time.

The expected value is an associative list, where the car of
each con cells is the project in question, and the cdr is
a list of symbols and values.

For example
((unreal (tab-width 4)
         (indent-tabs-mode t)
(cemacs (fill-column 80))
)
"
    )
  (defvar cemacs-projectile-grouping '(())
    "This variable contains what groups projects are associated with.

The underlying system does not actually have a concept of what a group is,
a group is simply a generic 'project' designed to be used for other projects,
as oppose to on their own.

The expected value of the variable is an associative list,
 specificly using cons cells,
where, the expected car of each cons cell is the name of the project,
and the cdr is a list of groups to be 'bound' to the project.

For example
((UnrealTournament unreal)
(cemacs elisp)
)
"
    )
  (defvar c-projectile-group-detect-functions '())
  ;; TODO Detect projcet type base on uproj / function
  (defun cemacs-projectile-project-hook ()
    (let* ((current-project (projectile-project-name))
           (current-project-root (projectile-project-name))
           (current-project-function
            (cdr (assoc current-project
                        cemacs-projectile-project-functions)))
           (current-group-functions '())
           (current-project-groups (cdr (assoc current-project cemacs-projectile-grouping)))
           (current-project-locals (cadr (assoc
                                          current-project
                                          cemacs-projectile-project-locals))))
      (dolist (x-group current-project-groups)
        (cemacs-add-multiple-splicing 'current-group-functions
                                      (cdr (assoc x-group cemacs-projectile-project-functions))))
      (dolist (x-function current-group-functions)
        (funcall x-function))
      (dolist (x-local current-project-locals)
        (eval `(setq-local ,(car current-project-locals)
                           (cadr current-project-locals)))
        )))
  )
(req-package rainbow-blocks
  :commands
  (rainbow-blocks-mode)
  :config
  (set-face-attribute 'rainbow-blocks-depth-1-face nil :foreground "white")
  (set-face-attribute 'rainbow-blocks-depth-2-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-blocks-depth-3-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-blocks-depth-4-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-blocks-depth-5-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-blocks-depth-6-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-blocks-depth-7-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-blocks-depth-8-face nil :foreground "spring green")
  (set-face-attribute 'rainbow-blocks-depth-9-face nil :foreground "sienna1")
  )
(req-package rainbow-delimiters
  :commands
  (rainbow-delimiters-mode)
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "white")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "spring green")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "sienna1")
  )
(req-package rainbow-mode
  :commands
  (rainbow-mode)
  :hook
  (prog-mode . rainbow-mode)
  )
(req-package restart-emacs
  :commands
  (restart-emacs)
  :bind
  (("C-x M-a" . restart-emacs))
  :config
  )
(req-package smartparens
  :after aggressive-indent
  :commands
  (smartparens-global-mode
   smartparens-mode
   cemacs-smartparens-enforcer-mode
   cemacs-smartparens-global-enforcer-mode
   sp-kill-whole-line
   c-sp-natural-delete-word
   c-sp-natural-delete-word-backwards
   sp-kill-region)
  :hook
  (cemacs-init-setup . smartparens-global-mode)
  ;; (prog-mode . cemacs-smartparens-enforcer-mode)
  ;; Fix for *scratch* loading before this is hooked
  (lisp-interaction-mode . cemacs-smartparens-enforcer-mode)
  :bind
  (:map smartparens-mode-map
        ;; Pair management bindings which are required for strict-mode
        ("M-<backspace>" .              sp-backward-unwrap-sexp)
        ;; ("M-e" .                     sp-forward-slurp-sexp)
        ;; ("M-a" .                     sp-backward-slurp-sexp)
        ;; ("M-[" .                     sp-forward-barf-sexp)
        ;; ("M-]" .                     sp-backward-barf-sexp)

        ;; General Pair Manamagent and Navigation
        ("C-M-k" .                      sp-kill-sexp)       ; Allow killing by pair
        ("C-M-f" .                      sp-forward-sexp)
        ("C-M-b" .                      sp-backward-sexp)
        ("M-n" .                        sp-kill-hybrid-sexp))
  :init
  (defvar cemacs-smartparens-enforcer-mode-map nil
    "Keymap used for `cemacs-smartparens-enforcer-mode'.")
  :config
  ;; Defualt Configuration
  (require 'smartparens-config)

  ;; Keybinds
  ;; Enforce smartparens-strict-mode with keybinds
  (defvar cemacs-smartparens-enforcer-mode-map nil
    "Keymap used for `cemacs-smartparens-enforcer-mode'.")
  (setq cemacs-smartparens-enforcer-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap kill-word] 'sp-kill-word)
          (define-key map [remap kill-line] 'sp-kill-hybrid-sexp)
          (define-key map [remap backward-kill-word] 'sp-backward-kill-word)
          (define-key map [remap kill-region] 'sp-kill-region)
          (define-key map [remap delete-region] 'sp-delete-region)
          (define-key map [remap kill-whole-line] 'sp-kill-whole-line)
          (define-key map (kbd "<C-backspace>") #'c-sp-natural-delete-word-backwards)
          (define-key map (kbd "M-d") #'c-sp-natural-delete-word)
          map)
        )

  ;; Variables
  (setq-default sp-autoinsert-pair nil          ; More trouble than it's worth
                sp-autoskip-closing-pair nil
                )

  ;; smartparens Custom Adapted Logic
  (defun c-sp-natural-delete-word (&optional arg)
    "Modified version of `natural-delete-word' for smartparens"
    (interactive)
    (let ((original-point (point)))
      ;; Following two characters are whitespace/blank or end of line
      (cond ((= (line-end-position) (point))
             (cemacs-forward-whitespace :traverse-newlines)
             (sp-delete-region original-point (point)))
            ;; Following two characters are whitespace/blank
            ((and (string-match "[[:blank:]]" (string (char-after)))
                  (string-match "[[:blank:]]" (string
                                               (char-after (+ (point) 1)))))
             ;; traverse all whitespace upto line beginning
             (cemacs-forward-whitespace)
             (sp-delete-region original-point (point)))
            (:default (sp-kill-word 1)))
      (constrain-to-field nil (point)))
    )
  (defun c-sp-natural-delete-word-backwards ()
    "Modified version of `natural-delete-word-backwards' for smartparens"
    (interactive)
    (let ((original-point (point))
          (original-line (line-number-at-pos (point)))
          )
      ;; Delete whitespace hungrily if at line beginning across lines
      (cond ((= (line-beginning-position) (point))
             (cemacs-backward-whitespace :traverse-newlines)
             (sp-delete-region original-point (point))
             )
            ;; Previous two characters are whitespace/blank
            ((and (string-match "[[:blank:]]" (string (char-before)))
                  (string-match "[[:blank:]]" (string (char-before
                                                       (- (point) 1))
                                                      )))
             ;; traverse all whitespace upto line beginning
             (cemacs-backward-whitespace)
             (sp-delete-region original-point (point))
             )
            (:default
             (sp-backward-kill-word 1)
             (if (not (= original-line (line-number-at-pos (point))))
                 ;; Try not to move the cursor too far
                 (end-of-line))
             )
            )
      (constrain-to-field nil original-point)
      (point))
    )
  ;; Keybinds
  ;; Enforce smartparens-strict-mode with keybinds

  (cemacs-add-multiple-to-list 'aggressive-indent-protected-commands
                               #'c-sp-natural-delete-word
                               #'c-sp-natural-delete-word-backwards
                               #'sp-delete-char
                               #'sp-backward-delete-char
                               #'sp-kill-word
                               #'sp-backward-kill-word
                               #'sp-kill-symbol
                               #'sp-backward-kill-symbol
                               #'sp-delete-symbol
                               #'sp-backward-delete-symbol
                               #'sp-delete-region
                               #'delete-region)
  ;; Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)

  ;; Pair angled brackets in c-modes
  ;; (sp-local-pair sp-c-modes "<" ">")

  (define-minor-mode cemacs-smartparens-enforcer-mode
    "Toggle smartparens mode but enforce balancing of sexps more.

  This mode tries harder to balance pairs, but isn't as aggressive as
   smartparens-strict-mode.
  This is designed to help prevent delimiter based syntax errors
  This has the downside of being quite aggressive with interfering with editing,
  but, this is probably worth it, for the early 'alarm' for syntax errors.

  This mode, unlike `smartparens-strict-mode' does not affect all commands,
  particularly `delete-char' still can delete pairs as normal, so that can
  be used as an effective alternative to advanced sexp editing.
  The current content of
  `cemacs-smartparens-enforcer-mode-map' is:

   \\{cemacs-smartparens-enforcer-mode-map}"
    :keymap cemacs-smartparens-enforcer-mode-map
    )
  (define-globalized-minor-mode cemacs-smartparens-global-enforcer-mode
    cemacs-smartparens-enforcer-mode
    ignore
    )
  )
(req-package smooth-scrolling
  :hook
  (cemacs-init-setup . smooth-scrolling-mode)
  :config
  (setq scroll-conservatively 0)
  )
(req-package smart-yank
  :commands
  (smart-yank-mode
   smart-yank-pop
   )
  :config
  )
(req-package sublimity
  :commands
  (sublimity-mode)
  ;; :hook
  ;; (cemacs-init-setup . sublimity-mode)
  ;; :config
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  ;;(require 'sublimity-map) ;; experimental
  ;; (setq sublimity-scroll-weight 4
  ;; sublimity-scroll-drift-length 4
  ;;      sublimity-map-size 10
  ;;      sublimity-map-fraction 0.3
  ;;      sublimity-map-text-scale -7
  ;;      sublimity-map-set-delay 1
  ;; )
  )
(req-package undo-tree
  :bind
  (("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)
   ;; Unbind Included Keymaps
   :map undo-tree-map
   ("C-x r u".  nil)
   ("C-x r U".  nil)
   ("C-x r".  nil))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil  ; brings performance enhancement
        undo-tree-history-directory-alist backup-directory-alist
        )
  )
(req-package visible-mark
  ;; A minor mode to show you where the mark is current
  :commands
  (visible-mark-mode
   global-visible-mark-mode)
  :hook
  (cemacs-init-setup . global-visible-mark-mode)
  :config
  (set-face-attribute 'visible-mark-active nil
                      :background "#006F00" :foreground "white")
  )
(req-package vlf
  :config
  (require 'vlf-setup)
  )
(req-package visual-fill-column
  :config
  (setq visual-fill-column-width 80)
  )
(req-package volatile-highlights
  :config
  (volatile-highlights-mode)
  )
(req-package ws-butler
  ;; Unobtrusively clean up extrenuous whitespace and convert tabs to whitespace
  :config
  (ws-butler-global-mode)
  ;; Somebody though setting to 't' was a good idea, defeats the point and is
  ;; aggressively follows 'indent-tabs-mode'.
  (setq ws-butler-convert-leading-tabs-or-spaces nil
        )
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq-local ws-butler-mode nil)))
  )
(req-package which-key
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 2          ; Don't get in the way of normal usage
        which-key-sort-order 'which-key-description-order
        )
  )
(req-package yasnippet
  :require
  auto-yasnippet
  )
(req-package auto-yasnippet
  :config
  )
(req-package zoom
  ;; A window rebalancing minor mode
  :config
  (zoom-mode)
  ;; Tweak tweak rebalancing ratios
  (setq zoom-size '(70 . 30))
  )

;; Finalize Initialization
;; Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
(cemacs-init-setup)
(provide 'init)
;;; init.el ends here
