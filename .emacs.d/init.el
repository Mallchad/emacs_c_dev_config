;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrent whatsoever.

;;; Commentary:
;; Extra Load Paths
(add-to-list 'load-path '"~/.emacs.d/etc/")
;;; Code:
;; Macros
(defmacro WITH_SYSTEM(type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))
(defmacro WHEN_WINDOWS (&rest body)
  "Evalute BODY when the OS is Windows."
  (declare (indent defun))
  (when (eq system-type 'windows-nt)
    (body)
    )
  )
(defmacro WHEN_LINUX (&rest body)
  "Evalute BODY when the OS is Linux."
  (declare (indent defun))
  (when (eq system-type 'gnu/linux)
    `(eval ,@body)
    )
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
;; Configuration
(defun cemacs-cc-mode()
  "Hook function for cc derived modes."
  (interactive)
  (setq tab-width 4)
  (setq-default c-basic-offset 4)
  )
(add-hook 'c-mode-common-hook 'cemacs-cc-mode)
(defun cemacs-cpp-mode()
  "Hook function for `c++-mode'."
  (interactive)
  (c-set-style "stroustrup")
  )
(add-hook 'c-mode-common-hook 'cemacs-cpp-mode)
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
  (setq
   electric-indent-mode nil)
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
                 '(font . "MesloLGS NF-13:style=Regular")
                 ))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-13:style=Regular")
                 ))
  ;;Enable built-in modes
  (global-hl-line-mode)
  ;; Disable Window Decorations
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame))
    )
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
(defun cemacs-vanilla-keys-configure()
  "Set up personal keybinds after initilization."
  (interactive)
  ;; Emacs Control Bindings
  ;; Here we try to keep as many standard editor bindings as possible, to
  ;; make it less jarring if a non-emacs user needs to use it for any reason
  ;; Navigation
  (global-set-key (kbd "C-e") #'cemacs-natural-end-of-line)
  (global-set-key (kbd "<home>") #'cemacs-natural-beginning-of-line)
  (global-set-key (kbd "<end>") #'cemacs-natural-end-of-line)
  (global-set-key (kbd "M-b") #'cemacs-natural-backward-word)
  (global-set-key (kbd "M-f") #'cemacs-natural-forward-word)
  (global-set-key (kbd "<C-left>") #'cemacs-natural-backward-word)
  (global-set-key (kbd "<C-right>") #'cemacs-natural-forward-word)
  (global-set-key (kbd "C-,") #'pop-to-mark-command)
  ;; Editing Commands
  (global-set-key (kbd "<C-backspace>") #'cemacs-natural-delete-word-backwards)
  (global-set-key (kbd "M-d") #'cemacs-natural-delete-word)
  (global-set-key (kbd "<C-delete>") #'cemacs-natural-delete-word)
  (global-set-key (kbd "M-\\") #'cemacs-natural-delete-whitespace)
  (global-set-key (kbd "M-SPC") #'cemacs-natural-one-space)
  (global-set-key (kbd "C-x r") #'revert-buffer)
  ;; Other
  (global-set-key (kbd "C-x k") #'cemacs-buffer-kill-volatile)
  (global-set-key (kbd "M-o") #'ff-find-other-file)
  (global-set-key (kbd "C-x e") 'cemacs-find-user-init-file)
  ;; Unbind Keys
  (unbind-key (kbd "<insert>"))         ; 'overwrite-mode
  (unbind-key (kbd "<insertchar>"))     ; 'overwrite-mode
  )
(defun cemacs-init-setup()
  "Run after-initilization setup."
  (interactive)
  ;;Misc Setup
  (delete-other-windows)
  (setq-default indent-tabs-mode nil
                )
  (setq
   ;; Performance improvements
   inhibit-compacting-font-caches t
   jit-lock-chunk-size 6000
   ;; Mode Setting
   indent-tabs-mode nil               ;use spaces for indendation
   transient-mark-mode nil
   global-hl-line-mode t
   )
  (delete-selection-mode t)             ; delete activated region on typing
  (global-subword-mode t)               ; treat delimited words seperately
  (global-whitespace-mode t)            ; visualize tab characters
  (global-visual-line-mode t)           ; make some commands to operate on visual lines
  ;; Backup
  (setq make-backup-files nil
        backup-by-copying t
        auto-save-default nil)
  (setq-default fill-column 80        ; Change where auto-line wrap fill modes trigger
                )
  ;; Save recentf on every file open
  (add-hook 'find-file-hook 'recentf-save-list)
  (fset 'yes-or-no-p 'y-or-n-p ) ; Make all yes or no prompts consistent
  (fset 'overwrite-mode 'ignore) ; Disable pain in the arse insert mode
  ;; Re-enable disabled functions
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; Run Functions
  (cemacs-configure-session-decorations)
  (cemacs-vanilla-keys-configure)
  (run-hooks 'cemacs-init-setup-hook)
  )
;; Run early setup to prettify the session
(defun cemacs-early-init ()
  "An early setup function which must run be `cemacs-init-setup'."
  (interactive)
  ;; Variables
  (setq warning-minimum-log-level :debug  ; Log warnings in a volatile buffer
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
      (cemacs-init-local-frame (selected-frame))
    )
  (load custom-file)
  )
(cemacs-early-init)
;;Req Package Setup
;; Attempt to install req-package if it's not alread loaded
(when (not (require 'req-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'req-package)
  )
(require 'req-package)
(setq use-package-always-ensure t)
;; Built in Packages
(req-package flyspell
  :config
  ;; Unbind Default keys
  (define-key flyspell-mode-map [(control ?\,)] nil)
  (define-key flyspell-mode-map [(control ?\.)] nil)
  )
(req-package org
  :require
  ;; A cool new package which inserts links
  org-cliplink
  :hook
  (org-mode . cemacs-text-mode)
  :config
  (defvar cemacs-org-priority-list
    '(("* *" "top")
      ("ARCHIVE" "bottom")
      ))
  (org-defkey org-mode-map (kbd "C-,") 'pop-to-mark-command)
  ;; `org-return' is infuriating, so disable it until something better is found
  (define-key org-mode-map (kbd "RET") 'nil)
  ;; Functions
  ;; NOTE(mallchad): Hardcoded section for personal setup, feel free to
  ;; change.
  (defun cemacs-org-load-files ()
    (interactive)
    (cl-loop for x-folder in
             '("~/org"
               "~/notes"
               "~/userdata/org"
               "~/userdata/notes"
               )
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
          (right-tag "")
          )
      ;; Normalize tag strings
      (while (or (car taglist-left) (car taglist-right))
        (setq left-tag (or (pop taglist-left) ""))
        (setq right-tag (or (pop taglist-right) ""))
        (when (stringp left-tag)
          (push (downcase left-tag) comp-left)
          )
        (when (stringp right-tag)
          (push (downcase right-tag) comp-right)
          ))
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
                     (concat comp-left-tag-string (pop comp-left)
                             )))
             (while (car comp-right)
               (setq comp-right-tag-string
                     (concat comp-right-tag-string (pop comp-right)
                             )))
             (if (eq less-than 'no-priority)
                 (setq less-than
                       (string-collate-lessp comp-left-tag-string comp-right-tag-string))
               ))
            )
      less-than
      )
    )
  (defun cemacs-org-sort-taglist-get ()
    "Get a list of tags for the heading under point."
    (interactive)
    (or (org-get-tags) (list ""))
    )
  (defun cemacs-org-sort-by-tag ()
    "Sort top level org headings by tag alphanumerically, grouping archive tags."
    (interactive)
    ;; Can't function by original point, get line and match it again
    (let ((position-along-original-line (- (point) (line-beginning-position)))
          (original-line-contents
           (buffer-substring (line-beginning-position) (line-end-position)))
          )
      (mark-whole-buffer)
      (org-global-cycle 1)                ; Hide all subtrees
      (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get 'cemacs-org-tagwise-comp-func)
      ;; Try return to the line the command was invoked on
      (search-forward original-line-contents)
      (goto-char (+ position-along-original-line (line-beginning-position)))
      )
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
  ;; Bindings
  (global-set-key (kbd "C-M-#") #'cemacs-org-stamp-time)
  (define-key org-mode-map (kbd "C-c C-x C-s") #'cemacs-org-archive-and-done)
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
    (load-theme 'sanityinc-tomorrow-bright t)
    )
  )
(req-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  :config
  ;; Prevent aggressive indentation after some commands
  ;; This essentially makes the behaviour less "aggressive" and offers
  ;; the user some freedom to move the cursor, before reverting to normal
  ;; behaviour
  (cemacs-add-multiple-to-list 'aggressive-indent-protected-commands 'cemacs-delete-word
                               'backward-kill-word
                               'cemacs-natural-delete-word
                               'cemacs-natural-delete-word-backwards
                               'cemacs-natural-delete-whitespace
                               'cemacs-natural-one-space
                               'delete-char
                               'backward-delete-char
                               'backward-delete-char
                               'hungry-delete-forward
                               'tab-to-tab-stop
                               'just-one-space
                               'delete-horizontal-space
                               )
  )
(req-package all-the-icons
  :require async
  :config
  (when (not (boundp 'cemacs-all-the-icons-fonts-installed))
    (all-the-icons-install-fonts 'skip)
    (customize-save-variable 'cemacs-all-the-icons-fonts-installed t))
  )
(req-package avy
  :require avy-zap
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char)
  (global-set-key (kbd "M-r") 'avy-pop-mark)
  (global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
  (setq avy-highlight-first t
        avy-background t
        )
  ;; The colours picked here are designed to maximize readabiltiy
  ;;
  ;; This is achieved by reducing 'glare' or 'eye-catching' colours
  ;; of the background by using dark and slightly desatured colours
  ;; The resulting effect is that the actual character part is the
  ;; most attenting grabbing part, which is exactly the pint of avy -
  ;; going where you're looking
  ;;
  ;; However rather annoyingly the text / boxes are spaced closed together,
  ;; making them bleed into each other
  ;; This is likely impossible to fix in GNU emacs fonts aside
  ;; Increasing font size works. But is not optimal
  (set-face-attribute 'avy-lead-face nil :background '"dark red" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "#1d1d62" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "#2a3418" :foreground "white")
  )
(req-package backup-each-save
  :config
  ;; TODO Stop being lazy and turn this is into a custom function
  (defun backup-each-save ()
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)
                            ))
        (let ((bfn (buffer-file-name)))
          (when (and (or backup-each-save-remote-files
                         (not (file-remote-p bfn)))
                     (funcall backup-each-save-filter-function bfn)
                     (or (not backup-each-save-size-limit)
                         (<= (buffer-size) backup-each-save-size-limit)))
            (copy-file bfn (backup-each-save-compute-location bfn) t t t)))))
  :hook
  (after-save . backup-each-save)
  (cemacs-kill-volatile-buffer-pre . backup-each-save)
  )
(req-package beacon
  :hook
  (cemacs-init-setup . beacon-mode)
  :config
  (setq beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1    ;; blink if the line changes
        beacon-blink-when-point-moves-horizontally 20
        beacon-size 40
        ;; Don't push the mark when the cursor moves a long distance
        beacon-push-mark nil
        )
  )
;;; A robust, prettified calender framework
(req-package calfw
  :require calfw-org
  )
(req-package centaur-tabs
  :hook
  (cemacs-init-setup . centaur-tabs-mode)
  :bind
  ("<C-tab>" . centaur-tabs-forward)
  ("<C-S-tab>" . centaur-tabs-backward)
  ("<C-iso-lefttab>" . centaur-tabs-backward)
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
  :config
  ;; Apply company-tng patch
  (company-tng-configure-default)
  (setq company-require-match 'never
        company-idle-delay 0
        )
  (define-key company-tng-map (kbd "C-p") nil)
  (define-key company-tng-map (kbd "C-n") nil)
  (WHEN_WINDOWS
    ;; Slow down company-idle-delay so it doesn't choke emacs
    (setq company-idle-delay 0.02)
    )
  )
(req-package crux
  :config
  ;; Fix crux not honouring visual lines
  (setq crux-move-visually t
        )
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-j") 'crux-top-join-line)
  (global-set-key (kbd "C-x C-o") 'crux-swap-windows)
  (global-set-key (kbd "C-o") 'crux-smart-open-line-above)
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
    (c-set-style "c#")
    )
  (add-hook 'csharp-mode-hook 'cemacs-csharp-mode)
  )
(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )
(req-package fireplace
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
      (set 'window (selected-window))
      )
    (unless (get-buffer "*fireplace*")
      (fireplace)
      )
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
  (global-set-key (kbd "C-x 3") 'cemacs-fireplace-split-window-right)
  (global-set-key (kbd "C-x 2") 'cemacs-fireplace-split-window-below)
  (add-hook 'after-make-frame-functions 'cemacs-fireplace-visit)
  )
(req-package flycheck
  :require flycheck-inline
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)
  :config
  ;; Disable annoying documentation warnings which are too strict
  ;; instead, use 'M-x checkdoc'
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; Fix flycheck not being able to find files in the load path
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )
(req-package free-keys
  )
(req-package god-mode
  :config
  )
(req-package helm
  :after hydra
  :require
  helm-flycheck
  helm-projectile
  helm-rg
  helm-swoop
  :init
  ;; Completley hide helm header
  (fset 'helm-display-mode-line #'ignore)
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil)
    )
  (add-hook 'helm-after-initialize-hook
            (defun hide-mode-line-in-helm-buffer ()
              "Hide mode line in `helm-buffer'."
              (with-helm-buffer
                (setq-local mode-line-format nil)
                )
              )
            )
  :config
  (require 'helm-config)
  (helm-mode)
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
  ;; Keybinds
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  ;; Swap Action and Completion Buttons
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  ;; TODO(mallchad) need to find more elegant keybinds
  ;;Query
  (defhydra hydra-query (:color blue)
    "query for"
    ("s" helm-swoop-without-pre-input "String")
    ("m" helm-mini "Mini")
    ("b" helm-bookmarks "Bookmarks")
    ("p" helm-projectile "Projectile")
    ("f" helm-flycheck "Flycheck")
    ("r" helm-rg "Ripgrep")
    ("C-p" helm-projectile-rg "Projectile rg")
    )
  (global-set-key (kbd "C-q") 'hydra-query/body)
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
  :config
  (defhydra hydra-slayer (:color blue)
    "kill shortcuts"
    ("x" slay-function "function(x)" :color red)
    ("l" kill-whole-line "whole line" :color red)
    ("b" slay-whole-buffer "whole buffer")
    )
  (global-set-key (kbd "C-s") 'hydra-slayer/body)
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
  :config
  ;; Fixes
  (defalias 'yas-expand-snippet 'ignore)        ; Prevent company-capf from erroring
  ;; Keybindings
  (define-key lsp-mode-map (kbd "M-#") 'lsp-ui-doc-show)
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
  (define-key lsp-mode-map (kbd "M-o") #'lsp-clangd-find-other-file)
  )
(req-package magit
  :config
  )
(req-package lua-mode
  :hook
  (lua-mode . lsp)
  :config
  )
(req-package multiple-cursors
  :after hydra
  :config
  (global-set-key (kbd "M-m") 'mc/edit-lines)
  ;; (defhydra hydra-multicurses (global-map "C-m" :color red)
  ;;   "Multiple Cursors"
  ;;   ("q" hydra--body-exit "quit")
  ;;   ("e" mc-edit-lines "edit lines")
  ;;   ("a" mc/mark-all-like-this "mark all")
  ;;   ("r" mc/mark-all-in-region "mark in region")
  ;;   )
  )
(req-package json-mode
  ;; A minor mode to aid with json editing
  )
(req-package omnisharp
  ;; :hook
  ;; (csharp-mode . omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  )
(req-package org-noter
  :config
  )
(req-package org-super-agenda
  :config
  )
(req-package origami
  :require lsp-origami
  :config
  ;; TODO(mallchad) need to setup keybinds for this package
  )
(req-package projectile
  :require
  flycheck-projectile
  :hook
  (cemacs-init-setup . projectile-mode)
  :config
  )
(req-package rainbow-blocks
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
  :hook
  (prog-mode . rainbow-mode)
  )
(req-package restart-emacs
  :config
  (global-set-key (kbd "C-x M-a") 'restart-emacs)
  )
(req-package smartparens
  :after aggressive-indent
  :config
  ;; Defualt Configuration
  (require 'smartparens-config) 
  ;; smartparens Custom Adapted Logic
  (defun cemacs-sp-natural-delete-word (&optional arg)
    "Modified version of `cemacs-natural-delete-word' for smartparens"
    (interactive)
    (let ((original-point (point)))
      ;; Following two characters are whitespace/blank or end of line
      (cond ((= (line-end-position) (point))
             (cemacs-forward-whitespace :traverse-newlines)
             (sp-delete-region original-point (point))
             )
            ;; Following two characters are whitespace/blank
            ((and (string-match "[[:blank:]]" (string (char-after)))
                  (string-match "[[:blank:]]" (string (char-after
                                                       (+ (point) 1))
                                                      )))
             ;; traverse all whitespace upto line beginning
             (cemacs-forward-whitespace)
             (sp-delete-region original-point (point))
             )
            (:default (sp-kill-word 1)
                      )
            )
      (constrain-to-field nil (point))
      )
    )
  (defun cemacs-sp-natural-delete-word-backwards ()
    "Modified version of `cemacs-natural-delete-word-backwards' for smartparens"
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
                 (end-of-line)
               ))
            )
      (constrain-to-field nil original-point)
      (point)
      )
    )
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
          (define-key map (kbd "<C-backspace>") 'cemacs-sp-natural-delete-word-backwards)
          (define-key map (kbd "M-d") 'cemacs-sp-natural-delete-word)
          map
          ))
  (cemacs-add-multiple-to-list 'aggressive-indent-protected-commands
                               'cemacs-sp-natural-delete-word
                               'cemacs-sp-natural-delete-word-backwards
                               'sp-delete-char
                               'sp-backward-delete-char
                               'sp-kill-word
                               'sp-backward-kill-word
                               'sp-kill-symbol
                               'sp-backward-kill-symbol
                               'sp-delete-symbol
                               'sp-backward-delete-symbol
                               'sp-delete-region
                               'delete-region
                               )
  ;; Pair management bindings which are required for strict-mode
  (define-key smartparens-mode-map (kbd "S-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-e") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-a") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-[") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-]") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp) ; Allow killing by pair

  ;; Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)

  ;; Pair angled brackets in c-modes
  (sp-local-pair sp-c-modes "<" ">")
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
  (eval-after-load 'init (smartparens-global-mode))
  (add-hook 'prog-mode-hook 'cemacs-smartparens-enforcer-mode)
  ;; Fix for *scratch* loading before this is hooked
  (add-hook 'lisp-interaction-mode-hook 'cemacs-smartparens-enforcer-mode)
  )
(req-package smooth-scrolling
  :hook
  (cemacs-init-setup . smooth-scrolling-mode)
  :config
  (setq scroll-conservatively 0)
  )
(req-package smart-yank
  :config
  )
(req-package sublimity
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
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  (setq undo-tree-enable-undo-in-region nil  ; brings performance enhancement
        undo-tree-history-directory-alist backup-directory-alist
        )
  ;; Unbind Included Keymaps
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  (define-key undo-tree-map (kbd "C-x r") nil)
  )
(req-package visible-mark
  ;; A minor mode to show you where the mark is current
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
  (setq ws-butler-convert-leading-tabs-or-spaces t)
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
  )
(req-package zoom
  ;; A window rebalancing minor mode
  :config
  (zoom-mode)
  ;; Tweak tweak rebalancing ratios
  (setq zoom-size '(70 . 30))
  )
;;Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
(cemacs-init-setup)
(provide 'init)
;;; init.el ends here
