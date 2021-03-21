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
;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "/
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
               (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))
  )
(package-initialize)
;; Constants
(defconst cemacs-universal-argument-double  '(16)
  "A constant that represents the value an argument is \
passed when a single universal argument is called.

The value essentially a list with the single value of 4"
  )
(defconst cemacs-universal-argument-double  '(16)
  "A constant that represents the value an argument is \
passed when two universal arguments are called.

The value essentially a list with the single value of 16"
  )
;; Custom Functions
(defun slay-function()
  "Kill the function surrounding the point.
Emacs' built in 'mark-defun' is used so that is what determines what is
aconsidered a function,
This function is effectively a shorthand of 'mark-defun' 'kill-region'."
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun slay-whole-buffer()
  "Kill the buffer in it's entirety."
  (interactive)
  (kill-region (point-min) (point-max))
  (kill-region (region-beginning) (region-end))
  )
(defun cemacs-scroll-up-in-place()
  "Scroll buffer up 1 line without moving cursor position vertically."
  ;; TODO(mallchad) this could accept an arg quite easilly
  ;; TODO(mallchad) function feels like it skips 1 line up or down
  ;; occasionally
  (interactive)
  (forward-line -1)
  (scroll-down-command 1)
  )
(defun cemacs-scroll-down-in-place()
  "Scroll buffer down 1 line without moving cursor position vertically.

This is a reverse version of 'cemacs-scroll-up-in-place"
  ;; TODO(mallchad) this could easilly be made mirror it's counterpart
  (interactive)
  (forward-line 1)
  (scroll-down-command -1)
  )
(defun cemacs-delete-word(mult)
  "Delete characters forward until encountering the end of a word.
With argument MULT, repeat this that many times, or perform deletion backwards
if negative.

This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word mult)
     (point))))
(defun cemacs-delete-word-backwards(mult)
  "Delete characters backward until encountering the beginning of a word.
With argument MULT, repeat this many times.

This command is a reverse of cemacs-delete-word"
  (interactive "p")
  (cemacs-delete-word (- mult))
  )
(defun cemacs-find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file)
  )
(defvar cemacs-kill-volatile-buffer-pre-hook nil)
(defvar cemacs-kill-volatile-buffer-post-hook nil)
(defun cemacs-kill-volatile-buffer()
  "Kill the current buffer unconditionally."
  (interactive)
  (run-hooks 'cemacs-kill-volatile-buffer-pre-hook)
  (kill-buffer (current-buffer))
  (run-hooks 'cemacs-kill-volatile-buffer-post-hook)
  )
;; Configuration
(defun cemacs-cc-mode()
  (interactive)
  (setq tab-width 4)
  (setq-default c-basic-offset 4)
  )
(add-hook 'c-mode-common-hook 'cemacs-cc-mode)
(defun cemacs-cpp-mode()
  (interactive)
  (c-set-style "stroustrup")
  )
(add-hook 'c-mode-common-hook 'cemacs-cpp-mode)
(defun cemacs-c-mode()
  (interactive)
  )
(add-hook 'c-mode-hook 'cemacs-c-mode)
(defun cemacs-elisp-mode()
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
  (interactive)
  (flyspell-mode)
  )
(add-hook 'markdown-mode-hook 'cemacs-markdown-mode)
(defun cemacs-org-mode()
  (interactive)
  (org-indent-mode)
  (flyspell-mode)
  (setq-local org-hide-leading-stars nil)
  (setq-local ws-butler-mode nil)
  )
(add-hook 'org-mode-hook 'cemacs-org-mode)
(defvar cemacs-custom-variables-dir "var/custom.el")
(defvar cemacs-init-setup-hook nil
  ;;A normal hook that runs at the end of init setup
  )
(defun cemacs-configure-local-frame(frame)
  "Set the frame paramaters for FRAME."
  ;; (split-window-horizontally)
  (set-frame-parameter frame 'menu-bar-lines nil)
  (set-frame-parameter frame 'vertical-scroll-bars nil)
  (set-frame-parameter frame 'horizontal-scroll-bars nil)
  (set-frame-parameter frame 'tool-bar-lines nil)
  (set-frame-parameter frame 'width (x-display-pixel-width))
  (set-frame-parameter frame 'height (x-display-pixel-height))
  )
(add-hook 'after-make-frame-functions 'cemacs-configure-local-frame)
(defun cemacs-configure-session-decorations()
  "Set the default frame paramaters and aethetics for the whole Emacs session.
Note this assumes that a frame does not already exist, for frame
configuration see cemacs-configure-local-frame"
  (interactive)
  ;;Set Fonts
  (WITH_SYSTEM gnu/linux
    (add-to-list 'default-frame-alist
                 '(font . "Hack-13:style=Regular")
                 ))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-13:style=Regular")
                 ))
  ;;Enable built-in modes
  (global-hl-line-mode)
  ;; Disable Window Decorations
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-configure-local-frame (selected-frame))
    )
  (setq-default mode-line-format nil
                vertical-scroll-bar nil
                horizontal-scroll-bar nil
                tool-bar-mode nil
                menu-bar-mode nil
                )
  (fringe-mode (cons 0 0))
  (setq whitespace-style '(trailing tabs tab-mark))
  )
(defun cemacs-vanilla-keys-configure()
  "Set up personal keybinds after initilization."
  (interactive)
  ;; Emacs Control Bindings
  ;; Navigation
  (global-set-key (kbd "C-x r") 'revert-buffer)
  (global-set-key (kbd "M-p") 'cemacs-scroll-up-in-place)
  (global-set-key (kbd "M-n") 'cemacs-scroll-down-in-place)
  (global-set-key (kbd "<C-backspace>") 'cemacs-delete-word-backwards)
  (global-set-key (kbd "C-,") 'pop-to-mark-command)
  ;; Editing Commands
  (global-set-key (kbd "M-d") 'cemacs-delete-word)
  ;; (global-set-key (kbd "M-l") 'downcase-dwim)
  ;; (global-set-key (kbd "C-M-l") 'nil)
  ;; (global-set-key (kbd "M-c") 'capitalize-word)
  ;; (global-set-key (kbd "C-M-c") 'upcase-char)
  ;; Other
  (global-set-key (kbd "C-x k") 'cemacs-kill-volatile-buffer)
  ;; Org Mode
  (global-set-key (kbd "C-M-#")
                  '(lambda()
                     (interactive)
                     (call-interactively
                      (org-time-stamp cemacs-universal-argument-double 'inactive))
                     ))
  ;; Unbind
  )
(defun cemacs-init-setup()
  "Run after-initilization setup."
  (interactive)
  ;;Misc Setup
  (delete-other-windows)
  (setq-default indent-tabs-mode nil
                )
  (setq inhibit-compacting-font-caches t   ;performance improvement
        ;; Mode Setting
        indent-tabs-mode nil               ;use spaces for indendation
        transient-mark-mode nil
        global-hl-line-mode t
        )
  (global-subword-mode t)
  (global-whitespace-mode t)
  (global-visual-line-mode)
  ;; Backup
  (setq make-backup-files nil
        backup-by-copying t
        auto-save-default nil)
  ;; Niggles
  (setq custom-file (concat user-emacs-directory cemacs-custom-variables-dir)
                                        ; move location of custom file
        recentf-max-saved-items 1000
        )
  (add-hook 'find-file-hook #'recentf-save-list)
  ;; (add-hook 'write-file-functions #'recentf-save-list)
  ;; (add-hook 'kill-buffer-hook #'recentf-save-list)
  ;; TODO(mallchad) this should really be a function
  (fset 'yes-or-no-p 'y-or-n-p ) ; Make all yes or no prompts consistent
  ;; Run Functions
  (cemacs-configure-session-decorations)
  (cemacs-vanilla-keys-configure)
  (run-hooks 'admin-cemacs-init-setup-hook)
  (run-hooks 'cemacs-init-setup-hook)
  )
;;Req Package Setup


(require 'req-package)
(setq use-package-always-ensure t)
;; Built in Packages
(req-package flyspell
  :config
  (define-key flyspell-mode-map [(control ?\,)] nil)
  (define-key flyspell-mode-map [(control ?\.)] nil)
  )
(req-package org
  :config
  (org-defkey org-mode-map (kbd "C-,") 'pop-to-mark-command)
  )
;; External Packages
(req-package async
  :config
  (setq async-bytecomp-package-mode t)
  )
(req-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t)
  )
(req-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  :config
  ;; Apply Aggressive Indent Patch
  (require 'aggressive-indent-patch)
  )
(req-package all-the-icons
  :config
  )
(req-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char)
  (global-set-key (kbd "M-r") 'avy-pop-mark)
  (setq avy-highlight-first t
        avy-background t
        )
  (set-face-attribute 'avy-lead-face nil :background '"firebrick" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "navy" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "dark olive green" :foreground "white")
  )
(req-package backup-each-save
  :hook
  (after-save . backup-each-save)
  (cemacs-kill-volatile-buffer-pre . (lambda ()
                                       (if (buffer-file-name)
                                           (backup-each-save)
                                         )))
  :config
  )
(req-package beacon
  :hook
  (cemacs-init-setup . beacon-mode)
  :config
  (setq beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1    ;; blink if the line changes
        beacon-blink-when-point-moves-horizontally 20
        )
  )
(req-package centaur-tabs
  :hook
  (cemacs-init-setup . centaur-tabs-mode)
  :bind
  ("<C-tab>" . centaur-tabs-forward)
  ("<C-S-tab>" . centaur-tabs-backward)
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  :init
  ;;Misc Settings
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-close-button nil
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "bar"
        centaur-tabs-set-bar t
        centaur-tabs-bar 'over
        centaur-tabs-modified-marker "*"
        centaur-tabs-mode t
        )
  :config
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
  (require 'company-tng-patch)
  (company-tng-configure-default)
  (setq company-require-match 'never
        company-idle-delay 0.05
        )
  )
(req-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-x e") 'cemacs-find-user-init-file)
  (global-set-key (kbd "C-j") 'crux-top-join-line)
  (global-set-key (kbd "C-x C-o") 'crux-swap-windows)
  (global-set-key (kbd "C-o") 'crux-smart-open-line-above)
  )
(req-package cmake-mode
  )
(req-package csharp-mode
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
(req-package fireplace)
(req-package flycheck
  :require flycheck-inline
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)
  :config
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
  ;;TODO(mallchad) Need to reduce the size of the space after helm source
  ;; Don't use helm's own displaying mode line function
  (set-face-attribute 'helm-source-header nil
                      :height 1.1
                      :foreground "dark cyan"
                      )
  ;; Keybinds
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
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
  (global-hungry-delete-mode)
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
  ;; TODO(mallchad) error occurs sometimes when killing-non-file buffers
  ;; (defhydra hydra-emacs (:color blue :hint nil))
  )
(req-package lsp-mode
  :after company
  ;; :require company-lsp
  :hook
  (c++-mode . lsp)
  (c-mode . lsp)
  (csharp-mode . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        )
  ;; (push 'company-lsp company-backends)
  )
(req-package lsp-ui
  :after
  lsp-mode
  flycheck
  :hook
  (lsp-ui-mode . (lambda() (lsp-ui-doc-mode -1)))
  :config
  (setq-default lsp-ui-doc-position 'top)
  )
(req-package magit
  :config
  )
(req-package lua-mode
  :after
  lsp
  :hook
  (lua-mode . lsp)
  :config
  (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t))
  ;; TODO(mallchad) should automatically extract emmy-lua for supported systems
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
  (global-set-key (kbd "C-x C-a") 'restart-emacs)
  )
(req-package rtags
  :require
  :config
  ;; TODO(need to evaluate whether is package is more useful than lsp or not)
  )
(req-package smartparens
  :hook
  (cemacs-init-setup . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  ;;Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)
  ;; TODO(mallchad) need  to setup bindings for this package
  ;; TODO(mallchad) need to get rid of annoying escape character completion
  ;; TODO(mallchad) need to make angled bracket pair for cc modes
  ;; TODO(mallchad) need to evalue a potentiol workaround to make hybrid
  ;; sexps spill over lines to make it useful for cc mode development
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
  :hook
  (cemacs-init-setup . sublimity-mode)
  :config
  (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  ;;(require 'sublimity-map) ;; experimental

  (setq sublimity-scroll-weight 7
        sublimity-scroll-drift-length 0
        ;;      sublimity-map-size 10
        ;;      sublimity-map-fraction 0.3
        ;;      sublimity-map-text-scale -7
        ;;      sublimity-map-set-delay 1
        )
  ;; TODO(mallchad) see if minimap or sublimity map can be readded
  )
(req-package treemacs
  :require lsp-treemacs
  :hook
  (treemacs-mode . (lambda() (text-scale-adjust -2)))
  :config
  (setq treemacs-width 30
        treemacs-position 'right
        )
  ;; TODO(mallchad) this package needs bindings
  )
(req-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  (setq undo-tree-enable-undo-in-region nil  ; brings performance enhancement
        undo-tree-history-directory-alist backup-directory-alist
        )
  ;; TODO(mallchad) need to setup undo-tree persistent history
  ;; Unbind Included Keymaps
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  (define-key undo-tree-map (kbd "C-x r") nil)
  )
(req-package vlf
  :config
  (require 'vlf-setup)
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
  )
(req-package zoom
  )
;;Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
(cemacs-init-setup)
(provide 'init)
;;; init.el ends here
