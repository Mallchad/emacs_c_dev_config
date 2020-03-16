;; Extra Load Paths
(add-to-list 'load-path '"~/.emacs.d/etc/")
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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))
  )
(package-initialize)
(defun slay-function()
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun slay-whole-buffer()
  ;;Kills the whole buffer
  (interactive)
  (mark-whole-buffer)
  (kill-region (region-beginning) (region-end))
  )
(defun scroll-up-in-place()
  (interactive)
  (forward-line -1)
  (View-scroll-line-backward)
  )
(defun scroll-down-in-place()
  (interactive)
  (forward-line 1)
  (View-scroll-line-forward)
  )
(defun global-keys-setup()
  ;;Sets up personal keybinds after initilization
  ;;Emacs Control Bindings
  (global-set-key (kbd "C-x r") 'revert-buffer)
  (global-set-key (kbd "M-p") 'scroll-up-in-place)
  (global-set-key (kbd "M-n") 'scroll-down-in-place)
  ;;sexp Navigation
  )
(defun cpp-mode-setup()
  ;; c-indent-comment-alist, c-indent-comments-syntactically-p (see Indentation Commands);
  ;; c-doc-comment-style (see Doc Comments);
  ;; c-block-comment-prefix, c-comment-prefix-regexp (see Custom Filling and Breaking);
  ;; c-hanging-braces-alist (see Hanging Braces);
  ;; c-hanging-colons-alist (see Hanging Colons);
  ;; c-hanging-semi&comma-criteria (see Hanging Semicolons and Commas);
  ;; c-cleanup-list (see Clean-ups);
  ;; c-basic-offset (see Customizing Indentation);
  ;; c-offsets-alist (see c-offsets-alist);
  ;; c-comment-only-line-offset (see Comment Line-Up);
  ;; c-special-indent-hook, c-label-minimum-indentation (see Other Indentation);
  ;; c-backslash-column, c-backslash-max-column (see Custom Macros).
  ;; (add-to-list ())
  )
(add-hook 'c++-mode-hook 'cpp-mode-setup)
(defun elisp-mode-setup()
  )
(add-hook 'emacs-lisp-mode 'elisp-mode-setup)
(defun programming-mode()
  ;;Sets up buffer for programming
  (display-line-numbers-mode)
  (electric-indent-mode 0))
(defvar init-setup-hook nil
  ;;A normal hook that runs at the end of init setup
  )
(defvar admin-init-setup-hook nil
  ;;A normal hook that runs admin setup hook prior to init-setup-hook
  )
(defun init-setup()
  ;;Runs after-initilization setup
  (interactive)
  ;;Set Fonts
  (WITH_SYSTEM gnu/linux
    (add-to-list 'default-frame-alist
		 '(font . "Source Code Pro-13:style=Regular")
		 ))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
		 '(font . "Consolas-13:style=Regular")
		 ))
  (toggle-frame-maximized)
					;(toggle-frame-fullscreen)
  ;;Disable window decorations
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-parameter nil 'undecorated nil)
  (fringe-mode 0)
  ;;Enable built-in modes
  (global-hl-line-mode)
  ;; Disable Window Decorations
  (setq menu-bar-mode nil
        tool-bar-mode nil
        scroll-bar-mode nil
        )
  (fringe-mode 0)
  (set-frame-parameter nil 'undecorated nil)
  ;;Misc Setup
  (global-keys-setup)
  (delete-other-windows)
  (split-window-horizontally)
  (setq inhibit-compacting-font-caches t   ;performance improvement
        ;; Mode Setting
        global-subword-mode t              ;easier navigation for camelcasing
        indent-tabs-mode nil               ;use spaces for indendation
        transient-mark-mode nil
        global-hl-line-mode t
        )
  (add-hook 'prog-mode-hook 'programming-mode)
  (run-hooks 'admin-init-setup-hook)
  (run-hooks 'init-setup-hook)
  )
;;Req Package Setup
(require 'req-package)
(setq use-package-always-ensure t)
(req-package async
  :config
  (setq async-bytecomp-package-mode t)
  )
  (req-package color-theme-sanityinc-tomorrow
    :config
    (load-theme 'sanityinc-tomorrow-bright t)
    )
(req-package ace-window
  :require ace-window
  :config
  )
(req-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c++-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))
               )
  )
;; (req-package all-the-icons
;;   :init
;;   (setq inhibit-compacting-font-caches t) ;;Improve windows performance
;;   )
(req-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char)
  (setq avy-highlight-first t
        avy-background t
        )
  (set-face-attribute 'avy-lead-face nil :background '"firebrick" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "navy" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "dark olive green" :foreground "white")
  )
(req-package beacon
  :hook
  (init-setup . beacon-mode)
  :config
  (setq beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1
        beacon-blink-when-point-moves-horizontally 20
        )
  )
(req-package centaur-tabs
  :hook
  (init-setup . centaur-tabs-mode)
  :bind
  ("<C-tab>" . centaur-tabs-forward)
  ("<C-S-tab>" . centaur-tabs-backward)
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
  (company-tng-configure-default)
  (setq company-require-match 'never
        company-idle-delay 0.1
        )
  )
(req-package crux
  :config
  (global-set-key (kbd "C-x e") 'crux-find-user-init-file)
  )
(req-package csharp-mode
  :require csharp-mode
  :config
  )
(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )
(req-package flycheck
  :hook
  (init-setup . global-flycheck-mode)
  :config
  )
(req-package flycheck-clang-analyzer
  :require flycheck
  :config
  (flycheck-clang-analyzer-setup)
  )
(req-package flycheck-color-mode-line
  :require flycheck
  :hook
  (flycheck-mode . flycheck-color-mode-line-mode)
  (global-flycheck-mode . flycheck-color-mode-line-mode)
  )
(req-package flycheck-inline
  :require flycheck
  :hook
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)
  )
(req-package flycheck-irony
  :config
  )
(req-package god-mode
  :require god-mode
  :config
  )
(req-package hide-mode-line
  :hook
  (init-setup . global-hide-mode-line-mode)
  )
(req-package helm
  :after hydra
  :require
  helm-projectile
  helm-flycheck
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
        helm-autoresize-max-height 40   ;Always takes up half the screen
        helm-autoresize-min-height 40
        helm-split-window-in-side-p t   ;Shows helm window in current buffer
        helm-mode-line-string nil
        )
  :config
  (require 'helm-config)
 ;;;Helm minibuffer config
  ;; Don't use helm's own displaying mode line function
  (set-face-attribute 'helm-source-header nil
                      :height 1.1
                      :foreground "dark cyan"
                      )
  ;; (set-face-attribute 'helm-eob-line nil :height 0.1)
  ;; (set-face-attribute 'helm-helper nil :height 0.1))
  ;; (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
  ;; (defun helm-toggle-header-line ()
  ;;   (if (> (length helm-sources) 1)
  ;;    (set-face-attribute 'helm-source-header
  ;;                        nil
  ;;                        :foreground helm-source-header-default-foreground
  ;;                        :background helm-source-header-default-background
  ;;                        :box helm-source-header-default-box
  ;;                        :height 1.0)
  ;;     (set-face-attribute 'helm-source-header
  ;;                      nil
  ;;                      :foreground (face-attribute 'helm-selection :background)
  ;;                      :background (face-attribute 'helm-selection :background)
  ;;                      :box nil
  ;;                      :height 0.1)))
  ;; (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
                                        ;(add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
  ;;Keybinds
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;;Swap Action and Completion Buttons
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  ;;Query
  (defhydra hydra-query (global-map "C-q" :color blue)
    "query for"
    ("s" helm-occur "string")
    ("b" helm-bookmarks "bookmarks")
    ("p" helm-projectile "project")
    ("f" helm-flycheck "flycheck")
    )
  )
(req-package highlight-parentheses
  :hook
  (init-setup . global-highlight-parentheses-mode)
  :config
  (setq hl-paren-background-colors '("gray"
                                     )
        hl-paren-colors '("black")
        hl-paren-highlight-adjacent nil
        )
  )
(req-package hl-todo
  :config
  (global-hl-todo-mode
   )
  )
(req-package hydra
  :config
  (defhydra hydra-slayer (global-map "C-s" :color blue)
    "kill shortcuts"
    ("x" slay-function "function(x)" :color red)
    ("l" kill-whole-line "whole line" :color red)
    ("b" kill-whole-buffer "whole buffer")
    )
                                        ;(defhydra hydra-grab (:color)
                                        ;  )
                                        ;(global-set-key (kbd "C-g") (hydra-grab/body))
  )
(req-package lsp-mode
  :after company
  :require company-lsp
  :hook
  (c++-mode . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil)
  (push 'company-lsp company-backends)
  )
(req-package lsp-ui
  :after
  lsp-mode
  flycheck
  :config
  )
(req-package magit
  :require magit
  :config
  )
(req-package multiple-cursors
  :require hydra
  :config
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
  )
(req-package projectile
  :hook
  (init-setup . projectile-mode)
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
  )
(req-package smartparens
  :hook
  (init-setup . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  ;;Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)
  )
(req-package smooth-scrolling
  :hook
  (init-setup . smooth-scrolling-mode)
  :config
  (setq scroll-conservatively nil)
  )
(req-package sublimity
  :hook
  (init-setup . sublimity-mode)
  :config
  (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  ;;(require 'sublimity-map) ;; experimental

  (setq sublimity-scroll-weight 15
        sublimity-scroll-drift-length 0
        ;;      sublimity-map-size 10
        ;;      sublimity-map-fraction 0.3
        ;;      sublimity-map-text-scale -7
        ;;      sublimity-map-set-delay 1
        )
  )
(req-package treemacs
  )
(req-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  (setq undo-tree-enable-undo-in-region nil)
  )
(req-package volatile-highlights
:config
(volatile-highlights-mode)
)
(req-package ws-butler
:config
(ws-butler-global-mode)
)
(req-package zoom
  )
;;Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
;;Automatic Custom Variables
(custom-set-variables)
(custom-set-faces)

(init-setup)
(provide 'init)
;;; init.el ends here
