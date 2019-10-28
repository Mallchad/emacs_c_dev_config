;;Automatic Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(helm-mode t)
 '(package-selected-packages
   (quote
    (el-get req-package aggressive-indent centaur-tabs color-theme-sanityinc-tomorrow company company-irony csharp-mode flycheck flycheck-irony god-mode helm helm-projectile highlight-indentation irony magit powerline projectile rtags smartparens smooth-scroll smooth-scrolling sr-speedbar srefactor sublimity ws-butler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;Req Package Setup
(require 'req-package)
(req-package aggressive-indent
  :require aggressive-indent
  :config
  )
(req-package centaur-tabs
  :require centaur-tabs
  :config
  )
(req-package color-theme-sanityinc-tomorrow
  :require color-theme-sanityinc-tomorrow
  :config
  )
(req-package company
  :require company
  :config
  )
(req-package company-irony
  :require company-irony
  :config
  )
(req-package csharp-mode
  :require csharp-mode
  :config
  )
(req-package flycheck
  :require flycheck
  :config
  )
(req-package flycheck-irony
  :require flycheck-irony
  :config
  )
(req-package god-mode
  :require god-mode
  :config
  )
(req-package helm
  :require helm
  :config
  )

(req-package helm-projectile
  :require helm-projectile
  :config
  )
(req-package highlight-indentation
  :require highlight-indentation
  :config
  )
(req-package irony
  :require irony
  :config
  )
(req-package magit
  :require magit
  :config
  )
(req-package powerline
  :require powerline
  :config
  )
(req-package projectile
  :require projectile
  :config
  )
(req-package rtags
  :require
  :config
  )
(req-package smartparens
  :require
  :config
  )
(req-package smooth-scroll
  :require smooth-scroll 
  :config
  )
(req-package smooth-scrolling
  :require smooth-scrolling
  :config
  )
(req-package sr-speedbar
  :require sr-speedbar
  :config
  )
(req-package sublimity
  :require sublimity
  :config
  )
(req-package ws-butler
  :require ws-butler
  :config
  )
