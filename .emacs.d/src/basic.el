


;;; This is a rather awkward attempt to tweak the emacs default to be more
;;; comfortable for average users without sacrificing the immense power of emacs

(defun basic-common-init ()

  ;; Styling
  (setq-default mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
    (vc-mode vc-mode)
    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
   )

  (setq
     ;; GDB doesn't work as well when you have to keep recreating your watch expressions
   gdb-delete-out-of-scope nil


   delete-selection-mode t
   )

  ;; The mark is much more powerful when transient-mark-mode is nil
    ;; If you still need transient-mark-mode like many functions need just
    ;; press `C-SPC' twice to enable it.
    ;; Really in future more functions should be normallized to respect transient mark off...
    (transient-mark-mode -1)

  ;; Great noob friendly way to manage multiple window configurations
  (tab-bar-mode 1)
  (tool-bar-mode -1)

  (pixel-scroll-mode)

  ;; Only really the left fringe does useful things, its just visually noisy to have both
  ;; ("left-only" . (nil . 0))
  (set-fringe-mode '(nil . 0))

  ;; It's really offputting to have 2 different y or n providers
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Disable pain in the arse insert mode by default
  (fset 'overwrite-mode 'ignore)

  ;; Should be a want-to-have feature for all programming projects
  (add-hook 'prog-mode'hook 'display-line-numbers-mode)
  (add-hook 'prog-mode'hook 'flymake-mode)

  (global-set-key (kbd "<mouse-3>") 'mouse-bar-open)

  ;; Load Last for performance
  (load-theme 'wombat)
)

(defun basic-init ()
  (basic-common-init)
  
  ;; Use noob friend editing keys, C-z, C-c, C-v
  (cua-mode 1)
)

(defun amalgamacs-init (&optional external-libraries)
    "Opinionated version of basic-init"

    (basic-common-init)
    ;; Styling
    (setq-default
     ;; The mode-line isn't that important we just need the buffer name, we can
     ;; get that from other thinsg like the tab-bar
     mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
     )

    ;; This  really only  exists for  noobs  to use  emacs, it  isn't that  well
    ;; organized and missing  a lot of commands,  just disable it to  get rid of
    ;; visual noise
    (menu-bar-mode -1)

    ;; Load Last for performance
    (when (not  (bound-and-true-p external-libraries))
      )
    (when (bound-and-true-p external-libraries)
      )

    "target external libraries
- beacon
- smooth-scroll (it's kind of really slow)
- which-key
- natural
- bury successful compilation ;; Has kind of janky logic would need to be rewritten
- projectile (very important)
- visible mark
- volatile highlight
- good scroll mode


Heavier packages
- ivy (or something similar)
- swiper
- backup-each-save
- undo-tree
- flycheck
- rainbow-delimiters
"
)

"Things the user should probably use
- tabs
- multiple window management
- compilation buffer
- compilation buffer error movement
- describe-(keys)
- info-emacs-manual
- project management
- revert-buffer
- basic navigation commands
- incremental search
- occur
- query replace
- ispell (if supported)
- the mark
- pop mark and pop global mark
- swap point and mark
- kill ring
- paragraph navigation
- mark page
- emacs tutorial
- fill paragraph
"

(basic-init)
