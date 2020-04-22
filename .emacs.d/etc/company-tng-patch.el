
;; This is simply a patch to fix the behaviour of company-lsp
;; when used with company-tng
(require 'company-tng)

;;;###autoload
(defun company-tng-frontend (command)
  "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion."
  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-tng--overlay ov)
       (overlay-put ov 'priority 2))
     (advice-add 'company-select-next :before-until 'company-tng--allow-unselected)
     (advice-add 'company-fill-propertize :filter-args 'company-tng--adjust-tooltip-highlight))
    (update
     (let ((ov company-tng--overlay)
	   (selected (nth company-selection company-candidates))
	   (prefix (length company-prefix)))
       (move-overlay ov (- (point) prefix) (point))
       (overlay-put ov
		    (if (= prefix 0) 'after-string 'display)
		    (and company-selection-changed selected))))
    (hide
     (when company-tng--overlay
       (delete-overlay company-tng--overlay)
       (kill-local-variable 'company-tng--overlay))
     (advice-remove 'company-select-next 'company-tng--allow-unselected)
     (advice-remove 'company-fill-propertize 'company-tng--adjust-tooltip-highlight))
    (pre-command
     (when (and company-selection-changed
		(not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)
       ))))

(defvar company-clang-insert-arguments)
(defvar company-semantic-insert-arguments)
(defvar company-rtags-insert-arguments)
(defvar lsp-enable-snippet)


;;;###autoload
(defun company-tng-configure-default ()
  "Apply the default configuration to enable company-tng."
  (setq company-require-match nil)
  (setq company-frontends '(company-tng-frontend
			    company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  (setq company-clang-insert-arguments nil
	company-semantic-insert-arguments nil
	company-rtags-insert-arguments nil
	lsp-enable-snippet nil)
  (advice-add #'eglot--snippet-expansion-fn :override #'ignore)
  (let ((keymap company-active-map))
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)
    (define-key keymap [tab] 'company-select-next)
    (define-key keymap (kbd "TAB") 'company-select-next)
    (define-key keymap [backtab] 'company-select-previous)
    (define-key keymap (kbd "S-TAB") 'company-select-previous)
    ))
(provide 'company-tng-patch)
;;; company-tng-patch.el ends here
