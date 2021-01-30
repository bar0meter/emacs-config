
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; LSP MODE
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((lsp-mode . efs/lsp-mode-setup)
  (java-mode . lsp)
  (go-mode . lsp-deferred)
  (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration)))))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
	(setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-highlight-symbol-at-point nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        )
  (lsp-enable-which-key-integration t)
  (setq lsp-idle-delay 0.500)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
							("<tab>" . company-indent-or-complete-common)
							(("\C-\M-b" . lsp-find-implementation)
							 ("M-RET" . lsp-execute-code-action)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; TODO: Disabling company box mode as it was causing overlapping issue
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
	:custom
	(lsp-ui-sideline-enable t)
	(lsp-ui-sideline-show-hover nil)
	(lsp-ui-doc-position 'bottom)
	(lsp-ui-doc-show))

(provide 'init-lsp)
