(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package smartparens
	:config
  (smartparens-global-mode t))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

;; (use-package tree-sitter
;;   :init (global-tree-sitter-mode)
;;   :hook ((ruby-mode . tree-sitter-hl-mode)
;;          (js-mode . tree-sitter-hl-mode)
;;          (typescript-mode . tree-sitter-hl-mode)
;;          (go-mode . tree-sitter-hl-mode)))
;; (use-package tree-sitter-langs)

;; (use-package highlight-indent-guides)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (setq highlight-indent-guides-method 'character)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :custom
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-style "bar")
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "●")
;;   (centaur-tabs-set-bar 'under)
;;   (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)

;;   :bind
;;   (("s-{" . #'centaur-tabs-backward)
;;    ("s-}" . #'centaur-tabs-forward)))

(provide 'init-editor)
