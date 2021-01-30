(use-package dired
  :ensure nil
  :commands (dired dired-jump)
	:bind (("C-x C-j" . dired-jump))
	:config
	(evil-collection-define-key 'normal 'dired-mode-map
		"h" 'dired-up-directory
		"l" 'dired-find-file)
	(setq dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil))

(use-package dired-single
    :ensure t
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-dired)