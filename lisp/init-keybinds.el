(use-package general
  :config
  (general-evil-setup t)
  
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-SPC")
  (general-imap "j"
              (general-key-dispatch 'self-insert-command
                :timeout 0.25
                "k" 'evil-normal-state))

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(load-theme :which-key "choose theme")))


(use-package which-key
    :config
    (which-key-mode))

(provide 'init-keybinds)
