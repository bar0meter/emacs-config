;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
;; (use-package selectrum
;;   :straight (:host github :repo "raxod502/selectrum")
;;   :custom
;;   (selectrum-count-style 'current/matches)
;;   :init
;;   ;; This doesn't actually load Selectrum.
;;   (selectrum-mode +1)
;;   (dawran/leader-keys "TAB" #'selectrum-repeat))

;; ;; Map C-j and C-k to next and previous candidate for selectrum
;; ;; (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
;; ;; (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate)

;; (define-key minibuffer-local-map (kbd "C-j") 'next-line-or-history-element)
;; (define-key minibuffer-local-map (kbd "C-k") 'previous-line-or-history-element)

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
;; (use-package prescient
;;   :config
;;   ;; Remember usage statistics across Emacs sessions.
;;   (prescient-persist-mode +1)
;;   ;; The default settings seem a little forgetful to me. Let's try
;;   ;; this out.
;;   (setq prescient-history-length 1000))

;; ;; Package `selectrum-prescient' provides intelligent sorting and
;; ;; filtering for candidates in Selectrum menus.
;; (use-package selectrum-prescient
;;   :straight (:host github :repo "raxod502/prescient.el"
;;                    :files ("selectrum-prescient.el"))
;;   :after selectrum
;;   :config
;;   (selectrum-prescient-mode +1))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light nil)))

;; Package `ctrlf' provides a replacement for `isearch' that is more
;; similar to the tried-and-true text search interfaces in web
;; browsers and other programs (think of what happens when you type
;; ctrl+F).
;; (use-package ctrlf
;;   :straight (:host github :repo "raxod502/ctrlf")
;;   :bind
;;   ("s-f" . ctrlf-forward-literal)

;;   :init



;;   (ctrlf-mode +1))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("C-l" . consult-goto-line)
         ("C-q" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s g" . consult-git-grep)      ;; Alternatives: consult-grep, consult-ripgrep
         ("M-s f" . consult-find)          ;; Alternatives: consult-locate, my-fdfind
         ("C-s" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s s" . consult-isearch)
         ;; Other bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))

  ;; Optionally configure the register preview function. This gives a
  ;; consistent display for both `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  ;; Optionally tweak the register preview window.
  ;; * Sort the registers
  ;; * Hide the mode line
  ;; * Resize the window, such that the contents fit exactly
  (advice-add #'register-preview :around
              (lambda (fun buffer &optional show-empty)
                (let ((register-alist (seq-sort #'car-less-than-car register-alist)))
                  (funcall fun buffer show-empty))
                (when-let (win (get-buffer-window buffer))
                  (with-selected-window win
                    (setq-local mode-line-format nil)
                    (setq-local window-min-height 1)
                    (fit-window-to-buffer)))))

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Configure preview. Note that the preview-key can also be configured on a
  ;; per-command basis via `consult-config'.
  ;; The default value is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))
(setq selectrum-refine-candidates-function #'orderless-filter)
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

(use-package embark
  :bind
  ("C-c s" . embark-act)

  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  :custom
  (embark-action-indicator
   (lambda (map)
     (which-key--show-keymap "Embark" map nil nil 'no-paging)
     #'which-key--hide-popup-ignore-command)
   embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'init-selectrum)
