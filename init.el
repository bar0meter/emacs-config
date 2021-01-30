(menu-bar-mode -1)          ; Disable the menu bar

;; Ignore annoying sound for wrong command 
(setq ring-bell-function 'ignore)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq warning-minimum-level :emergency)

(defvar efs/default-font-size 130)
(defvar efs/default-variable-font-size 140)
(defvar efs/default-font-family "UbuntuMono Nerd Font")

;; font
(set-face-attribute 'default nil :font efs/default-font-family :height efs/default-font-size :weight 'regular)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font efs/default-font-family :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font efs/default-font-family :height efs/default-variable-font-size :weight 'regular)

;; tab
(setq tab-width 4)
(setq c-basic-offset 4)
(setq-default line-spacing 0.3)
(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'init-straight)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-themes) ;; UI
(require 'init-evil) ;; EVIL MODE
(require 'init-git) ;; VERSION CONTROL

(require 'init-org) ;; ORG MODE SETTINGS

(require 'init-ivy) ;; IVY
;; (require 'init-selectrum)
(require 'init-projectile) ;; Projectile
(require 'init-keybinds)
(require 'init-lsp) ;; LSP MODE

(require 'init-java) ;; JAVA
(require 'init-go) ;; GO
(require 'init-javascript) ;; JAVASCRIPT, TYPESCRIPT, VUE
(require 'init-editor)
(require 'init-flycheck)
(require 'init-yasnippet)
(require 'init-dired)
