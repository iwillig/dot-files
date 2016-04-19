;;; package --- Summary:
;;; Commentary:
;; Supports
;;    Clojure
;;    JavaScript
;; #############################################

;; ----- Package setup -----
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ----- Default Front -----
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; ----- Defaults -----
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(auto-fill-mode -1)

(setq inhibit-splash-screen t)
(global-font-lock-mode 1)

(put 'downcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(global-auto-revert-mode t)
(global-hl-line-mode 1)

(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column 250)
(setq show-trailing-whitespace t)
(setq ispell-program-name "aspell")
(x-focus-frame nil)
(load-file "~/.emacs.d/private.el")

(progn
  (dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;;(desktop-save-mode 1)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; ----- Third Party Packages -----
(require 'use-package)

(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t)

(use-package git-timemachine
  :bind ("C-c C-g" . git-timemachine)
  :ensure t)

(use-package eldoc
  :ensure t)

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("<C-tab>" . company-complete))

(use-package autopair
  :ensure t
  :init (autopair-global-mode))

;; ----- Power line ------
(use-package powerline
  :ensure t
  :init (progn
          (require 'powerline)
          (powerline-default-theme)
          (setq ns-use-srgb-colorspace nil)))

(use-package minimap
  :ensure t)

(use-package avy
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t)

;; (use-package clj-refactor
;;   :ensure t
;;   :init (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun clojure-hook ()
  (require 'clojure-mode)
  (setq cljr-suppress-middleware-warnings t)
  (define-clojure-indent
    (defroutes 'defun)
    (defroutes 'defun)
    (async     'defun)
    (for-all   'defun)
    (fact      'defun)
    (facts     'defun)
    (against-background 'defun)
    (render    'defun)
    (query     'defun)
    (ident     'defun)
    (params    'defun)
    (future-fact 'defun)
    (future-facts 'defun)
    (endpoint 'defun)
    (on-status 'defun)
    (method 'defun)
    (on-request 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (context 'defun)))

(use-package clojure-mode
  :ensure t
  :init
  (clojure-hook)
  (add-hook 'clojure-mode-hook 'clojure-hook)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  ;;(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  ;;(add-hook 'clojure-mode-hook (lambda () (yas-minor-mode 1)))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package ag
  :ensure t
  :init
  (setq ag-highlight-search t)
  (setq ag-reuse-window 't))

(use-package highlight-sexp
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
  (setq hl-sexp-background-color "#2D3235"))

(use-package ido
  :ensure t
  :init (ido-mode t))

(use-package restclient
  :ensure t)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (setq js2-basic-offset 4)
  (setq js2-bounce-indent-p t))

(use-package mocha
  :ensure t)

(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((mocha-project-test-directory . "test")
     (mocha-options . "--recursive --reporter dot -t 5000")
     (mocha-environment-variables . "NODE_ENV=test")))))


(add-to-list 'load-path "~/opt/tern/emacs")
(use-package tern
  :init (add-hook 'js2-mode-hook (lambda () (tern-mode 1))))

(use-package company-tern
  :ensure t
  :init (add-to-list 'company-backends 'company-tern))

(use-package mustache-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode t))

(use-package circe
  :ensure t
  :init )

(defun setup-irc ()
  (require 'circe)
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  (setq circe-reduce-lurker-spam t)
  (setq circe-network-options
        `(("Freenode"
           :nick "iwillig"
           :channels ("#craftyplans" "#clojure" "#clojurescript" "#datomic")
           :nickserv-password ,freenode-password))))
(setup-irc)

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe "Freenode"))

;; ----- Themes -----

(use-package grandshell-theme
  :init (load-theme 'grandshell t)
  :ensure t)
