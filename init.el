;;; package --- Summary:
;;; Commentary:
;; Supports
;;    Clojure
;; #############################################

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
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

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column 250)
(setq show-trailing-whitespace t)
(setq ispell-program-name "aspell")
(x-focus-frame nil)

(progn
  (dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(require 'use-package)

(use-package  magit
  :bind ("C-c g" . magit-status)
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

(use-package powerline
  :ensure t
  :init (setq ns-use-srgb-colorspace nil))

(defun load-space-line ()
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package spaceline
  :ensure t
  :init (load-space-line))

(use-package spacemacs-theme
  :ensure t
  :init (load-theme 'spacemacs-dark t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(defun clojure-hook ()
  (require 'clojure-mode)
  (enable-paredit-mode)
  (define-clojure-indent
    (defroutes 'defun)
    (defroutes 'defun)
    (async     'defun)
    (for-all   'defun)
    (fact      'defun)
    (facts     'defun)
    (render    'defun)
    (query     'defun)
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
  :init (clojure-hook))

(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojure-mode-hook 'el-doc-mode)

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
