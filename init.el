;;; package --- Summary:
;;; Commentary:

;; Supports
;;    Clojure
;;    JavaScript
;; #############################################

;; ----- Package setup -----

;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; ----- Default Front -----

(set-face-attribute 'default nil :family "Liberation Mono")

;; ----- Defaults -----
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(auto-fill-mode -1)

(setq-default

 inhibit-splash-screen t
 indent-tabs-mode nil
 ring-bell-function 'ignore
 whitespace-line-column 250
 ispell-program-name "aspell"

 gc-cons-threshold 100000000)

(put 'downcase-region 'disabled nil)

(global-font-lock-mode 1)

;; (global-linum-mode 1)

(global-auto-revert-mode t)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(x-focus-frame nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(load-file "~/.emacs.d/private.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

(ido-mode +1)

;; ----- Third Party Packages -----
(require 'use-package)

(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t)

(use-package diff-hl
  :ensure t
  :init ())

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(setq flycheck-checkers '(javascript-eslint))

(use-package indent-guide
  :ensure t)

(use-package eldoc
  :ensure t
  :init (indent-guide-global-mode))

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("<C-tab>" . company-complete))

(use-package autopair
  :ensure t
  :init (autopair-global-mode))

;; ----- Power line ------

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

;; ----- Clojure -----
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
  (setq ag-highlight-search t
        ag-reuse-window 't))

(use-package ido
  :ensure t
  :init (ido-mode t))

(use-package markdown-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


  (setq js2-basic-offset 2
        js2-bounce-indent-p t))

(use-package mocha
  :ensure t)

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

(use-package quack
  :ensure t)

(use-package geiser
  :ensure t
  :config (setq geiser-active-implementations '(guile)))

(use-package indent-guide
  :ensure t)

(use-package circe
  :ensure t)

(defun setup-irc ()
  (require 'circe)
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  (setq circe-reduce-lurker-spam t)
  (setq circe-network-options
        `(("Freenode"
           :nick "iwillig"
           :channels ("#craftyplans"
                      "#fsf"
                      "#clojure"
                      "#postgis"
                      "#clojurescript"
                      "#datomic")
           :nickserv-password ,freenode-password))))

(setup-irc)

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe "Freenode"))

(use-package ibuffer-vc
  :ensure t
  :config (add-hook 'ibuffer-hook
                    (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "M-m") 'er/expand-region))

(use-package workgroups2
  :ensure t
  ;;:config (workgroups-mode 1)
  )

(use-package smex
  :ensure t
  :config
  ;;(smex-initialize)
  ;;(global-set-key (kbd "M-x") 'smex)
  ;;(global-set-key (kbd "M-x") 'smex-major-mode-commands)
  )


;; ----- Themes -----

;; (use-package afternoon-theme)
;; (load-theme 'wombat t)
(use-package base16-theme
  :ensure t)

(load-theme 'base16-default-dark t)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq sml/theme 'dark)
  )

;; ----- Social -----

(use-package elfeed
  :ensure t
  :config (setq elfeed-feeds
                '("http://planet.gnu.org/rss20.xml"
                  "https://hacks.mozilla.org/feed/"
                  "http://z.caudate.me/rss/"
                  "http://planet.clojure.in/atom.xml"
                  "http://planet.lisp.org/rss20.xml"
                  "http://xkcd.com/rss.xml"
                  "http://prl.ccs.neu.edu/blog/feeds/all.atom.xml"
                  "http://www.scheme.dk/planet/atom.xml"
                  "http://www.universetoday.com/feed/"
                  "http://lambda-the-ultimate.org/rss.xml"
                  "http://planet.emacsen.org/atom.xml")))

(provide 'init)
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
