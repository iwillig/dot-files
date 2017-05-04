;;; package --- Summary:
;;; Commentary:

;; Supports
;;    Clojure
;;    JavaScript
;; #############################################

;; ----- Package setup -----

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (message ";; Refreshing the package archives")
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (message ";; Installing use package")
  (package-install 'use-package))

;; ----- Default Front -----

(set-default-font "Fira Code")

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(when (string-equal system-type "gnu/linux")
  (message ";; Setting the front on linux")
  (set-face-attribute 'default nil :height 112))

;; ----- Defaults -----
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(auto-fill-mode -1)
(global-prettify-symbols-mode +1)

(setq echo-keystrokes 0.01)


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
(global-linum-mode t)
(x-focus-frame nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

  (require 'clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (clj-refactor-mode 1)

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
  (setq cider-test-show-report-on-success t)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
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

;; ----- Javascript -----

(use-package js2-mode
  :ensure t
  :init

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

  (setq js2-basic-offset 2
        js2-bounce-indent-p t))

(use-package feature-mode
  :ensure t)

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

(use-package bnfc
  :ensure t)

;; ----- -----

(require 'eshell)
(require 'em-smart)

;; ----- Themes -----

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

(when (string-equal system-type "gnu/linux")
  (use-package spaceline
    :ensure t
    :init
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)))

;; ----- Common Lisp -----
(use-package slime
  :ensure t)

;; ----- Other stuff -----
(use-package define-word
  :ensure t
  :config (global-set-key (kbd "C-c d") 'define-word-at-point))

(use-package yaml-mode
  :ensure t)

;; ----- Org mode -----

(require 'ob-clojure)
(require 'cider)
(require 'ob)

;; Org mode!
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (clojure . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-babel-clojure-backend 'cider
      org-confirm-babel-evaluate nil)

(defvar org-babel-default-header-args:clojure
  '((:results . "silent")))

(defun org-babel-execute:clojure (body params)
  (print body params)
  (cider-interactive-eval body))

(add-hook 'org-src-mode-hook
          '(lambda ()
             (set (make-local-variable 'cider-buffer-ns)
                  (with-current-buffer
                      (overlay-buffer org-edit-src-overlay)
                    cider-buffer-ns))))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode +1))
