;; #############################################
;; Ivan Willig's emacs config file for emacs 24
;; Includes supports working in
;;    Javascript
;;    Python
;;    Clojure
;;
;; Requires package.el, emacs package system
;; 
;; #############################################
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; some global settings
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(global-rainbow-delimiters-mode)
(setq inhibit-splash-screen t)
(add-to-list 'load-path "~/.emacs.d/")

(global-font-lock-mode 1)
(put 'downcase-region 'disabled nil)

(global-linum-mode 1)

;; ----------------------------------------
;; safely install all of the packages
(defun install-packages ()
  (let ((packages '(magit python-mode clojure-mode
                          starter-kit starter-kit-lisp starter-kit-bindings starter-kit-ruby
                          flymake-cursor geiser rainbow-delimiters scheme-complete scala-mode
                          zenburn-theme coffee-mode autopair
                          nrepl yaml-mode paredit js2-mode)))

    (dolist (p packages)
      (when (not (package-installed-p p))
        (message "Installing package %s" p)
        (package-install p)))))

(install-packages)

(load-theme 'misterioso)

;;(set-default-font "terminus")
(set-default-font "Liberation Mono 11")
(setq tab-width 4)

;; set up white space mode and enable it globally
(require 'whitespace)

(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; turn off the tool and menu bar by default
(progn
  (dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))


;; text editing mode
;; add some spelling checkers for the text modes
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
(add-hook 'rst-adjust-hook (lambda () (flyspell-mode)))

;; ido mode
(require 'ido)
(ido-mode t)

(require 'autopair)
(autopair-global-mode)

;; ----------------------------------------
;; python mode
(require 'python-mode)
;; make sure we never use tabs... ever.
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (hs-minor-mode t)))

;; ----------------------------------------
;; javascript mode

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(setq js-indent-level 4)

(add-hook 'js-mode-hook (lambda () (paredit-mode -1)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))


(defun jslint-info ()
  (interactive)
  (insert "/*jslint nomen: true */  \\n"
          "/*global define: true */"))


(add-hook
 'js-mode-hook
 (lambda ()
   (imenu-add-menubar-index)
   (hs-minor-mode t)))

;; flymake mode
(when (load "flymake" t)


  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
    	       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jslint" (list "--terse" local-file))))


  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "/home/ivan/.emacs.d/pycheckers"  (list local-file))))

  
  (setq flymake-err-line-patterns
	(cons '("^\\(.*\\)(\\([[:digit:]]+\\)):\\(.*\\)$"
		1 2 nil 3)
	      flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.js\\'" flymake-jslint-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))


(require 'flymake-cursor)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;  ----------------------------------------
;; elisp mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;; use eldoc 

;; ----------------------------------------
;; slime mode
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
          (defun paredit-mode-enable () (paredit-mode 1))
    (add-hook 'slime-mode-hook 'paredit-mode-enable)
    (add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
    (setq slime-protocol-version 'ignore)))

;; (require 'slime)
;; (slime-setup)

(require 'clojure-mode)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (flyspell-prog-mode)))

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; yaml-mode info
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; scheme mode
;;; use quack
;;; set the default scheme binary to racket
 (setq scheme-program-name "racket")

(autoload 'scheme-smart-complete "scheme-complete" nil t)


(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
  (lambda ()
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
    (eldoc-mode)))

(require 'geiser)
(setq geiser-active-implementations '(racket))
