;; #############################################
;; Ivan Willig's emacs confi file for emacs 23
;; 
;; #############################################

(load-file "~/.emacs.d/package.el")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(setq inhibit-splash-screen t)
(add-to-list 'load-path "~/.emacs.d/")

;; set up white space mode and enable it globally
(require 'whitespace)

(setq whitespace-style (quote
  ( spaces tabs newline space-mark tab-mark newline-mark)))
;; (global-whitespace-mode 1)

;; turn off the tool and menu bar by default
(progn
  (dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))


;; ----------------------------------------
;; safely install all of the packages
(defun install-packages ()
  (let ((packages '(magit python-mode clojure-mode 
                          nrepl
                          slime slime-repl paredit color-theme js2-mode)))
    (dolist (p packages)
      (when (not (package-installed-p p))
        (message "Installing package %s" p)
        (package-install p)))))

(install-packages)

;; text editing mode
;; add some spelling checkers for the text modes
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
(add-hook 'rst-adjust-hook (lambda () (flyspell-mode)))


;; ----------------------------------------
;; color theme stuff
(require 'color-theme)
(require 'color-theme-subdued)
(color-theme-subdued)
(set-default-font "Terminus")

;; ----------------------------------------
;; ido mode
(require 'ido)
(ido-mode t)


;; ----------------------------------------
;; python mode
(require 'python-mode)
;; make sure we never use tabs... ever.
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))

;; ----------------------------------------
;; javascript mode
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(setq js2-indent-level 4)


(eval-after-load 'js
  '(progn 
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u03BB")
                                 nil)))))))




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

(require 'slime)
(slime-setup)

(require 'clojure-mode)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (flyspell-prog-mode)))

