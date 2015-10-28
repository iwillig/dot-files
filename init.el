;;; package --- Summary:
;;; Commentary:
;; Supports
;;    Javascript
;;    Python
;;    Clojure
;;    TypeScript
;;    CoffeScript
;; Requires package.el, emacs package system
;;
;; #############################################
(require 'package)
;;; Code:

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; some global settings
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(auto-fill-mode -1)

(setq inhibit-splash-screen t)
(global-font-lock-mode 1)

(put 'downcase-region 'disabled nil)
(setq-default indent-tabs-mode nil) ;; don't use tabs
(global-linum-mode 1) ;; i like line numbers
(global-auto-revert-mode t)
(global-hl-line-mode 1)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ----------------------------------------
;; safely install all of the packages
(defun install-packages ()
  (let ((packages '(magit
                    python-mode
                    ;; clojure start
                    clojure-mode
                    midje-mode
                    cider
                    clj-refactor
                    flycheck-clojure
                    flycheck-pos-tip
                    ;; clojure end
                    company
                    rainbow-delimiters
                    rainbow-mode
                    ag
                    autopair
                    scss-mode
                    yaml-mode
                    markdown-mode
                    scss-mode
                    sass-mode
                    flycheck
                    paredit
                    js2-mode
                    fringe-helper
                    cyberpunk-theme
                    subatomic-theme
                    powerline
                    spaceline
                    eyebrowse
                    anzu
                    git-timemachine
                    smeargle
                    gist
                    align-cljlet
                    git-gutter-fringe
                    flatland-theme
                    mustache-mode
                    browse-at-remote
                    spacemacs-theme
                    ample-theme
                    seti-theme
                    basic-theme
                    noctilux-theme
                    paradox
                    solarized-theme)))

    (dolist (p packages)
      (when (not (package-installed-p p))
        (message "Installing package %s" p)
        (package-install p)))))

(install-packages)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column 250)
(setq show-trailing-whitespace t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'ample t)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(x-focus-frame nil)

(require 'highlight-sexp)
(add-hook 'clojure-mode-hook 'highlight-sexp-mode)
(setq hl-sexp-background-color "#2D3235")


(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(setq cljr-suppress-middleware-warnings t)

(defun pp-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "python -mjson.tool" (current-buffer) t)))

;;; Key commands
(global-set-key (kbd "C-x i") 'indent-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x q") 'fill-paragraph)

(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)

(setq ispell-program-name "aspell")

;; git gutter mode
(global-git-gutter-mode t)

(require 'powerline)
(require 'spaceline-config)
(spaceline-emacs-theme)
(setq ns-use-srgb-colorspace nil)

(eyebrowse-mode t)

(global-set-key (kbd "C-c M-g") 'browse-at-remote)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; raindow mode makes emacs display the color of a hex value in the
;;; background of the test. Its useful for editing css files
(add-hook 'prog-mode-hook 'rainbow-mode)

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
;; Markdown mode hooks
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (whitespace-mode 1)))
(setq markdown-command "markdown_py")

;; ---------------------------------------
;; scss mode

(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; ----------------------------------------
;; python mode
(require 'python-mode)
;; make sure we never use tabs... ever.
(setq tab-width 2)

(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (hs-minor-mode t)))
(add-hook 'python-mode-hook (lambda () (flymake-mode)))

;; ----------------------------------------
;; Javascript mode

(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;(setq js2-indent-level 2)

(setq js2-basic-offset 2)
(setq js2-mode-show-strict-warnings nil)

(add-to-list 'load-path "~/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-hook 'js2-mode-hook (lambda () (whitespace-mode f)))
(add-hook 'js2-mode-hook (lambda () (paredit-mode -1)))
(add-hook 'js2-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js2-mode-hook (lambda () (turn-off-auto-fill)))

(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))

;; ----------------------------------------
;; tern stuff


(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck)

;;  ----------------------------------------
;; elisp mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;; use eldoc
;; ----------------------------------------

(require 'company)
;;(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)
(require 'cider)
(require 'clojure-mode)
;; cider stuff
;;(add-hook 'cider-repl-mode-hook #'company-mode)
;;(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (flyspell-prog-mode)))


(require 'clj-refactor)

(setq cider-repl-history-file "~/.cider_history")

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
  (Given 2)
  (When  2)
  (Then  2)
  (context 2)
  (describe 'defun)
  (context 'defun)
  (it 2)
  (should 2))

;; ####################
;; configure midje
(require 'midje-mode)
;;(require 'clojure-jump-to-file)
(add-hook 'clojure-mode-hook 'midje-mode)
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

;; yaml-mode info
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ruby
;;(add-hook 'rub-mode-hook 'git-gutter-mode)
(setq ruby-deep-indent-paren nil)
(setq ruby-indent-level 2)
;; ------------------------------
;; coffeescript
(add-hook 'coffee-mode-hook 'whitespace-mode)
(add-hook 'coffee-mode-hook 'flyspell-prog-mode)
