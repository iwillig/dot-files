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

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)


(package-initialize)


;; some global settings
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)

(setq inhibit-splash-screen t)
(add-to-list 'load-path "~/.emacs.d/")
(global-font-lock-mode 1)
(put 'downcase-region 'disabled nil)
(setq-default indent-tabs-mode nil) ;; don't use tabs
(global-linum-mode 1) ;; i like line numbers
(global-auto-revert-mode t)
(global-set-key (kbd "C-x i") 'indent-region)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ----------------------------------------
;; safely install all of the packages
(defun install-packages ()
  (let ((packages '(magit
                    python-mode
                    clojure-mode
                    starter-kit
                    starter-kit-lisp
                    starter-kit-bindings
                    starter-kit-ruby
                    flymake-easy
                    flymake-ruby
                    flymake-coffee
                    flymake-cursor
                    flymake-jshint
                    flymake-haml
                    haml-mode
                    geiser
                    rainbow-delimiters
                    rainbow-mode
                    scheme-complete
                    scala-mode
                    zenburn-theme
                    coffee-mode
                    autopair
                    nrepl
                    scss-mode
                    yaml-mode
                    auto-complete
                    markdown-mode
                    paredit
                    js2-mode)))

    (dolist (p packages)
      (when (not (package-installed-p p))
        (message "Installing package %s" p)
        (package-install p)))))

(install-packages)
(add-to-list 'load-path "~/.emacs.d/")

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)



;; night theme
;;(load-theme 'cyberpunk t)

(add-to-list 'custom-theme-load-path  "~/.emacs.d/noctilux-theme/")
(load-theme 'noctilux t)

;; day theme
;; (load-theme 'adwaita)

;;(require 'rust-mode)
;;(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; raindow mode makes emacs display the color of a hex value in the
;;; background of the test. Its useful for editing css files
(add-hook 'prog-mode-hook 'rainbow-mode)

;;(set-default-font "terminus")
;;(set-default-font "Liberation Mono 10")
;;(setq tab-width 4)

;; set up whitespace mode and enable it globally
(require 'whitespace)

(setq whitespace-line-column 80
       whitespace-style '(face tabs newline space tab-mark newline-mark))

(add-hook 'prog-mode-hook 'whitespace-mode)


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

;; ---------------------------------------
;; scss mode

(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; ----------------------------------------
;; python mode
(require 'python-mode)
;; make sure we never use tabs... ever.
(setq tab-width 4)


(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (hs-minor-mode t)))

;; ----------------------------------------
;; javascript mode

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(setq js-indent-level 4)

(add-hook 'js-mode-hook (lambda () (paredit-mode -1)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))


(add-hook
 'js-mode-hook
 (lambda ()
   (imenu-add-menubar-index)
   (hs-minor-mode t)))

(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))

;; ----------------------------------------
;; tern stuff

(add-to-list 'load-path "~/opt/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))



;; flymake mode
(when (load "flymake" t)

  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
;;      (list "/usr/local/bin/jslint" (list "--terse" local-file))
      (list "/Users/ivan.willig/.emacs.d/jschecker" (list local-file))))


  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "/Users/ivan.willig/.emacs.d/pycheckers"  (list local-file))))

  
  (setq flymake-err-line-patterns
        (cons '("^\\(.*\\)(\\([[:digit:]]+\\)):\\(.*\\)$"
                1 2 nil 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.js\\'" flymake-jslint-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))

;; allow more then one error on a line
(setq flymake-number-of-errors-to-display 4)
;; set the logging level to really high
;;(setq flymake-log-level 3)

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

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; nrepl stuff

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

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

;; ------------------------------
;; ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;; ------------------------------
;; coffeescript
(require 'coffee-mode)
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook 'whitespace-mode)
(add-hook 'coffee-mode-hook 'flyspell-prog-mode)

(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)


;; ------------------------------
;; IRC stuff

(require 'erc)
(require 'erc-join)
(erc-autojoin-mode 1)
(erc-spelling-mode 1) ;; I am an awful speller
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#intentmedia"
         "#opengeo"
         "#geonode"
         "#modilabs"
         "#craftyplans"
         "#clojure")))

(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

(setq erc-keywords '("ivan" "beer")) ;; 

(defun irc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
       :nick "iwillig" :password "****"
       :full-name "Ivan Willig"))
