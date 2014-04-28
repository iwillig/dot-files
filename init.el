;; #############################################
;; Ivan Willig's emacs config file for emacs 24
;; Includes supports working in
;;    Javascript
;;    Python
;;    Clojure
;;    TypeScript
;;    CoffeScript
;; Requires package.el, emacs package system
;;
;; #############################################
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'custom-theme-load-path  "~/.emacs.d/")



;; some global settings
(prefer-coding-system 'utf-8)
(show-paren-mode t)
(windmove-default-keybindings)
(auto-fill-mode -1)

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
                    ;; clojure stuff
                    clojure-mode
                    midje-mode
                    clojure-test-mode

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

                    powerline

                    haml-mode
                    geiser
                    rainbow-delimiters
                    rainbow-mode
                    scheme-complete
                    scala-mode
                    haskell-mode
                    ;; terminal stuff

                    multi-term
                    coffee-mode
                    autopair
                    cider
                    nrepl
                    parscope
                    scss-mode
                    yaml-mode
                    auto-complete
                    markdown-mode
                    scss-mode
                    sass-mode
                    flymake-sass

                    paredit
                    js2-mode
                    fringe-helper
                    golden-ratio
                    sublime-themes
                    cyberpunk-theme
                    ;; ;; themes
                    color-theme-sanityinc-tomorrow
                    zenburn-theme
                    assemblage-theme
                    solarized-theme
                    sunny-day-theme
                    helm
                    git-gutter-fringe)))

    (dolist (p packages)
      (when (not (package-installed-p p))
        (message "Installing package %s" p)
        (package-install p)))))

(install-packages)
(add-to-list 'load-path "~/.emacs.d/")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;(load-theme 'meta t)
(load-theme 'zenburn t)
;;(load-theme 'flatui t)

(require 'highlight-sexp)


(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)



(require 'powerline)
(powerline-default-theme)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
;;
;; golden resizes the windows to maxiumize
;;(require 'golden-ratio)
;;(golden-ratio-mode 1)

(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)


;; git gutter mode
(global-git-gutter-mode t)

;; git-gutter-fringe
(require 'git-gutter-fringe)
(add-hook 'prog-mode-hook 'git-gutter-mode)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; raindow mode makes emacs display the color of a hex value in the dasdasdasdad
;;; background of the test. Its useful for editing css files
(add-hook 'prog-mode-hook 'rainbow-mode)

;; set up whitespace mode and enable it globally
;; (require 'whitespace)

;; (setq whitespace-line-column 120)

;; (setq whitespace-style '(face
;;         tabs
;;         spaces
;;         trailing
;;         newline
;;         lines-tail
;;         ;; newline-mark
;; ;;        empty
;;         space-before-tab
;;         space-after-tab))

;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; (set-face-attribute 'whitespace-space  nil
;;                     :background "#0A0A0A"
;;                     :weight 'bold)

;; (set-face-attribute 'whitespace-line nil
;;                     :background "#0A0A0A"
;;                     :weight 'bold)

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
;; javascript mode

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))



;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(setq js2-indent-level 4)

(add-hook 'js2-mode-hook (lambda () (paredit-mode -1)))
(add-hook 'js2-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js2-mode-hook (lambda () (turn-off-auto-fill)))

(add-hook
 'js2-mode-hook
 (lambda ()
   (imenu-add-menubar-index)
   (hs-minor-mode t)))

(global-set-key (kbd "C-c d") 'hs-show-block)
(global-set-key (kbd "C-c M-d") 'hs-show-all)
(global-set-key (kbd "C-c s") 'hs-hide-block)
(global-set-key (kbd "C-c M-s") 'hs-hide-all)

(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))

;; ----------------------------------------
;; tern stuff

(add-to-list 'load-path "~/opt/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (flymake-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


'(add 1 1 1 1)
;; add(1, 2, 3, 4);


;; flymake mode
(when (load "flymake" t)

  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/jschecker" (list local-file))))


  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pycheckers"  (list local-file))))


  (setq flymake-err-line-patterns
        (cons '("^\\(.*\\)(\\([[:digit:]]+\\)):\\(.*\\)$"
                1 2 nil 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.js\\'" flymake-jslint-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init)))

;; allow more then one error on a line
(setq flymake-number-of-errors-to-display 4)
;; set the logging level to really high

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
  (defroutes 'defun)
  (for-all   'defun)
  (fact      'defun)
  (facts     'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  (describe 'defun)
  (context 'defun)
  (it 2)
  (should 2))

;; ####################
;; configure midje
(require 'midje-mode)
(require 'clojure-jump-to-file)
(add-hook 'clojure-mode-hook 'midje-mode)
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
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'rub-mode-hook 'git-gutter-mode)
(setq ruby-deep-indent-paren nil)
(setq ruby-indent-level 2)
;; ------------------------------
;; coffeescript
(require 'coffee-mode)
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook 'whitespace-mode)
(add-hook 'coffee-mode-hook 'flyspell-prog-mode)
(add-hook 'coffee-mode-hook 'git-gutter-mode)

;; (require 'flymake-coffee)
;; (add-hook 'coffee-mode-hook 'flymake-coffee-load)

;; ------------------------------
;; typescript
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-hook 'typescript-mode-hook 'whitespace-mode)
(add-hook 'typescript-mode-hook  'rainbow-delimiters-mode)

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
         "#socialplanning"
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
       :nick "iwillig"
       :full-name "Ivan Willig"))
