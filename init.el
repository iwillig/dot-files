;;; Ivan Willig Tue 11 Oct 2011 08:00:46 PM EDT
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(prefer-coding-system 'utf-8)

(show-paren-mode t)

;; only works on trunk version of emacs(
(load-theme 'wombat)

;; taken from the emacs starter kit 
;; thanks phil
;; ------------------------------

(when (not package-archive-contents)
  (package-refresh-contents))
(progn
  (dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(setq inhibit-splash-screen t)

(windmove-default-keybindings)

(global-set-key 
 (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one

(global-set-key
 (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
;; ------------------------------
;; end of emacs starter kit


;; packages
(defvar packages 
  (quote (haskell-mode
          scala-mode
          ipython
          python-mode
          yasnippet
          yaml-mode
          slime
          slime-repl
          clojure-mode
          markdown-mode
          ido-ubiquitous
          magit
          paredit)))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Liberation Mono")))))


;; ido mode stuff
(require 'ido)
(ido-mode t)

;; yaml mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; text mode
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
;; rst mode
(add-hook 'rst-adjust-hook (lambda () (flyspell-mode)))

;; start of emacs lisp config
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; end of emacs lisp stuff

;; haskell config
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;; flymake mode seems to kill the haskell mode
;;; not sure why
(add-hook 'haskell-mode-hook (lambda () (flymake-mode -1)))
;; end haskell config

;; python config
(require 'python-mode)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(defun jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/home/ivan/.emacs.d/jscheckers" (list local-file))))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/home/ivan/.emacs.d/lintrunner.py" (list local-file))))



(when (load "flymake" t)
  (setq flymake-err-line-patterns
        (cons '("Error:\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
                nil 1 2 3)
              flymake-err-line-patterns))

  ;; (add-to-list 'flymake-allowed-file-name-masks
  ;;              '("\\.js\\'" jslint-init))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
;; end of python config


;; javascript
;; start of javascript mode
(eval-after-load 'js
  '(progn (setq js-indent-level 4)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; end of js mode

;;

;; magit config
(global-set-key (kbd "C-c g") 'magit-status)

;; slime and clojure stuff for lisp development.
;; taken from 
;; http://riddell.us/ClojureSwankLeiningenWithEmacsOnLinux.html

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (flyspell-prog-mode)))

(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))	
	(defun paredit-mode-enable () (paredit-mode 1))	
	(add-hook 'slime-mode-hook 'paredit-mode-enable)	
	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
	(setq slime-protocol-version 'ignore)))

(require 'slime)
(slime-setup)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun)))

;; custom functions

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)
(add-hook 'after-make-frame-functions 'toggle-fullscreen)
 
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; (global-set-key (kbd "C-c t") 'insert-time)

(require 'org-install)
(require 'yaml-mode)

(require 'scheme)

;; for scheme mode
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(load-file "/home/ivan/.emacs.d/geiser/elisp/geiser-load.el")
(put 'downcase-region 'disabled nil)

(require 'erc)

(require 'erc-join)
(erc-autojoin-mode 1)


(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#opengeo" "#geonode"
         "#socialplanning"
         "#mapstory" "#geotools" "#geoserver"
         "#clojure" "#modilabs" "#zco" )))


(defun irc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
       :nick "iwillig" :password "<newyorkpass>"
       :full-name "Ivan Willig"))

(setq-default show-trailing-whitespace t)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" default))))
