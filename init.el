;;; Ivan Willig Tue 11 Oct 2011 08:00:46 PM EDT
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; only works on trunk version of emacs
(load-theme 'wombat)

;; taken from the emacs starter kit 
;; thanks phil
;; ------------------------------

(when (not package-archive-contents)
  (package-refresh-contents))

(progn
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; packages
(defvar packages 
  (quote (haskell-mode
		  magit
		  paredit)))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

(windmove-default-keybindings)

(global-set-key 
 (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one

(global-set-key
 (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; ------------------------------
;; end of emacs starter kit

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil 
		:inverse-video nil 
		:box nil 
		:strike-through nil 
		:overline nil 
		:underline nil 
		:slant normal 
		:weight normal 
		:height 98 
		:width normal 
		:foundry "unknown" 
		:family "Liberation Mono")))))

(ido-mode t)
(setq inhibit-splash-screen t)

(defun load-paredit ()
  (lambda () (paredit-mode +1)))

;; start of emacs lisp config
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; end of emacs lisp stuff

;; start of javascript mode

(add-hook 'js-mode-hook (lambda () (paredit-mode +1)))
(setq js-indent-level 2)
;; end of js mode

;; haskell config

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; end haskell config

;; python config

(setq tab-width 4)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/ivan/.emacs.d/pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
                            '("\\.py\\'" flymake-pyflakes-init)))
(setq python-python-command "ipython")
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; end of python config


;; magit config
(global-set-key (kbd "C-c g") 'magit-status)


;; custom functions
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%c" (current-time))))


