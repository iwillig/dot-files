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
  (dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(setq inhibit-splash-screen t)

;; packages
(defvar packages 
  (quote (haskell-mode
		  ido-ubiquitous
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
		:family "Mono")))))


;; ido mode stuff
(require 'ido)
(ido-mode t)


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
;;; flymake mode seems to kill the haskell mode
;;; not sure why
(add-hook 'haskell-mode-hook (lambda () (flymake-mode -1)))
;; end haskell config

;; python config

(add-hook 'python-mode-hook (lambda () (paredit-mode +1)))
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

;; make .mako files act like .html files
(setq auto-mode-alist---
      (append '(("\\.mako$" . html-mode)) auto-mode-alist))


;; end of python config


;; magit config
(global-set-key (kbd "C-c g") 'magit-status)


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

(global-set-key (kbd "C-c t") 'insert-time)

