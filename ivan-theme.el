(deftheme ivan)


(let ((class '((class color) (min-colors 89)))
      (font-color "#eeeeee")
      (font-background "#212121")
      (hightlight-green "yellow green")
      (rainbow-1 "pale green")
      (rainbow-2 "#fa98fa")
      (error-color "salmon1"))


  (custom-theme-set-faces
   'ivan

   `(default ((,class (:foreground "#eeeeee" :background "#000000"))))

   `(cursor ((,class (:background "white"))))
   `(highlight ((,class (:foreground "white" :background "dark green"))))
   `(error ((,class (:foreground ,error-color))))
   `(warning ((,class (:foreground "orange"))))
   `(success ((,class (:foreground "yellow green"))))
   ;; Compilation faces
   `(compilation-mode-line-fail ((,class (:foreground "dark green"))))
   `(compilation-mode-line-run  ((,class (:foreground "dark goldenrod"))))
   `(compilation-mode-line-exit ((,class (:foreground "SpringGreen4"))))
   ;; Highlighting faces

   `(highlight ((,class (:foreground "black" :background "yellow green"))))
   `(region ((,class (:foreground "white" :background "dark green"))))
   `(secondary-selection ((,class (:background "dark slate gray"))))
   `(isearch ((,class (:foreground "white" :background "dark goldenrod"))))
   `(lazy-highlight ((,class (:background "gray25"))))

   `(font-lock-builtin-face ((,class (:foreground "LightSteelBlue"))))
   `(font-lock-comment-face ((,class (:foreground "SpringGreen3"))))
   `(font-lock-constant-face ((,class (:foreground "turquoise"))))
   `(font-lock-function-name-face ((,class (:foreground "pale green"))))
   `(font-lock-keyword-face ((,class (:foreground "white"))))
   `(font-lock-string-face ((,class (:foreground "dark khaki"))))
   `(font-lock-type-face ((,class (:foreground "aquamarine"))))
   `(font-lock-variable-name-face ((,class (:foreground "yellow green"))))
 
   ;; whitespace-mode


   `(whitespace-space ((,class (:background "black" :foreground "black"))))
   `(whitespace-hspace ((,class (:background "black" :foreground "black"))))
   `(whitespace-tab ((,class (:background "black" :foreground "black"))))
   `(whitespace-newline ((,class (:foreground "#3a3a3a"))))
   `(whitespace-trailing ((,class (:foreground "black" :background "black"))))
   `(whitespace-line ((,class (:background "salmon1" ))))
   `(whitespace-space-before-tab ((,class (:background "black" :foreground "black"))))
   `(whitespace-indentation ((,class (:background "black" :foreground "black"))))
   `(whitespace-empty ((,class (:background "black" :foreground "black"))))

   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,rainbow-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,rainbow-2))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,rainbow-2))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,rainbow-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,rainbow-2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,rainbow-1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground  ,rainbow-2))))


   ;; flymake
   
   `(flymake-errline ((,class (:foreground "salmon1" :underline t))))

   
))

(provide-theme 'ivan)
