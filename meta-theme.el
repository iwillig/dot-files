(deftheme meta
  "A theme for working all day")

(custom-theme-set-faces
 'meta
 '(default ((t (:background "#1D1D1D"  :foreground "#eee" ))))
 '(cursor  ((t (:foreground "#fff"))))



 '(font-lock-comment-face ((t (:foreground "#61635e")))) 
 '(font-lock-comment-delimiter-face ((t (:foreground "#61635e")))) ;
 '(font-lock-doc-face ((t (:foreground "#77507b"))))

 '(font-lock-doc-string-face ((t (:foreground "#77507b"))))
 '(font-lock-string-face ((t (:foreground "#21C0FF"))))
 '(font-lock-keyword-face ((t (:foreground "#729fcf"))))
 '(font-lock-builtin-face ((t (:foreground "#4CD6FF"))))
 '(font-lock-function-name-face ((t (:foreground "#c4a000"))))
 '(font-lock-variable-name-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#4e9a06"))))
 '(font-lock-type-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))


 
 '(highlight  ((t (:background "#898989" :foreground "#000"))))

 '(flycheck-error ())
 
 )

(provide-theme 'meta)
