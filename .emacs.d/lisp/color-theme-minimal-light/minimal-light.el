(deftheme mnml-light)

(let ((background "#fafafa")
      (folded "#aaa")
      (comment "#777")
      (region "#b6d6fd")
      (cursor "#444"))

  (custom-theme-set-faces 
   'mnml-light

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background))))
   `(cursor ((t (:background ,cursor :foreground ,cursor))))

   `(region ((t (:foreground ,cursor :background ,region))))

   ;; Comments
   `(font-lock-comment-face ((t (:foreground ,comment :slant oblique))))
   `(font-lock-comment-delimeter-face ((t (:foreground ,comment :slant oblique))))
   ;; hideshow-vis
   `(hs-face ((t (:background ,background :foreground ,folded :slant oblique :box (:line-width 1 :color ,folded)))))))

(provide-theme 'mnml-light)
