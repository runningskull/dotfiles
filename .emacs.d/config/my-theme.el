(defun color/dark ()
  (interactive)
  (load-theme 'smyx)
  (disable-theme 'mnml-light))

(defun color/light ()
  (interactive)
  (load-theme 'mnml-light)
  (disable-theme 'smyx))


;; Default color theme
;;(load-theme 'mnml-light t)
;; now set in my-gui.el

;; Default font
(set-default-font "Source Code Pro-12")


(provide 'my-theme)
