
(defun css-electric-pair ()
  (interactive)
  (electric-pair-mode))

(add-hook 'css-mode-hook 'css-electric-pair)

(provide 'my-css)
