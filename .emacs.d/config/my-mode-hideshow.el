(defun enable-and-fold ()
  (interactive)
  (hideshowvis-enable)
  (evil-close-folds))

(use-package hideshow
  :defer t
  :bind (:map evil-normal-state-map
	 ("zm" . enable-and-fold))
  :diminish hideshow-mode)

(use-package hideshowvis
  :commands hideshowvis-enable
  :config (hideshowvis-symbols))



(provide 'my-mode-hideshow)
