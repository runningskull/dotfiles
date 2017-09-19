(defun enable-and-fold ()
  (interactive)
  (hideshowvis-enable)
  (evil-close-folds))

(use-package hideshow
  :defer t
  :init (setq hs-hide-comments-when-hiding-all nil)
  :bind (:map evil-normal-state-map
	      ("zm" . enable-and-fold)
	      ("zl" . hs-hide-level))
  :diminish hideshow-mode)

(use-package hideshowvis
  :commands hideshowvis-enable
  :config (hideshowvis-symbols))



(provide 'my-mode-hideshow)
