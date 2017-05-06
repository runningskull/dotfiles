

(use-package geiser
  :bind (:map evil-normal-state-map
	      (["C-k"] . comint-previous-matching-input-from-input)
	      (["C-j"] . comint-next-matching-input-from-input)

	      (",r,," . geiser-eval-definition-and-go)
	      (",r,r" . geiser-eval-buffer-and-go)
	      ))
