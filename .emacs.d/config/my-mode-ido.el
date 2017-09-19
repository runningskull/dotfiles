(defun bind-ido-keys ()
  (interactive)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match))


(use-package ido-vertical-mode
  :config
  (progn
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))

(use-package ido-everywhere
  :init   (ido-everywhere 1))

(use-package flx-ido
  :config
  (progn
    (add-hook 'ido-setup-hook 'bind-ido-keys)
    (flx-ido-mode 1)))

(provide 'my-mode-ido)
