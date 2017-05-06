(defun my-dired-vinegar ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (find-alternate-file "..")
    (if (fboundp 'dired-jump)
	(dired-jump)
      (dired default-directory))))

(define-key evil-normal-state-map "-" 'my-dired-vinegar) 
(evil-define-key 'normal dired-mode-map "-" 'my-dired-vinegar) 

(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)))


;; Hide details by default
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Re-use the same buffer when opening a directory with <return>
(put 'dired-find-alternate-file 'disabled nil)


(provide 'my-mode-dired)
