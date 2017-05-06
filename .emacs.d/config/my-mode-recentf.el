(defun ido-recentf-open ()
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(use-package recentf
  :init 
  (progn
    (setq recentf-max-menu-items 250))
  :config 
  (progn 
    (add-to-list 'recentf-exclude "elpa")
    (add-to-list 'recentf-exclude ".ido.last")
    (recentf-mode 1)))


(provide 'my-mode-recentf)

