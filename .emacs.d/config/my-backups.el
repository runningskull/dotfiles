;; All backup files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Disk space is cheap
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))



(provide 'my-backups)
