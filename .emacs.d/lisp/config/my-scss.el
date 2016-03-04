(add-hook 'scss-mode-hook (lambda ()
                            (evil-close-folds)))

(setq scss-compile-at-save nil)


(provide 'my-scss)
