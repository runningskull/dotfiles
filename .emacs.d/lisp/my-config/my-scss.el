(add-hook 'scss-mode-hook (lambda ()
                            (my-code-mode)
                            (evil-close-folds)))

(setq scss-compile-at-save nil)


(provide 'my-scss)
