;; Init
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;; Settings
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


(provide 'my-history)
