(defun set-inferior-lisp-clojure ()
  (setq inferior-lisp-program "lein repl"))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun evil-cider-keymaps ()
  (define-key evil-normal-state-map ",xb" 'cider-load-current-buffer)
  (define-key evil-normal-state-map ",xn" 'cider-repl-set-ns)
  (define-key evil-normal-state-map ",x," 'cider-insert-last-sexp-in-repl)

  (define-key evil-normal-state-map ",mm" 'cider-macroexpand-1)
  (define-key evil-normal-state-map ",mf" 'cider-macroexpand)

  (define-key evil-normal-state-map "\M-." 'cider-jump)
  (define-key evil-insert-state-map "\M-h" 'evil-window-left)
  (define-key evil-insert-state-map "\M-l" 'evil-window-right)

  (define-key evil-insert-state-map (kbd "C-<up>") 'cider-repl-previous-input)
  (define-key evil-insert-state-map (kbd "C-<down>") 'cider-repl-forward-input)
  )

(defun evil-clojure-keymaps ()
  (define-key evil-normal-state-map "\M-;" 'comment-dwim-line)
  )


(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'evil-cider-keymaps)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-clojure-keymaps)
(add-hook 'clojure-mode-hook 'set-inferior-lisp-clojure)

(provide 'my-clojure)

(evil-cider-keymaps)
