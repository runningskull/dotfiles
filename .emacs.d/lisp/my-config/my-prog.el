(add-hook 'prog-mode-hook (lambda ()
			    (imenu-add-menubar-index)
			    (hs-minor-mode t)
			    ;(projectile-on)
			    (auto-complete-mode)))

;; evil keymaps
(defun evil-paredit-keymaps ()
  (define-key evil-normal-state-map ",(" 'paredit-wrap-round)
  (define-key evil-normal-state-map ",)" 'paredit-wrap-round)
  (define-key evil-normal-state-map ",[" 'paredit-wrap-square)
  (define-key evil-normal-state-map ",]" 'paredit-wrap-square)
  (define-key evil-normal-state-map ",{" 'paredit-wrap-curly)
  (define-key evil-normal-state-map ",}" 'paredit-wrap-curly)
  (define-key evil-visual-state-map ",(" 'paredit-wrap-round)
  (define-key evil-visual-state-map ",)" 'paredit-wrap-round)
  (define-key evil-visual-state-map ",[" 'paredit-wrap-square)
  (define-key evil-visual-state-map ",]" 'paredit-wrap-square)
  (define-key evil-visual-state-map ",{" 'paredit-wrap-curly)
  (define-key evil-visual-state-map ",}" 'paredit-wrap-curly)

  (define-key evil-normal-state-map ",+" 'paredit-split-sexp)
  (define-key evil-normal-state-map ",-" 'paredit-join-sexps)
  (define-key evil-normal-state-map "<" 'paredit-backward)
  (define-key evil-normal-state-map ">" 'paredit-forward)

  (define-key evil-normal-state-map (kbd "M->") 'paredit-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "M-<") 'paredit-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd "C-<") 'paredit-backward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "C->") 'paredit-backward-barf-sexp)

  (define-key evil-insert-state-map (kbd "M->") 'paredit-forward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "M-<") 'paredit-forward-barf-sexp)
  (define-key evil-insert-state-map (kbd "C->") 'paredit-backward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "C-<") 'paredit-backward-barf-sexp)

  ;; combined w/ iterm mapping, allows use of meta in terminal
  (define-key evil-normal-state-map (kbd ",,>") 'paredit-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd ",,<") 'paredit-forward-barf-sexp)
  )

;; Start with folds closed
; (add-hook 'prog-mode-hook 'evil-close-folds)

(defun enable-paredit ()
  (paredit-mode)
  (evil-paredit-keymaps))

;; enable paredit
(progn
  (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'enable-paredit)))



(provide 'my-prog)
