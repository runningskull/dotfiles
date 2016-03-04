(add-hook 'prog-mode-hook (lambda ()
			    (imenu-add-menubar-index)
			    (hs-minor-mode t)
                                        ;(projectile-on)
			    ))


;; Extra paredit utils

(defun my-paredit-convert-to (open-fn)
  "Change ie. ( | ) to [ | ].  - where | is point position."
  (let ((pos (point)))
    (interactive)
    (paredit-backward-up)
    (funcall open-fn 1)
    ;;(paredit-forward-slurp-sexp)
    (right-char 1)
    (paredit-splice-sexp)
    (goto-char pos)))


(defun paredit-convert-to-round ()
  (interactive)
  (my-paredit-convert-to 'paredit-open-round))

(defun paredit-convert-to-square ()
  (interactive)
  (my-paredit-convert-to 'paredit-open-square))

(defun paredit-convert-to-curly ()
  (interactive)
  (my-paredit-convert-to 'paredit-open-curly))

(defun paredit-convert-to-angled ()
  (interactive)
  (my-paredit-convert-to 'paredit-open-angled))


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

  (define-key evil-normal-state-map ",,(" 'paredit-convert-to-round)
  (define-key evil-normal-state-map ",,[" 'paredit-convert-to-square)
  (define-key evil-normal-state-map ",,{" 'paredit-convert-to-curly)
  (define-key evil-normal-state-map ",,<" 'paredit-convert-to-angled)

  (define-key evil-normal-state-map ",/" 'paredit-split-sexp)
  (define-key evil-normal-state-map "<" 'paredit-backward)
  (define-key evil-normal-state-map ">" 'paredit-forward)

  (define-key evil-normal-state-map (kbd ",:") 'transpose-sexps)

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
(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'enable-paredit))



(provide 'my-prog)
