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


(use-package paredit
  :defer t
  :diminish paredit-mode
  :bind (:map evil-normal-state-map
	 ("TAB" . paredit-reindent-defun))
  :init
  (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
	      'paredit-mode)))

(provide 'my-mode-paredit)

