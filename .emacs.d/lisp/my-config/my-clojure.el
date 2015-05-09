(defun set-inferior-lisp-clojure ()
  (setq inferior-lisp-program "lein repl"))

(defun compojure-indents ()
  ;; nicer indentation for compojure routes
  (define-clojure-indent
    ; compojure
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)

    ; clj-api
    (GET* 2)
    (POST* 2)
    (PUT* 2)
    (DELETE* 2)
    (HEAD* 2)
    (ANY* 2)
    (middlewares 'defun)
    (defroutes* 'defun)
    (defapi 'defun)
    (swagger-ui 'defun)
    (swagger-docs 'defun)
    (swaggered 'defun)
    
    ; rafl stuff
    (add-id 'defun)))

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

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(defun PRE-cider-insert-defun-in-repl (&optional arg)
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-insert-defun-in-repl)))

(defun PRE-cider-insert-last-sexp-in-repl (&optional arg)
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-insert-defun-in-repl)))

(defun evil-cider-keymaps ()
  (define-key evil-normal-state-map ",cb" 'cider-load-buffer)
  (define-key evil-normal-state-map ",cn" 'cider-repl-set-ns)
  (define-key evil-normal-state-map ",cc" 'PRE-cider-insert-defun-in-repl)
  (define-key evil-normal-state-map ",c," 'PRE-cider-insert-last-sexp-in-repl)

  (define-key evil-normal-state-map ",ct" 'cider-test-run-tests)
  (define-key evil-normal-state-map ",cr" 'cider-namespace-refresh)
  (define-key evil-normal-state-map ",c " 'cider-repl-clear-buffer)

  (define-key evil-normal-state-map ",chh" 'cider-grimoire)
  (define-key evil-normal-state-map ",chw" 'cider-grimoire-web)

  (define-key evil-normal-state-map ",cmm" 'cider-macroexpand-1)
  (define-key evil-normal-state-map ",cma" 'cider-macroexpand-all)

  (define-key evil-normal-state-map "\M-." 'cider-jump-to-var)
  (define-key evil-normal-state-map "\M-," 'cider-jump-back)
  (define-key evil-insert-state-map "\M-h" 'evil-window-left)
  (define-key evil-insert-state-map "\M-l" 'evil-window-right)

  (define-key evil-insert-state-map (kbd "C-<up>") 'cider-repl-previous-input)
  (define-key evil-insert-state-map (kbd "C-<down>") 'cider-repl-forward-input)
  )

(defun evil-clojure-keymaps ()
  (define-key evil-normal-state-map "\M-;" 'comment-dwim-line)
  )

(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'false)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'evil-cider-keymaps)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-clojure-keymaps)
(add-hook 'clojure-mode-hook 'set-inferior-lisp-clojure)
(add-hook 'clojure-mode-hook 'compojure-indents)

(provide 'my-clojure)

(evil-cider-keymaps)
