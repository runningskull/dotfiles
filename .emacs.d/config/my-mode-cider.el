
;;;; Utils

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


;;;; Light wrappers to customize behavior 

(defun cider-set-ns+load-buffer (&optional arg)
  (interactive)
  (cider-repl-set-ns (cider-current-ns))
  (cider-load-buffer))

(defun cider-insert-defun-in-repl+eval (&optional arg)
  (interactive)
  (cider-eval-defun-at-point))

(defun cider-insert-defun-in-repl+eval+jump (&optional arg)
  (interactive)
  (cider-insert-defun-in-repl t)
  (evil-insert-state))

(defun cider-insert-last-sexp-in-repl+eval+jump (&optional arg)
  (interactive)
  (cider-insert-last-sexp-in-repl t)
  (evil-insert-state))

(defun cider-switch-to-repl-buffer+insert (&optional arg)
  (interactive)
  (cider-switch-to-repl-buffer)
  (evil-insert-state))

(defun cider-switch-to-repl-buffer+ns+insert (&optional arg)
  (interactive)
  (cider-switch-to-repl-buffer t)
  (evil-insert-state))

(defun cider-quit--all (&optional arg)
  (interactive)
  (cider-quit t))


;;;; Modeline

(defun my--cider--modeline-info ()
  "Return info for the `cider-mode' modeline.
Info contains project name and host:port endpoint."
  (-if-let (current-connection (ignore-errors (cider-current-connection)))
      (with-current-buffer current-connection
        (concat
         (when cider-repl-type
           (concat cider-repl-type ":"))
         (when cider-mode-line-show-connection
           (format "%s" (pcase (car nrepl-endpoint)
                          ("localhost" "")
                          (x x))))))
    "norepl")) 

(defun my-cider-repl-prompt (ns)
  (concat "
" ns "
❱ "))


(-whichkey-section- "REPL" ",c,")
(-whichkey-section- "Quit" ",c,q")
(-whichkey-section- "Eval" ",ce")
(-whichkey-section- "Tests" ",ct")
(-whichkey-section- "Macroexpansion" ",cm")



;;;; Setup

(use-package cider
  :bind (:map evil-normal-state-map

         (",cn" . cider-set-ns+load-buffer)
         (",cN" . cider-repl-set-ns)
         (",cc" . cider-insert-defun-in-repl+eval+jump)
         (",cC" . cider-insert-defun-in-repl+eval)

         (",c,," . cider-switch-to-repl-buffer+insert)
         (",c,n" . cider-switch-to-repl-buffer+ns+insert)

	 (",c;" . cider-refresh)

         (",c,qq" . cider-quit)
         (",c,qa" . cider-quit--all)

         (["C-<up>"]   . cider-repl-previous-input)
         (["C-k"]      . cider-repl-previous-input)
         (["C-<down>"] . cider-repl-forward-input)
         (["C-j"]      . cider-repl-forward-input)

         (",cee" . cider-eval-defun-at-point)

         (",ctt" . cider-test-run-test)
	 (",ctn" . cider-test-run-ns-tests)
	 (",ctl" . cider-test-run-loaded-tests)
	 (",ctp" . cider-test-run-project-tests)

         (",ch" . cider-doc)

         (",cmm" .  cider-macroexpand-1)
         (",cma" .  cider-macroexpand-all)
         (",cm,m" . cider-macroexpand-1-inplace)
         (",cm,a" . cider-macroexpand-all-inplace)

         ("\M-." . cider-find-var)
         ("\M-," . cider-pop-back)

         ;; override cider defaults
         ("M-h" . evil-window-left)
         ("M-l" . evil-window-right)
         )
  :config
  (progn
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-pop-to-buffer-on-connect 'display-only)
    (setq cider-repl-display-in-current-window nil)
    (setq cider-repl-shortcut-dispatch-char ?\\)
    (setq cider-mode-line '(:eval (format " $%s" (my--cider--modeline-info))))
    (setq cider-repl-prompt-function 'my-cider-repl-prompt)))


;; CLJS autocompletion is slow & hinders typing, so don't try to do it.
;; Instead, in CLJS buffers, let <TAB> be a shortcut to autocomplete.
(defun cljs-cider-no-autocomplete ()
  (make-local-variable 'company-idle-delay)
  (setq company-idle-delay nil)
  (define-key evil-insert-state-map (kbd "TAB") #'company-indent-or-complete-common))
(add-hook 'clojurescript-mode-hook 'cljs-cider-no-autocomplete)


;; Paredit in mah repl
(add-hook 'cider-repl-mode-hook 'paredit-mode)




(provide 'my-mode-cider)
