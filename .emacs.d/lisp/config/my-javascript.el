(require 'js-comint)

(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

;; REPL
(setq inferior-js-program-command "node")

(setq inferior-js-mode-hook
      (lambda ()
        (ansi-color-for-comint-mode-on)
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string "\033\\[[0-9]+[GJK]" "" output)))))

(eval-after-load 'js3-mode
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
          (setq js-indent-level 2)
          (setq js3-lazy-commas t)
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js3-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "\u0192")
                                  nil)))))))

(provide 'my-javascript)
