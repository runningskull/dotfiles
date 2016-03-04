(require 'python)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(defun python-keymaps ()
  (define-key evil-normal-state-map ",xf" 'python-shell-send-defun)
  (define-key evil-normal-state-map ",xb" 'python-shell-send-buffer)
  (define-key evil-visual-state-map ",x," 'python-shell-send-region))

(add-hook 'python-mode-hook 'python-keymaps)

(provide 'my-python)
