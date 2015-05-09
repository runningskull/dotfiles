(defun evil-slime-keymaps ()
  (define-key evil-normal-state-map ",x," 'slime-compile-defun)
  (define-key evil-normal-state-map ",xd" 'slime-eval-defun)
  (define-key evil-normal-state-map ",x;" 'slime-eval-last-expression-in-repl)

  (define-key evil-normal-state-map ",mm" 'slime-expand-1)
  (define-key evil-normal-state-map ",mm" 'slime-expand-1)
  )

; todo: is this needed?
; (require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
     (slime-setup '(
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-repl
                    slime-scratch))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (evil-slime-keymaps)))

(provide 'my-slime)
