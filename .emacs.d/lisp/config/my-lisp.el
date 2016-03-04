;; Scheme
(setq scheme-program-name
      "/Applications/mit-scheme/Contents/Resources/mit-scheme --emacs")
(require 'xscheme)

(defun my-scheme-keymaps ()
  (define-key evil-normal-state-map ",xb" 'xscheme-send-buffer)
  (define-key evil-normal-state-map ",xd" 'xscheme-send-definition)
  (define-key evil-normal-state-map ",xx" 'xscheme-send-previous-expression)
  (define-key evil-visual-state-map ",xx" 'xscheme-send-region))

(add-hook 'scheme-mode-hook 'my-scheme-keymaps)


;; Common Lisp
(defun set-inferior-lisp-sbcl ()
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(add-hook 'lisp-mode-hook 'set-inferior-lisp-sbcl)
;; (add-hook 'lisp-mode-hook 'global-aggressive-indent-mode)
;; (add-hook 'clojure-mode-hook 'global-aggressive-indent-mode)


(provide 'my-lisp)
