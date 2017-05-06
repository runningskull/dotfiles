
;; Other clojure file extensions
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))


(-whichkey-section- "Clojure/Cider" ",c")
(-whichkey-section- "Refactoring" ",cx")

(use-package clojure-mode
  :bind (:map evil-normal-state-map
	      (",c,c" . cider-connect)
	      (",c,j" . cider-jack-in)

	      ("\M-;" . comment-dwim-line)

	      ([tab] . paredit-reindent-defun)
	      (",-<TAB>" . paredit-reindent-defun)
	      ))


(provide 'my-lang-clojure)
