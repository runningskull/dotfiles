(defun my/add-to-list (list-var &rest elements)
  "Append ELEMENTS to the end of LIST-VAR. Returns the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))


;; Set nicer indentation
(put 'my/add-to-list 'lisp-indent-function 'defun)


;; Nice which-key display
(defmacro -whichkey-section- (description prefix)
  `(which-key-add-key-based-replacements ,prefix ,description))


(provide 'my-elisp-utils)
