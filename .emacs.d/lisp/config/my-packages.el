(defun bind-evil-key (modes key f)
  (let ((key (if f key mode))
        (modes (if f modes '(normal insert visual)))
        (f (if f f nil))) 
    (dolist (m mode)
      (define-key (make-symbol (concat "evil-" m "-state-map"))
                  key f))))

