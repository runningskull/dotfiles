;; Set emacs customization file.
;; We can't use this file or emacs will complain about a circular reference.
(setq custom-file "~/.emacs.d/config/my-customizations--emacs.el")
(load custom-file t)


;;;;
;;;; Config for emacs built-in features
;;;;

;; testing this
(setq scroll-preserve-screen-position 'always)

;; for dired
(setq insert-directory-program (executable-find "gls"))

(setq save-abbrevs nil)

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun zilongshanren/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'zilongshanren/stop-using-minibuffer)
