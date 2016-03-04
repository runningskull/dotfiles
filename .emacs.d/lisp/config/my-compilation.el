(defun flash-screen ()
  (invert-face 'default)
  (invert-face 'default))

(defun compile-autoclose (buffer string)
  (with-current-buffer buffer
    (cond ((and
	    (string-match "finished" string)
	    (not (search-backward "Ag started" nil t)))
	   (bury-buffer "*compilation*")
	   (winner-undo)
	   (message "Build successful."))
	  ((search-forward "Ag started" nil t)
	   (message ""))
	  (t                                                                    
	   (message "Compilation exited abnormally: %s" string)))))

(winner-mode)
(setq compilation-finish-functions 'compile-autoclose)


(provide 'my-compilation)
