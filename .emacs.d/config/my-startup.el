(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))



(provide 'my-startup)
