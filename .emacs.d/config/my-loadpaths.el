(defun lpath/ (s)
  (concat "~/.emacs.d/" s))


;; Base load paths
(my/add-to-list 'load-path
  (lpath/ "config")
  (lpath/ "packages"))


;; Custom themes
(my/add-to-list 'custom-theme-load-path
  (lpath/ "packages/color-theme-mnml-light"))

(setq custom-theme-load-path
      (append custom-theme-load-path
              (list (lpath/ "packages/emacs-color-theme-solarized")
                    (lpath/ "packages/color-theme-base16")
                    (lpath/ "packages/color-theme-mnml-light")
                    "~/projects/_forked/smyx"
                    )))



(provide 'my-loadpaths)
