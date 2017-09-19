;; Where to find packages
(setq package-archives
      '(("melpa". "http://melpa.milkbox.net/packages/")
        ("ELPA" . "http://tromey.com/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))


;; If new installation, load package repo contents
(when (not package-archive-contents)
  (package-refresh-contents))


;; ---- ;;


;; use-package helps organize emacs config

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(put 'use-package 'lisp-indent-function 'defun)

(setq use-package-verbose t)
; (setq use-package-always-ensure t)

(require 'use-package)

(use-package auto-compile
  ;; :config (auto-compile-on-load-mode)
  )



(provide 'my-package-init)
