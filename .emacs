(setq load-path (append load-path '("~/.emacs.d/lisp/")))
(setq load-path (append load-path '("~/.emacs.d/lisp/my-config/")))
(setq load-path (append load-path '("~/.emacs.d/lisp/evil-surround")))
(setq load-path (append load-path '("~/.emacs.d/lisp/js3-mode")))
(setq load-path (append load-path '("~/.emacs.d/lisp/ace-jump-mode/")))

(require 'my-init)



;; ~~~~~ CONFIGURATION ~~~~~
;; fuck you magit for making me do this
(setq magit-last-seen-setup-instructions "1.4.0")

;; file types
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; indent lines automatically
(electric-indent-mode t)
(electric-layout-mode t)
(after 'evil
  ;; fix conflict with electric-indent-mode in 24.4
  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent))

;; Spaces!
(setq-default indent-tabs-mode nil)

;; no line wrapping kthx
(setq-default truncate-lines t)

;; highlight matching parens
(show-paren-mode t)

;; command key
(setq mac-command-modifier 'meta)

;; let me see comments
(setq hs-hide-comments-when-hiding-all nil)

;; backups go in their own directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; offline hyperspec
(setq common-lisp-hyperspec-root "/Users/runningskull/documentation/lisp hyperspec/HyperSpec-7-0/Hyperspec")

;; colors & fonts
;; (load-theme 'solarized-dark t)
;; (load-theme 'base16-ocean t)
(load-file "/Users/runningskull/.emacs.d/lisp/color-theme-minimal-light/minimal-light.el")
(set-default-font "Source Code Pro")

;; nicer scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; don't be annoying
(setq ring-bell-function 'ignore)




;; ~~~~~ FURTHER CONFIG ~~~~~

;; modes
(require 'my-evil)
(require 'my-slime)
(require 'my-compilation)

;; languages
(require 'my-prog)
(require 'my-lisp)
(require 'my-html)
(require 'my-scss)
(require 'my-python)
(require 'my-clojure)
(require 'my-javascript)
(require 'my-clojurescript)




;; ~~~~~ EMACS CUSTOMIZE ~~~~~

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"])
 '(ansi-term-color-vector
   [unspecified "#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"])
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   (quote
    ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "be37526384fcc0dde6107e0f83a7928408147fcf561621ec9c778a547f97a564" "894678873f39f37c9815268523a896caf9be66d22a582331ad9c99bee180ca7d" "6b54a1210097c60a480e1f28f27cb41c0c6330bfb61ec6d1a625141c8d7fb5b9" "65807c7ae17cf57e91a312c7c865af7bf5ca60d4d65d0000654b5cd3ce78adf2" "a3f91517e24db601702a821d98124e1e070a090f45b404adeb04e970048616b0" "a47cfc55d2090e6bd07b03d9e695d8bc05ec4f628924281b031511e0f85e46e4" "3d44f65df109b11cfa28e62409ee597969adc76255549fc6b56fad645d5d55fe" "71f116ced24c4993212d555b477d28500cca2d883781efa8effad812bde6369d" "a6d7493c89f2ca494d4dbb60b74d07a79785b538fcae4fc1a5c1d44409e32fc0" "82e9af35335c4d50fbbc57e31c051d072a558b31c38aee61699bf7a93ad2ee17" "3ad08914813f9f3bb460346380621442ea11776ad017948386daa3790b871fd7" "3c7f75b0a5300783c94bc883a40a865ca89b689a07f11bbd4e55949c57c8f1c9" "eb7df574ca00f0611b22d393f971b2e08171ae08c74ef271b8699cbf239b9d09" "b5ed21e6df8a512573b60e3d383c9663060b8811a1a8fd45afa4430caf44b9e3" "72c8f9b8e29c2ceb64198bb41f38a847639a87f70b67e853998a60697a121fe4" "51c141b15fcdac8920c7a188135ae925c06973317d5063b2077ab9f2ee74bde6" "f67f39d37e6cc49e3167a82eed797d9c9b31ec3b4ef4777408384c29da73b8b4" "408b94ecfaace072ce22d20bf0dc54096682768788fb4d883fb4b1c94de9825b" "859a02eb783dbb9d1afb4ab93f1cbd5d42170386d0654320520f7b39db0e4542" "62628effe7f4227ecaa9ae0fd26e26b18340c55d1036192b068ec95aad90b484" "f90327912c8c90d71a4ebe4b58fdee4e362f2d019a5a4409209d65525aaad9de" "c6110c4be7dba000c4d30de82d225237404fe75473eb9519811ce7e0e528a828" "0e79d98bed571ed5c6c7e5bf042ff4213276d85bfe58c50848ba476acf0c9c80" "66c5fa4c4afec9b0ce01d2cb52172e1df0858879c1eee3c914904bbd48a02a74" "543976df2de12eb2ac235c79c7bc1dac6c58f4a34ae6f72237d6e70d8384f37a" "7262b3150ac70473a7cba0aafcf20ae05dcb3df70afedd80c88269907f73c8e3" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "4eaad15465961fd26ef9eef3bee2f630a71d8a4b5b0a588dc851135302f69b16" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(foreground-color "#708183")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-match ((t (:inherit match :foreground "black"))) t)
 '(helm-selection ((t (:background "ForestGreen" :foreground "White" :underline "White"))) t)
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 1.3))) t))
