(setq load-path (append load-path '("~/.emacs.d/lisp/")))
(setq load-path (append load-path '("~/.emacs.d/lisp/my-config/")))
(setq load-path (append load-path '("~/.emacs.d/lisp/evil-surround")))
(setq load-path (append load-path '("~/.emacs.d/lisp/js3-mode")))
(setq load-path (append load-path '("~/.emacs.d/lisp/ace-jump-mode/")))



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
    ("52bba65c6fdc7b9e3ffd0522a718a54f64e98db5109d10d90782d3845039b086" "5bcd847e618e8fc15a0876e79fea7722203e651e1f2a599d9cbd4ae9c3ac779a" "fb29b75bb022efa819b60441e8eb9ac65b21699c4f9ad2a51a94199f0d5c7c8f" "8c2c08f7ca07fa6f185f5a48bc2ee6c0116486a3089615a17202ebff6e64a8b0" "eb95790ee1b8d2ff9c88c7aef77b1df93a826551fc46ed39cdb17cfeceda042a" "b2c22cbb9914acdd521660ccbe3fa8dad2910c05aaf80ee5ab980216ed92664e" "023e77d496c1af29f097ce77ea9bedac6eba69ad374610e4e2c41776b3c7ebb9" "87348566108ec6b8433619a933dbb27fc785b7aa09bb9bd34a344dc08b53e083" "c3ef396314409827199df1d5a10965a1cc1fc2a5ed8eb63a5176155cfe8325bd" "20218ce9fc245f16cdcae9cefa0eac6c74d3bb83cd117a16e2151c03a0362fca" "9c22c0e799c54a71a07067a97ffdce90ae425ee092ab143bed47d95abf31b910" "7b4291e5aa4a191e8742d73c85f7fd0513a35ebbfbd0fd8698c8b7d205662299" "e6d9e53cfb6eda84833fa3859757382efc07177c0bb5c90881c5e585e881e7bf" "28da715fe0c4db92f6c20a86c64e23c539d372c33d9aafe62dea2b5feabf513b" "2f30f5c6cd9602c551c6f076c5be878d8a2ad5e5a41560a2ba99722198f78003" "e33ab4f087cb27fbb25a4a147be8e822ba21e9593594ed6f0ee321f4728b1201" "b79c952a20dff0fc75cfd4a6e4e58836737c1142a89e93c7c09b1c63028da00c" "36e600a85eed59c7c124625fb8f21f3c3f1d869416df12899b3afdd6d3582c34" "f75550739a3382b433446b108c3079371480183cb663075226a9017d451d6b48" "221961686c5e0de40255ec2bb3176a6a0ca0055ee412d90f0902a7ff21b702e1" "c5c7d23b6d507d271d21e74920c745a42d36f9747df72f45e2e937c1f7b020fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e2e7cd9039d46bd97a0e6f1793dc6b368d3aedbc348bb74f6bc2ff5cfdf9b282" "4f8201baa470bf9e0b2733428f047f69ecdb5762470fa07c30c654e71465cfd7" "4b2ab7f06a85bc9835fc814d2ed7816c8bda1f31ac3bccef911ec159b0803ca4" "032247ec745c968379d80cce0c463f4955f67f452ca8f539228438d3f5fd7adc" "60946b664dfdd8f5785ae326225001b693118fc64c1bea9eac10fd6f3955aac1" "14d3d23ce22dd201d79129b87afebe2de9cabe4085d1e0c097a4616b49b839ef" "4257aa86bb0e6f138c8f8cce08d43c91ec76b786ddaedba1f8275cbeb4ce0c20" "412a769d4103ea409f94eea32cd74f719ac8709d64812f3904b6a39d90633cc5" "94a36042f2514d794df873db1d7bc0656a949bc63f74b571d9d221b260279265" "93d33eee9b81c53de999131e769588c0b13c386f9c49031b36ecc9d04460ec3b" "1abda075ebacaa3795d675bb2be0a905322ac856f9c0c259da63f9ccfe1962ec" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "be37526384fcc0dde6107e0f83a7928408147fcf561621ec9c778a547f97a564" "894678873f39f37c9815268523a896caf9be66d22a582331ad9c99bee180ca7d" "6b54a1210097c60a480e1f28f27cb41c0c6330bfb61ec6d1a625141c8d7fb5b9" "65807c7ae17cf57e91a312c7c865af7bf5ca60d4d65d0000654b5cd3ce78adf2" "a3f91517e24db601702a821d98124e1e070a090f45b404adeb04e970048616b0" "a47cfc55d2090e6bd07b03d9e695d8bc05ec4f628924281b031511e0f85e46e4" "3d44f65df109b11cfa28e62409ee597969adc76255549fc6b56fad645d5d55fe" "71f116ced24c4993212d555b477d28500cca2d883781efa8effad812bde6369d" "a6d7493c89f2ca494d4dbb60b74d07a79785b538fcae4fc1a5c1d44409e32fc0" "82e9af35335c4d50fbbc57e31c051d072a558b31c38aee61699bf7a93ad2ee17" "3ad08914813f9f3bb460346380621442ea11776ad017948386daa3790b871fd7" "3c7f75b0a5300783c94bc883a40a865ca89b689a07f11bbd4e55949c57c8f1c9" "eb7df574ca00f0611b22d393f971b2e08171ae08c74ef271b8699cbf239b9d09" "b5ed21e6df8a512573b60e3d383c9663060b8811a1a8fd45afa4430caf44b9e3" "72c8f9b8e29c2ceb64198bb41f38a847639a87f70b67e853998a60697a121fe4" "51c141b15fcdac8920c7a188135ae925c06973317d5063b2077ab9f2ee74bde6" "f67f39d37e6cc49e3167a82eed797d9c9b31ec3b4ef4777408384c29da73b8b4" "408b94ecfaace072ce22d20bf0dc54096682768788fb4d883fb4b1c94de9825b" "859a02eb783dbb9d1afb4ab93f1cbd5d42170386d0654320520f7b39db0e4542" "62628effe7f4227ecaa9ae0fd26e26b18340c55d1036192b068ec95aad90b484" "f90327912c8c90d71a4ebe4b58fdee4e362f2d019a5a4409209d65525aaad9de" "c6110c4be7dba000c4d30de82d225237404fe75473eb9519811ce7e0e528a828" "0e79d98bed571ed5c6c7e5bf042ff4213276d85bfe58c50848ba476acf0c9c80" "66c5fa4c4afec9b0ce01d2cb52172e1df0858879c1eee3c914904bbd48a02a74" "543976df2de12eb2ac235c79c7bc1dac6c58f4a34ae6f72237d6e70d8384f37a" "7262b3150ac70473a7cba0aafcf20ae05dcb3df70afedd80c88269907f73c8e3" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "4eaad15465961fd26ef9eef3bee2f630a71d8a4b5b0a588dc851135302f69b16" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(foreground-color "#708183")
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . fullscreen))))
 '(sml/mode-width (quote right))
 '(sml/modified-char "Δ")
 '(sml/mule-info ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:foreground "#676869"))))
 '(helm-match ((t (:inherit match :foreground "black"))) t)
 '(helm-selection ((t (:background "ForestGreen" :foreground "White" :underline "White"))) t)
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 1.3))) t))




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

;; nicer scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; don't be annoying
(setq ring-bell-function 'ignore)

(setq split-height-threshold 1200)




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

