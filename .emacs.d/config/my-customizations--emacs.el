(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"])
 '(ansi-term-color-vector
   [unspecified "#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"] t)
 '(avy-keys
   (quote
    (97 115 100 103 104 107 108 113 119 101 114 116 121 117 105 111 112 122 120 99 118 98 110 109 102 106 59)))
 '(cider-auto-jump-to-error nil)
 '(cider-cljs-lein-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(cider-cljs-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(cider-default-repl-command "boot")
 '(cider-font-lock-dynamically (quote (macro function var deprecated core)))
 '(cider-mode-line (quote (:eval (format " ⌘%s" (my--cider--modeline-info)))))
 '(cider-pprint-fn (quote fipp))
 '(cider-repl-shortcut-dispatch-char 126)
 '(cider-show-error-buffer (quote except-in-repl))
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake
		  (company-capf company-dabbrev-code company-dabbrev)
		  (company-gtags company-etags company-keywords)
		  company-oddmuse company-files company-dabbrev)))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#708183")
 '(cursor-in-non-selected-windows nil)
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("e4d236687368759046159e9610d1634e45b69c4e593f065078c9820e9e270721" "b1b4289d49f0f57a2239761bb87a5fce2743fcd62877965e8cf67495964db6cd" "4b23de24edbf8a86ddeb188f644f325c54cc5f700b73e5163d7a541ceb8bfe1c" "65049bf5b4eae7b642c140b77bc1a82860904b63cbf30fa957907d2f240c87c7" "ee4eb0e4e4db61abe33a142029efb20769575e86828fd09234a2fcea96bf5cef" "4c6c71287a2817556ce4bfeb21ac034fc96e5c8e5d4c79165fe45779a2bb17f2" "4852c98037aafa14a434c3c52a174e1383048c76dd2ce6edc28bc9e76bed5b79" "eb24677b1f9fcbeb3ab2d51aa168e3d4ea3cdc5d08f31dafe175a42cc5d455f9" "0457a21f75064d0f6ebcb715727c1137443e9a38d42df6933d8421b02b1d7412" "659a835b57d59da732e4bcc8bf874ba1453c689b3251c3a9a2bceb63ed97aaa2" "40e375a650949bc2ef6517c1564e0541ebc3dcbe1023aac92bc53e331f7ed6e2" "53594638e39d60e3112f56e28ab5ea2efae59c8b82d79ce45b23d712d77bec6e" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "e697d31361bb4a0a2c15db5a18b2ff4b2bd256fbebc29fdc72deb802b505eb64" "4be85060065a7be74859406237874a2e6da9ad09a8494aea472f5ab47c19b160" "9a4343129004c45c7265fbbe1dd5d77ff75ae813a0d286a8685c913214d57c93" "398f0209bfd642cf7a5e3e03bdc20db2822fd6746225a4bd99ccf9b26d3059d0" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "ba83383a678890b808c7e22c4501779c445d154a1dc084ba644dc96c0c772ead" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "6150be00ff1d281217fbdda8b657036b0a5b64e98116efb3c22949c3085f823f" "1c5a778503253e72e917b9ff01059af6be631aad87a0bbed5ce35ecf27c58148" "369340b3aa9bf90a3312b3ba2d64b5245fe565d09178e18c04a1e30fda53669c" "8b3618e09a8e1f982f5972896ec5cdbe3c4164523fb99dfb7ad90a03f308d3f1" "bcbe1349d3f26b609aff47b98e5fcf99abd0812d854cc523d0df2f40bdeaa168" "31eb5dec288f776bfe93f41004d394f7f1120da292f3e54bf0dc6874a4e211db" "980dd28a59ac311a5092d684fd95bdf31f489298d28670f424cd03826e6aaa11" "b978240b2ad1c59808b31fb90e8fa9d6a199689811a8ee8b62e5d80bc82f66d3" "46a35f2573f7d87d0ed3ce4491334bd2d30fd23b7718672f0729ba1177a3a310" "388737cb8d9d843cb44a6db1450665931088f009715c15f77f185183f9ecf6ee" "65e5ab5f45a16e3c8bcdb1c1b32f22876ed51620cd6c47826d2f9d590b7df01e" "72f9c6d55e23651c2e500f0a6b12ec0758348a2798a30bb918d593c10d5e0c9d" "442b65b8b79e07ee68bb3785af2b7d1b18e3abbeaf8fec5f8eb252d250b40768" "14378f03e77e4ff0c965dd3b34f91a2af61d22afadd2ead264bd9fb21adf7f1d" "95dc7e2f0e7a2bafcdd544097f9603056f34ccca1d1bae6a0a214ee968dcd86c" "8b3f9737f1b923a95a665cc56782bf1e462c64212fa5c645cce68482f695c539" "fe979dae33ce46bef0fda2762e12d7743085f17a67adaaa928a46e985cd0d206" "58a504b24d52dbb84abb55f51672066dd9bc7055ce04c1cf20255bd2f606f183" "13e75136e3a2dc196e6a2471a458c9dcfcbd6391d496e8758c5bc0f2ee51a64f" "8f409c2899909281d0ba4a8fc2718f16ebd4419ad818905b4378c2c1663f3f14" "d583b2f74e9211ae7b9864ec5970276546abb7a5a654e9bdeaf51b0006e296a5" "fddd476f04e8cfb5c83db9750b9fbfc9ad22046e0d6baf7ae03f63857b2feed9" "f5eb0e77a3a6061d784e545ac677f01ce49e57264ad9741b33df0b0359bbf215" "6baace09c8226396ed636f11bedc87368ece18fbf56e679b11432e34332297ab" "b599601568b246b8b47b2996e04da71d94c9cf6ec74671a7f90008bf66bc6a53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "508104926293a47833321c95eca88f094d8629fa635677dbca4a92552d221d74" "849839ca2f2c13d42afbc3f4880355180e1388c310cb951e68ae05b9597b05f7" "e3fd1e3fd790ab340c5c65ccbf6ab50bf983516655e2b42bea59193bd295c6c6" "5ebc1aae1f4ad954bf55db5474919ec6cb4f18c1e9b215c1991085b6f417d694" "bb962503437379d4130290999ca76cbda16987b0605e0f134d6a905eb6fd6c49" "26419cde6045a791cb11c61bbe76942eda83d24df220be8458a74ba3cecc1eee" "3753230041b452abb6d95cdae783c8ac97624a568d61f2d1b0da43a1a25ba51f" "afa078909e9aaeb432a898dc409d5a510e4b42a5613b55e4c44ad2fceb2e8972" "5f38fa37dbe108c260191d041175b264709ca0ec99506efcfb57db7e6c31ec72" "606f9dde4c46588140ad12517ae27cca0a08bb59861442ef4dd9b2dfa5e7cb06" "6d9ae9976339dd0c19b162e10c2e8993be3902f329f4cfb230f405f010dcee2a" "e8e84ccb4fa90634688d26334207f7d00d0da08e11f009a8aeebda26cdb54c46" "fbea532953caf1b38c6500df51c7d57c632a9e61c96b4dfa012db139c2a160f0" "eedeae21cc4042e62fba51e85409ce346c99344c9007708bde3a082e6fd9efb9" "6233e77f2fbd8ba238edcf0e843cde08a633c82592e11c115292b9afa48c63f2" "075f55970c78b4bab3e2f68e5e23aacf7f06309d5a2ddb88bfe73d02b542ec8e" "6b8bd8ae58dbc9ac92bff8a183eb3ac699e134864f1ced15ebad6ba81570c720" "f7e1e686033138640123dd828bff413b16094d2776ad153a9b18d37e387b56c0" "f58228d57e9aef05d4bc61e1981682a3c41184f3feb0e172a68088a26d0524ab" "7e30cff71b488c1caca7f882458ce524aa25ba46987670da7d68cb29fe52c4cd" "d3d5a95d65bb0985be6093f8fa1593b969b28fc5e2c72aad7519113b49ed9b86" "502d8ee49a426f5d1fa97fd68410e6c8a10174b545b4a4e4a213b79d025c81fe" "8163f215fa01b1a589112d74ca260dcd06a6b5ed758f83ba48d63d7047a517c1" "07bacb954f65888f19b1574f7a4f7fc0b65f15a183ff976863379a9538051cbe" "2383e25de5c7a759d3fab96231e37e156a467f913d31623d2820e4ba50e07b07" "5881774ad0b4169b2689103e7f4dc8e8a884e6cff53154f7be1afe64aa4e05ea" "906b99046016f1bfb3fe9b94afb6951ad7918f4def4d1b5a36643aecbd6631cd" "d84b63e5d696b85be1fbc4cd3ba941a4632f155dc281d170507328944843e56f" "6bf401f805eccbb960650548c12c96cbee1a5d10fa3288fc9057c88551e92d38" "e2fa77d2ff88aad51edbbffb71bdad2317019c64a5ea5db4a714c782d04712f3" "ff11ff429167d7674f6cdb7aeafcb7928fc5dfca7dddb3a549c07e5b8788ad39" "1b1566017cfedef494b0d4f601b7ed7e96251ccb1a8e787c9b10ce6c7027c006" "6962d5627a440d08c0d878bb6d3709828eb3c06d9ac36ff203b4e519a2a501ab" "658870f0c9b314896ef8dd3fe3ab1bcd61d6ae1fabf01dbdfdd452717d48282c" "960ada35305bb7454f5aba44b565e05d757d204ae7fbf79e264349b3100bcbf7" "ca8f8fc0f5c21fa3adfe2f95fd0f9f908df242a13b7d40b181010fe7ea546df1" "fde4fb20bb3aceb9180b78882dc0cdcf06b53703bc685aa641f77e9cf131e45c" "b13450c7cd3c13b7993c6bbf727cb5ce65c3ca18c8239b518c5edfc26602f574" "47e92af7c3c4dd96047b96a400226e0ae9748f20c5a1747b614b43b34a67f5c7" "dad10be08cc951e861be0e85588254067ade206be0a335f8dfe9823cebd4b9b3" "6e62a32f71efef88807702ab1431b948c638bb29d3d75ff5d46837244af2353c" "0354dc72b7680001fbc4d7a756ed31e04e7e32550973f18424db58df93ef039c" "e3b2211ddef296f4d4cd8b9c9d47c8f5e58203816815ac45387c961035a986f1" "51426a289522d64f1abc96ac5bc45afa28963df2be248482d8f8efced35d4692" "5a25f5d0c02e02be1f2f0c67bcfe655b53dfe3ff18de8c8df0d89d76f1532e2a" "1483b875764a6e38eaede8304c867c33beb36a6600739d648e4241d77298572c" "b2dd48d5f217fb72e8e8a7d9c6d975331f723f45cdbbb3ffa65aef152d28e62e" "3ea420e5503565b86b9b44bf76e27c60490c72053ea533b5727121a815c76a99" "e5b677b06cd97f58dc4406d9f04b69ef565b8ec43d6dcba9b6ff8909cca3611c" "ea82df44e47b53d053c22bf050e722bc6b55494ea0729616de0b644e4b95717a" "8ca433b884d88313f3c2c8a67b8e386872fb6e24befefd20661c6ab13588bd3c" "e1be2d63183e308758bbb2b78700a83eaca6264ecf9966d0dea2aee43ead7362" "d7f402e8eae109e761956d78b8cc79c060d85e883323efa6031625056c471200" "d7ee94819017db55b8dc5f148390528f1ab31ef8f0054be9e9340aba8ce74b37" "73c843c5715059b096a8738a43d5599ce1564e57bb1200ca66e7555f8692cfa2" "610b61de904abe37e3bd63cf7d855206111bed14b146b9e58289e4890be2f91a" "1fad99b7cd9a804e6da94fe31b3c7758173867ce31504387dee45c21b77095ca" "697d85c2bdac789f93230a7df0e4ad3ab7d0df846449b9ab65e0df998bdfda7a" "c97b3b69facd78e63ee8b721d9ca6325541d3c90b98966b36d5e7c967757a561" "6b717e37c9d49dab2a711e778746cb2daad409e9c15f89a8e555213966d5c693" "a458626f949e4ce3426b167ab88394c9a59c2cd62a0304eee9716652b610600e" "e2ab794362925aa33fef620a5bb487f108a9838913cc39805fd61e64a99798ff" "859c642479992f2ff77f35222393efe4562a1d9d6691a2d648fc2ff85ea047ef" "11a583eefff2a961164ece566d1a61746a1a8989dfa24d2d44c86ca1e02f425c" "61fb4ff67e2f671c26022169f09a574cb8ca45c39f32a009381be62f5ca52070" "c29cb124f1a0c0d7fef9a6e3cdf8b1867ebaedfa0a9f9c10d87bf74cb1621009" "cc351222e1cda88645e6722873d0675a5c34b3c31e71569b68f83dd5d3a95f8d" "34525043318d3ac7f4f3d103b51a8cd1d8679a5927205acf1f104e27da0297d8" "d36a20f80f7c31b4cefbbac41ae623c42d635ac51e45a990f2e02f76033ffbef" "45dcc447121c8dd628d1a75c6322e45e70881406e1c8ebeac8c3b999fc5bead2" "5cfae0e79f6cd09aa0892b0993e811874f2375cf117a2945d3fe70243eaac443" "8b4c5a0e046588dc93b0b80ed5cdc58eda8f1764bd5ec105ae4fb4c0f1eb0c3d" "9d47445471ee34f14b7d2f44cb150d12a08011bccc6ed6c6c73bb8c0d5215360" "a91070b62ea63b20df952af005ae9df7be922ed3319ac12198adab940fb68c9f" default)))
 '(evil-mode-line-format (quote (before . mode-line-front-space)))
 '(evil-want-fine-undo nil)
 '(fci-rule-color "#F0F0F0")
 '(foreground-color "#708183")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(lua-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 8))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (geiser quack pbcopy dracula-theme doom-themes ace-window paredit company evil cider hideshowvis avy-mode mustache-mode paxedit exec-path-from-shell which-key web-mode visual-fill-column spaceline solarized-theme smex scss-mode rich-minority projectile powerline-evil popwin parenface-plus neotree names monochrome-theme mo-git-blame material-theme markdown-mode magit lua-mode js-comint ido-vertical-mode ido-ubiquitous gnugo frame-cmds flx-ido expand-region evil-paredit evil-nerd-commenter evil-leader diminish css-mode css-comb company-restclient company-quickhelp company-flx clj-refactor badger-theme avy ag)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-height 15)
 '(recentf-max-saved-items 1000)
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "user/start")
     (cider-refresh-before-fn . "user/stop")
     (eval progn
	   (setq cider-refresh-before-fn "user/stop" cider-refresh-after-fn "user/start"))
     (eval progn
	   (defun postmaster/stop-start
	       (setq cider-refresh-before-fn "user/stop" cider-refresh-after-fn "user/start"))
	   (add-hook
	    (quote cider-repl-mode-hook)
	    (quote postmaster/stop-start))
	   (put-clojure-indent
	    (quote GET)
	    2)
	   (put-clojure-indent
	    (quote PUT)
	    2)
	   (put-clojure-indent
	    (quote POST)
	    2)
	   (put-clojure-indent
	    (quote DELETE)
	    2)
	   (put-clojure-indent
	    (quote ANY)
	    2)
	   (put-clojure-indent
	    (quote wcar)
	    (quote defun))
	   (setq cider-refresh-before-fn "user/stop" cider-refresh-after-fn "user/start")))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(solarized-use-less-bold t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#D9D9D9")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#3C3C3C")
     (60 . "#3C3C3C")
     (80 . "#252525")
     (100 . "#252525")
     (120 . "#161616")
     (140 . "#161616")
     (160 . "#0E0E0E")
     (180 . "#0E0E0E")
     (200 . "#0E0E0E")
     (220 . "#080808")
     (240 . "#080808")
     (260 . "#080808")
     (280 . "#080808")
     (300 . "#080808")
     (320 . "#080808")
     (340 . "#080808")
     (360 . "#080808"))))
 '(vc-annotate-very-old-color "#161616")
 '(which-key-add-column-padding 5)
 '(which-key-allow-evil-operators nil)
 '(which-key-idle-delay 0.2)
 '(which-key-min-display-lines 4)
 '(which-key-mode t)
 '(which-key-popup-type (quote side-window))
 '(which-key-show-prefix (quote left))
 '(which-key-side-window-location (quote bottom))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(provide 'my-customizations)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#3bafda" :foreground "brightwhite"))))
 '(avy-lead-face-0 ((t (:background "#3bafda" :foreground "brightwhite"))))
 '(avy-lead-face-1 ((t (:background "#3bafda" :foreground "brightwhite"))))
 '(avy-lead-face-2 ((t (:background "#f86bf3" :foreground "brightwhite")))))
