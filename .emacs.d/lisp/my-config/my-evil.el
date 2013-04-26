(require 'evil)
(require 'evil-leader)

;; evil - making the most of SPC and RET
;; (from http://emacswiki.org/emacs/Evil#toc11)
(defun my-move-key (keymap-from keymap-to key)
  "Moves keybindings from one keymap to another, deleting from the old location"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(evil-mode 1)
(evil-leader/set-leader ",")

(provide 'my-evil)
