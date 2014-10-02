(define-key evil-normal-state-map ",,c"
  (lambda ()
    (interactive)
    (compile "~/projects/_rafflecopter/rafflecopter/static/styles/COMPILE.sh")))

(define-key evil-normal-state-map ",,w"
  (lambda ()
    (interactive)
    (compile "~/projects/_rafflecopter/rafflecopter/static/styles/COMPILE-WIDGET.sh")))

(provide 'my-rafl)
