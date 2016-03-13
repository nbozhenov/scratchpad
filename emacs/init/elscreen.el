(elscreen-start)

;; emulate vim tab-functionality
(define-key evil-motion-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-motion-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-motion-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-motion-state-map "gt" 'elscreen-next) ;next tab

