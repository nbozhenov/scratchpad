;;; Info-mode
; disable 'g' and 'n' hotkeys so that one can use them as evil commands
(add-hook 
 'Info-mode-hook 
 (lambda ()
   (define-key Info-mode-map "g" nil)
   (define-key Info-mode-map "n" nil)))
