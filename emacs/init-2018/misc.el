;;
;; Info-mode
;;
(add-hook 'Info-mode-hook
 (lambda ()
   (define-key Info-mode-map " " nil)
   (define-key Info-mode-map "g" nil)
   (define-key Info-mode-map "n" nil)
   (evil-define-key 'motion Info-mode-map " " nil)
   (evil-define-key 'motion Info-mode-map " g" 'Info-goto-node)
   (evil-define-key 'motion Info-mode-map " n" 'Info-next)))

;;
;; hl-line-mode
;;
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;
;; dired-mode
;;
(setq dired-free-space-program nil)

;;
;; Enable/disable misc modes.
;;
(show-paren-mode 1) ;; show matching parenthesis
(which-key-mode 1) ;; show key bindings for incomplete commands
(column-number-mode 1) ; display column number in status line
(blink-cursor-mode 0) ; use non-blinking cursor
(tool-bar-mode 0)
(menu-bar-mode 0)
