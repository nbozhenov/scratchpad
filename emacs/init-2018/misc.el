;;
;; hl-line-mode
;;
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

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
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
