;;
;; hl-line-mode
;;
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;
;; auto-fill-mode
;;
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'turn-on-auto-fill)

;;
;; profiler
;;
(use-package profiler
  :bind (:map profiler-report-mode-map
         ("C-i" . 'profiler-report-expand-entry)))

;;
;; Enable/disable misc modes.
;;
(show-paren-mode 1) ;; show matching parenthesis
(which-key-mode 1) ;; show key bindings for incomplete commands

;;
;; Hacks for windows
;;
(when (member system-type '(cygwin windows-nt ms-dos))
  (setq dired-free-space-program nil)
  (magit-auto-revert-mode 0)
  (global-magit-file-mode 0))

;;
;; Appearance and GUI customization
;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(column-number-mode 1) ; display column number in status line
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; These are two nice options for font selection. Need to make decision later.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9.5"))
;; DejaVu Sans Mono-9 looks bad for special characters and when bold.
;; (add-to-list 'default-frame-alist '(font . "Input-9"))
