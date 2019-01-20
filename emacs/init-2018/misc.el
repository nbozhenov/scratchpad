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
;; Spell checking
;;
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "my_enru"
        ispell-local-dictionary-alist '(("my_enru" "[[:alnum:]]" "[^[:alnum:]]"
                                         "[-']" t ("-d" "my_enru") nil utf-8))))

(use-package flyspell
  :defer t
  :hook ((text-mode . flyspell-mode)
         ; flyspell-prog-mode gives a lot of false-positive diagnostics for
         ; identifiers mentioned in comments.
         ; (prog-mode . flyspell-prog-mode)
         ))

(use-package flyspell-correct-popup
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . nil) ; default binding doesn't work in terminal
              ("C-'" . #'flyspell-correct-previous-word-generic)))

(use-package popup
  :defer t
  :bind (:map popup-menu-keymap
              ("C-j" . #'popup-select)
              ("C-h" . #'popup-help)))

;;
;; Enable/disable misc modes.
;;
(show-paren-mode 1) ;; show matching parenthesis
(which-key-mode 1) ;; show key bindings for incomplete commands

;;
;; Hacks for windows
;;
(when on-windows-p
  (setq dired-free-space-program nil)
  (magit-auto-revert-mode 0)
  (global-magit-file-mode 0))

;;
;; Appearance and GUI customization
;;
(menu-bar-mode 0)
(column-number-mode 1) ; display column number in status line

(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (blink-cursor-mode 0)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; An oversimplified adjustment to display sizes.
  (setq my-display-type (if (> (x-display-pixel-height) 1200) '4k 'full-hd))
  (let ((my-font (cond ((eq my-display-type '4k) "Input-11")
                       ((eq my-display-type 'full-hd) "Input-9")
                       (t (error "Unknown display type")))))
    ;; These are two nice options for font selection.
    ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9.5"))
    ;; DejaVu Sans Mono-9 looks bad for special characters and when bold.
    (add-to-list 'default-frame-alist (cons 'font my-font))))
