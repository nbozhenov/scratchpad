;;
;; hl-line-mode
;;
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;
;; highlight trailing whitespaces and indicate empty lines
;;
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
;; Ignore trailing spaces in certain modes.
(dolist (mode-hook '(comint-mode-hook
                     Info-mode-hook
                     magit-popup-mode
                     special-mode-hook
                     ))
  (add-hook mode-hook (lambda () (setq show-trailing-whitespace nil))))

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
;; Switching languages (input methods).
;;
(if (eq (window-system) 'w32)
    ; For some reason emacs-w32 with russian-computer (and other) input methods
    ; mishandles ',' and '?' keys. So, switching input methods on w32 is
    ; temporarily disabled.
    (progn
      (global-set-key (kbd "C-9") (lambda () (interactive)))
      (global-set-key (kbd "C-0") (lambda () (interactive))))
  (progn
    (global-set-key (kbd "C-9")
                    (lambda ()
                      (interactive)
                      (set-input-method nil (called-interactively-p))))
    (global-set-key (kbd "C-0")
                    (lambda ()
                      (interactive)
                      (set-input-method "russian-computer" (called-interactively-p))))))

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
  :defer t)
; :hook ((text-mode . flyspell-mode)
         ; flyspell-prog-mode gives a lot of false-positive diagnostics for
         ; identifiers mentioned in comments.
         ; (prog-mode . flyspell-prog-mode)
;        ))

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
  ; (magit-auto-revert-mode 0)
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

  ; Setup fonts.
  (let* ((host-id (cond ((string= my-hostname "hive\n") :hive)
                        ((string= my-hostname "nbozheno-MOBL\n")
                         (if on-windows-p :mobl-cyg :mobl-wsl))
                        (t nil)))
         (disp-type (if (> (x-display-pixel-height) 1500) :4k :full-hd))
         (my-font (cond ((eq host-id :hive) "Input-11")
                        ((eq host-id :mobl-cyg) "Input-9")
                        ((eq host-id :mobl-wsl) "Input-11")
                        ;; Use more widespread fonst on unnamed machines.
                        ((eq disp-type :4k) "DejaVu Sans Mono-11")
                        ((eq disp-type :full-hd) "DejaVu Sans Mono-9")
                        ((eq host-id nil) nil)
                        (t (error "Unknown host-id")))))
    (when my-font
      (add-to-list 'default-frame-alist (cons 'font my-font)))))
