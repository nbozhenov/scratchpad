;;
;; Setup important variables before loading evil.
;;
(setq evil-search-wrap nil)
; (setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-integration nil)

;;
;; Load and enable evil.
;;
(require 'evil)
(evil-mode 1)

(setq evil-collection-setup-minibuffer t)
(evil-collection-init)

;;
;; Cursor customization (obviously, for GUI mode only).
;;
(setq evil-insert-state-cursor 'bar)
(setq evil-replace-state-cursor 'hollow)

;;
;; Enable proper evil state in various modes.
;;
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

(require 'evil-magit)
(define-key global-map (kbd "\C-cdg") 'magit-status)

;;
;; Define some useful commands
;;
(evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
(evil-ex-define-cmd "wq" 
  (lambda () (interactive)
    (save-buffer (current-buffer))
    (kill-buffer-and-window)))

(define-key evil-emacs-state-map (kbd "C-z")
  (lambda () (interactive)
    (if (eq last-command 'evil-execute-in-emacs-state)
	(suspend-emacs)
      (evil-exit-emacs-state))))
