;; Setup important variables before loading evil.
(defun my-init/evil/init ()
  (setq evil-search-wrap nil)
  ; (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil))

(defun my-init/evil/config ()
  (evil-mode 1)
  ;; Treat my_c_var or my-lisp-var as one word.
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (evil-collection-init)
  (define-key company-active-map (kbd "C-j") 'company-complete-selection)

  ;;
  ;; Cursor customization (obviously, for GUI mode only).
  ;;
  (setq evil-insert-state-cursor 'bar)
  (setq evil-replace-state-cursor 'hollow)

  (require 'evil-magit)
  (define-key global-map (kbd "\C-cg") 'magit-status)

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
        (evil-exit-emacs-state)))))

(use-package evil
  :demand
  :ensure t
  :ensure evil-collection
  :ensure evil-magit
  :init (my-init/evil/init)
  :config (my-init/evil/config)
)
