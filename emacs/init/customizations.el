;; background-color
(set-background-color "white")

;; When point is on one of the paired characters, the other is highlighted.
(show-paren-mode 1)

;; Scroll up to this many lines, to bring point back on screen.
;; If point moves off-screen, redisplay will scroll by up to
;; `scroll-conservatively' lines in order to bring point just barely
;; onto the screen again.  If that cannot be done, then redisplay
;; rcenters point as usual.
(setq scroll-conservatively 50)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 10)

;; Do not disable toolbar and menubar
(tool-bar-mode 0)
(menu-bar-mode 0) ; `C-RClick' invokes the menu

;; TAB
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

;; One space between sentences
;; see desciption of `sentence-end' function for the other parameters
(setq sentence-end-double-space nil)

;; limit on number of Lisp variable bindings (default = 1300)
(setq max-specpdl-size 20000)
;; limit on depth in `eval', `apply' and `funcall' (default = 600)
(setq max-lisp-eval-depth 10000)

; (setq debug-on-quit t)
; (setq debug-on-error t)
; (setq debug-on-signal t)

;; I hope this code correctly configures emacs backup functionality
(setq 
    backup-directory-alist '(("." . "~/.emacs.d/backup")) ; Store backup files here
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5)   ; and how many of the old

;;completion
(setq completion-cycle-threshold 4)
;; inhibit splash screen
(setq inhibit-startup-screen t)

; switch to buffer menu instead of displaying buffer list
; (global-set-key "\C-x\C-b" 'buffer-menu)

; use text mode with auto-fill by default
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; truncate long lines
(setq truncate-lines t) ; works when auto-fill mode is turned off

; unbind set-fill-column (quite useless binding)
(global-unset-key "\C-xf") 

; use non-blinking cursor
(blink-cursor-mode 0)

;; Non-nil means find a file under alternative names, in existing buffers.
;; This means if any existing buffer is visiting the file you want
;; under another name, you get the existing buffer instead of a new buffer.
(setq find-file-existing-other-name t)

(define-key global-map (kbd "RET") 'evil-ret-and-indent)

(prefer-coding-system 'utf-8)

; i want to now what column i stay on
(column-number-mode 1)

; prefer splitting windows vertically
(setq split-width-threshold 200)

;;;;
;; M-e   my-scroll-other-window
;; M-E   my-scroll-other-window-back
;;
(defun my-scroll-other-window ()
  (interactive)
  (save-selected-window
    (select-window (other-window-for-scrolling) t)
    (scroll-up 1)))

(defun my-scroll-other-window-back ()
  (interactive)
  (save-selected-window
    (select-window (other-window-for-scrolling) t)
    (scroll-up -1)))

(mapc (lambda (keymap)
        (define-key (symbol-value keymap) (kbd "M-e") 'my-scroll-other-window)
        (define-key (symbol-value keymap) (kbd "M-E") 'my-scroll-other-window-back))
      '(evil-normal-state-map evil-insert-state-map evil-motion-state-map
        evil-visual-state-map evil-operator-state-map evil-replace-state-map
        global-map))

(icomplete-mode 1)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)


(defun y-find-file (filename &optional wild)
  "Find new file for the current client when in server-client mode."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (if (and proc (processp proc))
        (switch-to-buffer
         (car (server-visit-files `((,filename)) proc nil)))
      (find-file filename wild))))

(global-set-key "\C-x\C-f" 'y-find-file)
(global-set-key "\C-x\C-b" 'buffer-menu)

