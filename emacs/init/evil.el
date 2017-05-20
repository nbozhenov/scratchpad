;(setq evil-esc-mode t) ; prevent evil-mode from recreating input-decode-map
(setq evil-want-C-u-scroll t)
; C-i is equal to <tab> in terminal
(setq evil-want-C-i-jump nil) ; one is still able to invoke evil-jump-forward
(require 'evil)
(evil-mode 1)

;; FIXME: dirty hack
(defun evil-max-scroll-down () 0)

;; This code, I hope, disables vim bindings for Space and Return
;; because they are quite useless in vim
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; no mode should start in emacs mode
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;; my evil customizations
(define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)

; :q should only kill one buffer I suppose, and without unneeded questions
(defun y-kill-current-buffer ()
  (interactive)
  "Disconnect client from buffer when in server-client mode."
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (if proc
        (server-edit)
      (kill-buffer-and-window))))
(evil-ex-define-cmd "q[uit]" 'y-kill-current-buffer)

; :wq
(evil-ex-define-cmd "wq" 
  (lambda () (interactive)
    (save-buffer (current-buffer))
    (y-kill-current-buffer)))
; disable search wrap
(setq evil-search-wrap nil)

;; use evil key-bindings everywhere
;(setq evil-overriding-maps nil)

;; use 'tn' to find next tag
(evil-ex-define-cmd "tn[ext]" (lambda () (interactive) (find-tag "" t)))

;; use C-z to suspend emacs
; default evil-behavior is to call 'evil-emacs-state
(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key global-map (kbd "C-z") 'suspend-emacs)

;; small patch
(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-up (min (evil-max-scroll-down) c)))
      ;; use scroll-up instead of forward-line to scroll-down nicely when scrolling
      ;; from the very beginning of a buffer in presence of scroll-margin variable
      ;;(forward-line c)
      (scroll-up c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))


(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (1+ (/ (* 4 (evil-num-visible-lines)) 4)))))
      (save-excursion
        (scroll-up (min (evil-max-scroll-down) c)))
      ;; use scroll-up instead of forward-line to scroll-down nicely when scrolling
      ;; from the very beginning of a buffer in presence of scroll-margin variable
      ;;(forward-line c)
      (scroll-up c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))



(evil-define-command evil-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards
and sets `evil-ud-scroll-count'
Uses `evil-ud-scroll-count' instead If COUNT not specified.
Scrolls half the screen if `evil-ud-scroll-count' equals 0."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let* ((p (point))
           (cv (or count (max 0 (if (boundp 'evil-ud-scroll-count) evil-ud-scroll-count 0))))
           (c (if (= cv 0) (1+ (/ (* 4 (evil-num-visible-lines)) 4)) cv))
           (scrollable (max 0
                            (+ c (save-excursion
                                   (goto-char (window-start))
                                   (forward-line (- c)))))))
      (setq evil-ud-scroll-count cv)
      (save-excursion
        (scroll-down scrollable))
      (forward-line (- c))
      (when (= 0 (evil-count-lines p (point)))
        (signal 'beginning-of-buffer nil)))))







(define-key global-map (kbd "C-<return>")
  (lambda ()
    (interactive)
    (if (or (equal evil-state 'normal)
            (equal evil-state 'motion))
        (evil-emacs-state)
      (if buffer-read-only
          (evil-motion-state)
        (evil-normal-state)))))
