;; -*- lexical-binding: t -*-

;;; Use TAB as tab, not as '\t'
(setq function-key-map (delete '(tab . [9]) function-key-map))
(setq function-key-map (delete '(kp-tab . [9]) function-key-map))
(setq function-key-map (append function-key-map '((kp-tab . [tab]))))
(setq local-function-key-map (delete '(tab . [9]) local-function-key-map))
(setq local-function-key-map (delete '(kp-tab . [9]) local-function-key-map))
(setq local-function-key-map (append local-function-key-map '((kp-tab . [tab]))))
; use tab in minibuffer
(define-key minibuffer-local-map [tab] 'minibuffer-complete)

;;;;;;;;;;;;;;;;;;
;; Adding MELPA package archive (repository)
;; There are official GNU archive at http://elpa.gnu.org/packages/ and 
;; some non-official archives. See http://ergoemacs.org/emacs/emacs_package_system.html
;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use \C-u for scroll
(setq evil-want-C-u-scroll t)
;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; load evil
(require 'evil)
;; enable mode
(evil-mode 1)

;; This code, I hope, disables vim bindings for Space and Return
;; because they are quite useless in vim
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(elscreen-start)
;; emulate vim tab-functionality
;(load "elscreen" "ElScreen" t)
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab

;; no mode should start in emacs mode
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;; my evil customizations
(define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)
; :q should only kill one buffer I believe
(evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
; :wq
(evil-ex-define-cmd "wq" 
  (lambda () (interactive)
    (save-buffer (current-buffer))
    (kill-buffer-and-window)))
; disable search wrap
(setq evil-search-wrap nil)

;; emulate vim tab-functionality in motion state too
(define-key evil-motion-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-motion-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-motion-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-motion-state-map "gt" 'elscreen-next) ;next tab

;; use evil key-bindings everywhere
;(setq evil-overriding-maps nil)

;; use 'tn' to find next tag
(evil-ex-define-cmd "tn[ext]" (lambda () (interactive) (find-tag "" t)))

;;; C-c as general purpose escape key sequence.
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
;; documentation of it.
;;(set-quit-char "C-c")

;; I still want to use 'C-x C-c' to quit emacs
(define-key global-map (kbd "C-x <escape>") 'kill-emacs)
(define-key global-map (kbd "C-x C-g") 'kill-emacs)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; disable toolbar and menubar
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
;; don't enter debugger if quit was signalled
(setq debug-on-quit nil)

;; I hope this code correctly configures emacs backup functionality
(setq 
    backup-directory-alist '(("." . "~/.emacs.d/backup")) ; Store backup files here
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5)   ; and how many of the old

;; inhibit splash screen
(setq inhibit-startup-screen t)


;;; Info-mode
; disable 'g' and 'n' hotkeys so that one can use them as evil commands
(add-hook 
 'Info-mode-hook 
 (lambda ()
   (define-key Info-mode-map "g" nil)
   (define-key Info-mode-map "n" nil)))

; switch to buffer menu instead of displaying buffer list
(global-set-key "\C-x\C-b" 'buffer-menu)

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

;TAB runs the command org-cycle
;(add-hook 
 ;'org-mode-hook 
 ;(lambda ()
   ;(define-key org-mode-map "TAB" org-cycle)))
(add-to-list 'auto-mode-alist '("^TODO$" . org-mode))

(defun toggle-tab-width ()
    "Toggle tab width between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 4 8))
    (redraw-display))

;; c-mode
;; ; You can determine which offset to edit by hitting [ctrl-c ctrl-s] on any line.
;; ; C-c C-o runs the command c-set-offset
(c-add-style "my-c-style"
             '("linux"
               (c-offsets-alist . ((case-label . 2)
                                   (statement-case-intro . 2)
                                   (access-label . -2)))))
(setq c-default-style "my-c-style"
      c-basic-offset 4)
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

(define-key global-map (kbd "RET") 'evil-ret-and-indent)

