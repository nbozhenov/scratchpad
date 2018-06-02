;;
;; BACKUP
;;
(setq 
    backup-directory-alist '(("." . "~/.emacs.d/backup")) ; Store backup files here
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5)   ; and how many of the old


;;
;; DISPLAY environment variable
;;
(defun my-update-display-env ()
  (let ((filename (concat (getenv "HOME") "/.display.txt")))
    (when (file-exists-p filename)
      (setenv "DISPLAY"
              (with-temp-buffer
                (insert-file-contents filename)
                (buffer-string))))))
(run-with-timer 20 20 'my-update-display-env)


;;
;; Interface
;;
(if (display-graphic-p)
    (set-background-color "white")
  (load-theme 'tango-dark))

;; Inhibit splash screen.
(setq inhibit-startup-screen t)
;; Use fundamental mode in *scratch* buffer.
(setq initial-major-mode 'text-mode)

;; Unicode
(prefer-coding-system 'utf-8)

;; Smooth scrolling
(setq scroll-conservatively 2)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 10)

;; TAB
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

;; Make sure <tab> and <escape> work identically in GUI and in terminal. That
;; is, translate their terminal representations into GUI form.
(define-key input-decode-map (kbd "C-i") [tab])
; M-x stops working in cygwin terminal if the line below is uncommented.
; Seems that Alt is translated into some sequence including "C-[" combination.
; (define-key input-decode-map (kbd "C-[") [escape])

;; One space between sentences
;; see desciption of `sentence-end' function for the other parameters
(setq sentence-end-double-space nil)

; truncate long lines
(setq truncate-lines t) ; works when auto-fill mode is turned off

;; Non-nil means find a file under alternative names, in existing buffers.
;; This means if any existing buffer is visiting the file you want
;; under another name, you get the existing buffer instead of a new buffer.
(setq find-file-existing-other-name t)

;; limit on number of Lisp variable bindings (default = 1300)
(setq max-specpdl-size 5000)
;; limit on depth in `eval', `apply' and `funcall' (default = 800)
(setq max-lisp-eval-depth 3000)

;; Buffers (see also helm.el)
(global-set-key "\C-x\C-b" 'buffer-menu)

; unbind set-fill-column (quite useless binding)
(global-unset-key "\C-xf") 
(custom-set-variables '(fill-column 80))

; xclip
(xclip-mode (if (getenv "SSH_CONNECTION") 1 0))
