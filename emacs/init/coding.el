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

(defun my-gcc-hook ()
  (when (string-match "/gcc/" buffer-file-name)
    (setq indent-tabs-mode t)
    (c-set-style "gnu" nil)))

(add-hook 'c++-mode-hook 'my-gcc-hook)

;; (define-key ruby-mode-map (kbd "C-j")
;;   (lambda ()
;;     (interactive)
;;     (reindent-then-newline-and-indent)))

;; no pop-up window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; open all headers in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; open all gcc sources in c++-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.c\\'" . c++-mode))
;; open machine description files in lisp-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.md\\'" . lisp-mode))


;; UNDERSCORE is part of a word!
(modify-syntax-entry ?_ "w")
;; c-mode
(add-hook 'c-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
;; c++-mode
(add-hook 'c++-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))

