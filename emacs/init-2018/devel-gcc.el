(defun my-gcc-hook ()
  (when (string-match "/gcc/" (or buffer-file-name ""))
    (setq indent-tabs-mode t)
    (c-set-style "gnu" nil)))

(add-hook 'c++-mode-hook 'my-gcc-hook)

;; open all gcc sources in c++-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.c\\'" . c++-mode))
;; open machine description files in lisp-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.md\\'" . lisp-mode))
