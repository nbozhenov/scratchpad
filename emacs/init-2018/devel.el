;;
;; C-mode styles and formatting.
;;

;; open all headers in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; You can determine which offset to edit by hitting [C-c C-s] on any line.
;; C-c C-o runs the command c-set-offset.
(c-add-style "my-c-style"
             '("linux"
               (c-basic-offset . 2)
               (c-offsets-alist . ((access-label . -2)
                                   (case-label . 2)
                                   (innamespace . [0])
                                   (statement-case-intro . 2)))))
(setq c-default-style "my-c-style")

;; Shift preprocessor directives to the left.
(setq c-electric-pound-behavior '(alignleft))

;; Clang-format
(require 'clang-format)
(define-key global-map (kbd "\C-cdf") 'clang-format)


;; UNDERSCORE is a part of a word!
(modify-syntax-entry ?_ "w")
(add-hook 'c-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
(add-hook 'c++-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))
