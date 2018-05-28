;;
;; LLVM
;;
(dolist (config-file (list "emacs.el" "llvm-mode.el" "tablegen-mode.el"))
  (load (concat my-emacs-init-dir "/third_party/llvm/emacs/" config-file)))


;;
;; C/C++
;;

;; c-show-syntactic-information may be usefult to create my own style.
;; As well as example in third_party/llvm/emacs/emacs.el.
(setq c-default-style "llvm.org")

;; Shift preprocessor directives to the left.
(setq c-electric-pound-behavior '(alignleft))

;; open all headers in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Clang-format
(require 'clang-format)
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "\C-cdf") 'clang-format)))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "\C-cdf") 'clang-format)))


;;
;; Keybindings
;;
(define-key global-map (kbd "\C-cdc") 'comment-or-uncomment-region)


;;
;; UNDERSCORE is a part of a word!
;;
(modify-syntax-entry ?_ "w")
(add-hook 'c-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
(add-hook 'c++-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))
