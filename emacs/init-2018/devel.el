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

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "\C-cf") 'clang-format)
            (define-key c-mode-base-map (kbd "\C-c\C-c") 'comment-or-uncomment-region)
            ))


;;
;; company-mode
;;

;; For rtags, *.el files should match rtags version.
(dolist (config-file (list "rtags.el" "helm-rtags.el" "company-rtags.el" "flycheck-rtags.el"))
  (load (concat my-emacs-init-dir "/third_party/rtags-2.18/" config-file)))

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)

;; This is a temporary association. It must be revisited later as it is not
;; clear whether a better shortcut (C-n) should be used, and how much useful is
;; the original function bound to M-/.
(define-key global-map (kbd "M-/") nil)
(define-key evil-insert-state-map (kbd "M-/") 'company-complete)


;;
;; Keybindings
;;
(rtags-enable-standard-keybindings global-map)


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
