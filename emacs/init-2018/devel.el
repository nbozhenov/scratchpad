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
;; RTAGS
;;
(when my-init/rtags/installed-p
  (require 'rtags)
  (require 'helm-rtags)
  (require 'company-rtags)
  (require 'flycheck-rtags)
  (rtags-enable-standard-keybindings global-map))


;;
;; company-mode
;;
(when my-init/rtags/installed-p
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends))
(global-company-mode)


;;
;; flycheck-mode
;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (flycheck-mode 1)
            (flycheck-select-checker 'rtags)
            (setq-local flycheck-check-syntax-automatically '(save))))


;;
;; Octave
;;
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;;
;; Bash
;;
(use-package sh-script
  :defer
  :config
  (setq sh-basic-offset 2))
