;;
;; LLVM
;;
(dolist (config-file (list "emacs.el" "llvm-mode.el" "tablegen-mode.el"))
  (load (concat my-emacs-init-dir "/third_party/llvm/emacs/" config-file)))


;;
;; GCC
;;
(add-hook
 'c++-mode-hook
 (lambda ()
   (when (string-match "/gcc/" (or buffer-file-name ""))
     (setq indent-tabs-mode t)
     (c-set-style "gnu" nil))))

;; open all gcc sources in c++-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.c\\'" . c++-mode))
;; open machine description files in lisp-mode
(add-to-list 'auto-mode-alist '("gcc.*\\.md\\'" . lisp-mode))


;;
;; C/C++
;;

;; c-show-syntactic-information may be usefult to create my own style.
;; As well as example in third_party/llvm/emacs/emacs.el.
;; You can determine which offset to edit by hitting C-c C-s on any line.
;; C-c C-o runs the command c-set-offset
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
;; For rtags, *.el files should match rtags version.
(defvar my-init/rtags/installed-p nil)
(defun my-init/rtags/setup-load-path ()
  (setq my-init/rtags/installed-p t)
  (with-temp-buffer
    (condition-case nil
        (call-process "rc" nil t nil "--version")
      (error (setq my-init/rtags/installed-p nil)))
    (when my-init/rtags/installed-p
      (let* ((rdm-version-full (buffer-string))
             (components (split-string rdm-version-full "\\." nil "[[:space:]]*"))
             (major (car components))
             (minor (car (cdr components)))
             (package-name (concat "rtags-" major "." minor)))
        (add-to-list 'load-path (concat my-emacs-init-dir "/third_party/"
                                        package-name)))))
  my-init/rtags/installed-p)

(use-package rtags
  :ensure nil
  :if (my-init/rtags/setup-load-path)
  :after (c-mode)
  :config
  ; (require 'rtags)
  (require 'helm-rtags)
  (require 'company-rtags)
  (require 'flycheck-rtags)
  (rtags-enable-standard-keybindings global-map)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
)


;;
;; company-mode
;;
(use-package company
  :config
  (global-company-mode))


;;
;; flycheck-mode
;;
(use-package flycheck
  :defer ; the package is loaded when cc-mode first used.
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (flycheck-mode 1)
              ; (flycheck-select-checker 'rtags)
              (setq-local flycheck-check-syntax-automatically '(save))))
)


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


;;
;; Diff
;;

;; No pop-up window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
