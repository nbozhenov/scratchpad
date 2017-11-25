;; -*- lexical-binding: t -*-


(defvar my-emacs-dir "~/z/scratchpad/emacs")
(defvar my-init-dir (concat my-emacs-dir "/init"))

(add-to-list 'load-path (concat my-emacs-dir "/elib"))

(defvar my-init-files
      (list
       "keyboard"
       "elpa"
       "evil"
       ;"elscreen"
       ;"y-elscreen"
       "info"
       "coding"
       "tags"
       "yorg"
       "org"
       ; "org-minutes"
       "customizations"
       "facilities"
       ;"sdcv"
       "xclip"
       "llvm/emacs"
       "llvm/llvm-mode"
       "llvm/tablegen-mode"
       ))

(if (equal (user-real-login-name) "bozhenov_n")
    (setq my-init-files (append my-init-files (list "work")))
  (setq my-init-files (append my-init-files (list "home"))))

(dolist (config-file my-init-files)
  (load (concat my-init-dir "/" config-file)))

(require 'helm)

(defun update-display-env ()
  (setenv "DISPLAY"
          (with-temp-buffer
            (insert-file-contents
             (concat (getenv "HOME") "/.display.txt"))
            (buffer-string))))

(run-with-timer 20 20 'update-display-env)
