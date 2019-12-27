;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;
;; My configuration variables
;;
(defvar my-emacs-dir "~/z/scratchpad/emacs")
(defvar my-emacs-init-dir (concat my-emacs-dir "/init-2018"))
;(add-to-list 'load-path (concat my-emacs-dir "/elib"))
;(require 'y-hs)

; Some configurations needs to be done differently on different machines.
(defvar my-hostname (shell-command-to-string "hostname"))
(defvar my-on-windows-p (if (member system-type '(cygwin windows-nt)) t nil))

;; Package archive.
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; Most packages are installed by 'use-package using :ensure keyword. However,
; the use-package itself needs to be installed manually.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar my-emacs-init-files
  (list
   "core"
   "devel"
   "evil"
   "helm"
   "misc"
   "org"
   "mypim"
   ))

(dolist (config-file my-emacs-init-files)
  (load (concat my-emacs-init-dir "/" config-file)))
