;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar my-emacs-dir "~/z/scratchpad/emacs")
(defvar my-emacs-init-dir (concat my-emacs-dir "/init-2018"))

(add-to-list 'load-path (concat my-emacs-dir "/elib"))
(require 'y-hs)

(defvar my-emacs-init-files
  (list
   "core"
   "devel"
   ; "devel-gcc"
   "evil"
   "helm"
   "misc"
   ))

(dolist (config-file my-emacs-init-files)
  (load (concat my-emacs-init-dir "/" config-file)))
