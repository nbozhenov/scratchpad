;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(setq on-windows-p
      (if (member system-type '(cygwin windows-nt)) t nil))

(defvar my-emacs-dir "~/z/scratchpad/emacs")
(defvar my-emacs-init-dir (concat my-emacs-dir "/init-2018"))

; (add-to-list 'load-path (concat my-emacs-dir "/elib"))
; (require 'y-hs)

;; For rtags, *.el files should match rtags version.
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
      (add-to-list 'load-path (concat my-emacs-init-dir "/third_party/" package-name)))))

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
