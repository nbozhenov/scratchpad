;;;; Package management
(require 'package)
(setq package-load-list '(all))
;; by default `package-initialize' is called after loading this init file
;; but I want load packages first
(package-initialize)
(setq package-enable-at-startup nil)

;; Adding MELPA package archive (repository)
;; There are official GNU archive at http://elpa.gnu.org/packages/ and 
;; some non-official archives. See http://ergoemacs.org/emacs/emacs_package_system.html
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
