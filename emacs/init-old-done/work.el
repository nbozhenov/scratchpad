;;;;
;; GUI
;;
;; Next code work with Emacs 21.4, 22.3, 23.1.
;; (when window-system
;;   (let (
;;         (px (display-pixel-width))
;;         (py (display-pixel-height))
;;         (fx (frame-char-width))
;;         (fy (frame-char-height))
;;         tx ty
;;         )
;;     ;; Next formulas discovered empiric on Windows host with default font.
;;     (setq tx (- (/ px fx) 7))
;;     (setq ty (- (/ py fy) 4))
;;     (setq tx px ty py)
;;     (setq initial-frame-alist '((top . 2) (left . 2)))
;;     (add-to-list 'initial-frame-alist (cons 'width tx))
;;     (add-to-list 'initial-frame-alist (cons 'height ty))
;;     ) )



;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                        '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                        '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))

;(shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz")

;; (unless (tty-type)
;;   (shell-command
;;    (concat "wmctrl -i -r "
;;            (frame-parameter nil 'outer-window-id)
;;            " -badd,maximized_vert,maximized_horz")))

(defun y-maximize-frame (&optional arg)
  (interactive)
  (set-frame-parameter arg 'fullscreen 'fullwidth)
  (set-frame-parameter arg 'fullscreen 'fullheight)
  (sleep-for 0.1)
  (set-frame-parameter arg 'fullscreen 'maximized))

(add-hook 'after-make-frame-functions 'y-maximize-frame)

;;;;
;; Font
;;
(defun y-setup-fonts (&optional arg)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))

(setq default-frame-alist '((font . "DejaVu Sans Mono-10")))
