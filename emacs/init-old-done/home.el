(require 'y-debug)

(defun y-fit-frame-size (&optional arg)
  (interactive)
  (set-frame-position arg 0 0)
  (set-frame-parameter arg 'fullscreen 'fullheight)
  (set-frame-width arg 120))

(add-hook 'after-make-frame-functions 'y-fit-frame-size)

