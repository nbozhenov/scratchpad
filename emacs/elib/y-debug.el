;; -*- lexical-binding: t -*-

(defun y-dbg-msg (msg)
  (save-excursion
    (set-buffer (get-buffer-create "y-dbg"))
    (goto-char (point-max))
    (insert msg)))


(provide 'y-debug)

