(defvar org-drill-input-mode-map (make-sparse-keymap))
(define-key org-drill-input-mode-map "\C-c'" 'org-drill-take-input-finish)
;;(define-key org-drill-input-mode-map "\C-c \C-k" 'org-drill-interrupt-input)

(define-minor-mode org-drill-input-mode
  "Minor mode for buffers supposed to take user's answer."
  nil "org-drill-input-mode" org-drill-input-mode-map)

(defun org-drill-take-input-start ()
  (interactive)
  (setq buffer (generate-new-buffer "*org-drill-input*"))
  (switch-to-buffer-other-window buffer)
  (org-drill-input-mode))

(defun org-drill-take-input-finish ()
  (interactive)
  (let (retval)
    (setq retval (buffer-string))
    (kill-buffer-and-window)
    retval))


(defun org-drill-common-length (lhs rhs)
  (if (and (not (equal "" lhs))
           (not (equal "" rhs))
           (equal (capitalize (elt lhs 0))
                  (capitalize (elt rhs 0))))
      (1+ (org-drill-common-length (substring lhs 1) (substring rhs 1)))
    0))

(defun org-drill-find-longest-substring (standard input)
  (let ((max-len 0)
        (pos-s 0)
        (pos-i 0))

    (dotimes (i (length standard))
      (dotimes (j (length input))
        (let ((common (org-drill-common-length (substring standard i) (substring input j))))
          (when (< max-len common)
            (setq max-len common
                  pos-s i
                  pos-i j)))))

    (if (eq 0 max-len)
        nil
      (list (substring standard 0 pos-s)
            (substring input 0 pos-i)
            (substring input pos-i (+ pos-i max-len))
            (substring standard (+ pos-s max-len))
            (substring input (+ pos-i max-len))))))
  
(defun org-drill-diff-strings (standard input)
  (destructuring-bind (before-s before-i match after-s after-i)
      (or (org-drill-find-longest-substring standard input) '(nil nil nil nil nil))
    (if match
        (concat (org-drill-diff-strings before-s before-i)
                (propertize match 'face '(:background "green"))
                (org-drill-diff-strings after-s after-i))
      (propertize input 'face '(:background "red")))))

;; (y-dbg-msg (format "\n%s\n" (org-drill-diff-strings "abcdef" "kabcdgef")))
;; (y-dbg-msg (format "\n%s\n" (org-drill-diff-strings "Kill'em all!" "to be or not to be? what's the question!")))
;; (y-dbg-msg (format "\n%s\n" (org-drill-diff-strings "My mother was a witch. She was burned alive!" "Nero BURNS like a hell!")))

      
