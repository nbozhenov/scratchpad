;; TODO: add a keymap into the mode (to format links)
;; TODO: add hooks for org-html-export-*
(define-minor-mode org-minutes-mode
  "Mode to properly highlight participants names in minutes."
  nil nil nil
  (if org-minutes-mode
      (org-minutes-add-keywords)
    (org-minutes-remove-keywords))
  (font-lock-flush))

;; Merging fontification of overlapping font-lock-keywords may be confusing if
;; no append/prepend symbols are provided. For example, in the following code
;; only the first fontification has effect:
;;
;; ("TODO:" (0 org-minutes-wait-face) prepend)
;; ("^\\(\\**\\)\\(\\* \\)\\(.*\\)"
;;  (1
;;   (org-get-level-face 1))
;;  (2
;;   (org-get-level-face 2))
;;  (3
;;   (org-get-level-face 3)))
;;
;; while the following sample works as expected:
;;
;; ("^\\(\\**\\)\\(\\* \\)\\(.*\\)"
;;  (1
;;   (org-get-level-face 1))
;;  (2
;;   (org-get-level-face 2))
;;  (3
;;   (org-get-level-face 3)))
;; ("TODO:" (0 org-minutes-wait-face) prepend)
;;
;; This combination works regardless of the keyword order:
;;
;; ("^\\(\\**\\)\\(\\* \\)\\(.*\\)"
;;  (1
;;   (org-get-level-face 1) append)
;;  (2
;;   (org-get-level-face 2) append)
;;  (3
;;   (org-get-level-face 3) append))
;; ("TODO:" (0 org-minutes-wait-face) prepend)
;;
(defun org-minutes-add-keywords ()
  ; org-minutes-participants shouldn't be modified between enabling/disabling
  ; the mode. Otherwise, not all keywords will be removed when the mode is
  ; unloaded.
  (make-local-variable 'org-minutes-participants)
  (setq org-minutes-participants (org-minutes-get-participants))
  ; Dirty hack:
  ; 1) Remove (and remember) all the current font-lock-keywords.
  ; 2) Combine old and new keywords (as if org-minutes-keywords were added first).
  ; 3) Restore font-lock-keywords.
  (let* ((oldkwds (if (eq t (car font-lock-keywords))
                      (cdr (cdr font-lock-keywords))
                    font-lock-keywords))
         (newkwds (org-minutes-get-keywords)))
    (setq font-lock-keywords nil)
    (font-lock-add-keywords nil (append oldkwds newkwds))))

(defun org-minutes-remove-keywords ()
  (font-lock-remove-keywords nil (org-minutes-get-keywords)))

(defun org-minutes-get-keywords ()
  (let ((retval nil))
    (setq retval
          (cons (list (org-minutes-get-participants-re "TODO")
                      (list 0 'org-minutes-todo-face 'prepend))
                retval))
    (setq retval
          (cons (list (org-minutes-get-participants-re "WIP")
                      (list 0 'org-minutes-wip-face 'prepend))
                retval))
    (setq retval
          (cons (list (org-minutes-get-participants-re "DONE")
                      (list 0 'org-minutes-done-face 'prepend))
                retval))
    (setq retval
          (cons (list "TODO:"
                      (list 0 'org-minutes-wait-face 'prepend))
                retval))
    (setq retval
          (cons (list "DONE:"
                      (list 0 'org-minutes-done-face 'prepend))
                retval))
    (setq retval
          (cons (list "SKIP:"
                      (list 0 'org-minutes-skip-face 'prepend))
                retval))))

(defun org-minutes-get-participants ()
  (save-excursion
    (let ((retval nil))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+PARTICIPANTS:\\(.*\\)" nil t)
        (setq retval (append retval (split-string (match-string 1)))))
      retval)))

(defun org-minutes-get-participants-re (prefix)
  (concat prefix " \\("
          (mapconcat 'identity org-minutes-participants "\\|")
          "\\):"))

(defface org-minutes-todo-face '((t :weight bold :foreground "Red3")) "")
(defface org-minutes-wip-face  '((t :weight bold :foreground "maroon3")) "")
(defface org-minutes-wait-face '((t :weight bold :foreground "Red4")) "")
(defface org-minutes-done-face '((t :weight bold :foreground "ForestGreen")) "")
(defface org-minutes-skip-face '((t :weight bold :foreground "DeepSkyBlue3")) "")

(defvar org-minutes-todo-face 'org-minutes-todo-face "")
(defvar org-minutes-wip-face  'org-minutes-wip-face "")
(defvar org-minutes-wait-face 'org-minutes-wait-face "")
(defvar org-minutes-done-face 'org-minutes-done-face "")
(defvar org-minutes-skip-face 'org-minutes-skip-face "")
