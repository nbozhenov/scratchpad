;; Based on https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00224.html

(defun org-colorize-faces-todo-classify (string)
  (let (state)
    (cond ((string-prefix-p "TODO:" string)
           (setq state "wait"))
          ((string-prefix-p "TODO" string)
           (setq state "todo"))
          ((string-prefix-p "WIP" string)
           (setq state "wip"))
          ((string-prefix-p "DONE" string)
           (setq state "done")))
    (list state)))


(defun org-do-colorize-faces-todo (limit)
  "Run through the buffer and add overlays to colored text."
  (let* ((name-re "\\(Andrei\\|Artur\\|Nikolai\\|Olga\\)")
         (pref-re "\\(TODO\\|WIP\\|DONE\\)")
         (list-re (concat name-re "\\(\\+" name-re "\\)*"))
         (list-gen-re (concat " \\(" list-re "\\|All\\)"))
         (todo-re (concat pref-re "\\(" list-gen-re "\\)?:")))
    (while (re-search-forward todo-re limit t)
      (let* ((whole (match-string 0))
             (state (car (org-colorize-faces-todo-classify whole))))
        (cond ((equal state "todo")
               (font-lock-prepend-text-property
                (match-beginning 0) (match-end 0)
                'face `((t (:foreground "Red3" :weight bold)))))
              ((equal state "done")
               (font-lock-prepend-text-property
                (match-beginning 0) (match-end 0)
                'face `((t (:foreground "ForestGreen" :weight bold)))))
              ((equal state "wait")
               (font-lock-prepend-text-property
                (match-beginning 0) (match-end 0)
                'face `((t (:foreground "Red4" :weight bold)))))
              ((equal state "wip")
               (font-lock-prepend-text-property
                (match-beginning 0) (match-end 0)
                'face `((t (:foreground "Blue3" :weight bold))))))))))


(defun org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let* ((em org-fontify-emphasized-text)
         (lk org-activate-links)
         (org-font-lock-extra-keywords
          (list
           ;; Call the hook
           '(org-font-lock-hook)
           ;; Headlines
           `(,(if org-fontify-whole-heading-line
                  "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
                "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
             (1 (org-get-level-face 1))
             (2 (org-get-level-face 2))
             (3 (org-get-level-face 3)))
           ;; Table lines
           '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
             (1 'org-table t))
           ;; Table internals
           '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
           '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
           '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
           '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
           ;; Drawers
           (list org-drawer-regexp '(0 'org-special-keyword t))
           (list "^[ \t]*:END:" '(0 'org-special-keyword t))
           ;; Properties
           (list org-property-re
                 '(1 'org-special-keyword t)
                 '(3 'org-property-value t))
           ;; Links
           (if (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
           (if (memq 'angle lk) '(org-activate-angle-links (0 'org-link t)))
           (if (memq 'plain lk) '(org-activate-plain-links (0 'org-link t)))
           (if (memq 'bracket lk) '(org-activate-bracket-links (0 'org-link t)))
           (if (memq 'radio lk) '(org-activate-target-links (0 'org-link t)))
           (if (memq 'date lk) '(org-activate-dates (0 'org-date t)))
           (if (memq 'footnote lk) '(org-activate-footnote-links))
           ;; Targets.
           (list org-any-target-regexp '(0 'org-target t))
           ;; Diary sexps.
           '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
           ;; Macro
           '("{{{.+}}}" (0 'org-macro t))
           '(org-hide-wide-columns (0 nil append))
           ;; TODO keyword
           (list (format org-heading-keyword-regexp-format
                         org-todo-regexp)
                 '(2 (org-get-todo-face 2) t))
           ;; DONE
           (if org-fontify-done-headline
               (list (format org-heading-keyword-regexp-format
                             (concat
                              "\\(?:"
                              (mapconcat 'regexp-quote org-done-keywords "\\|")
                              "\\)"))
                     '(2 'org-headline-done t))
             nil)
           ;; Priorities
           '(org-font-lock-add-priority-faces)
           ;; Tags
           '(org-font-lock-add-tag-faces)
           ;; Tags groups
           (if (and org-group-tags org-tag-groups-alist)
               (list (concat org-outline-regexp-bol ".+\\(:"
                             (regexp-opt (mapcar 'car org-tag-groups-alist))
                             ":\\).*$")
                     '(1 'org-tag-group prepend)))
           ;; Special keywords
           (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
           (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
           (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
           (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
           ;; Emphasis
           (if em
               (if (featurep 'xemacs)
                   '(org-do-emphasis-faces (0 nil append))
                 '(org-do-emphasis-faces)))
           '(org-do-colorize-faces-todo)
           ;; Checkboxes
           '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
             1 'org-checkbox prepend)
           (if (cdr (assq 'checkbox org-list-automatic-rules))
               '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                 (0 (org-get-checkbox-statistics-face) t)))
           ;; Description list items
           '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
             1 'org-list-dt prepend)
           ;; ARCHIVEd headings
           (list (concat
                  org-outline-regexp-bol
                  "\\(.*:" org-archive-tag ":.*\\)")
                 '(1 'org-archived prepend))
           ;; Specials
           '(org-do-latex-and-related)
           '(org-fontify-entities)
           '(org-raise-scripts)
           ;; Code
           '(org-activate-code (1 'org-code t))
           ;; COMMENT
           (list (format org-heading-keyword-regexp-format
                         (concat "\\("
                                 org-comment-string "\\|" org-quote-string
                                 "\\)"))
                 '(2 'org-special-keyword t))
           ;; Blocks and meta lines
           '(org-fontify-meta-lines-and-blocks))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (org-set-local 'org-font-lock-keywords org-font-lock-extra-keywords)
    (org-set-local 'font-lock-defaults
                   '(org-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords) nil))
