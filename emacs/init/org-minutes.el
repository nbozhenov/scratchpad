(setq org-html-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<font size=4><code>%s</code></font>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<font size=4><code>%s</code></font>")))

(setq org-html-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>")))

(defun org-colorize-faces-todo-classify (string)
  (let (state)
    (cond ((string-prefix-p "TODO:" string)
           (setq state "wait"))
          ((string-prefix-p "TODO" string)
           (setq state "todo"))
          ((string-prefix-p "WIP" string)
           (setq state "wip"))
          ((string-prefix-p "SKIP" string)
           (setq state "skip"))
          ((string-prefix-p "DONE" string)
           (setq state "done")))
    (list state)))


(defun org-colorize-minutes-get-regexp ()
  (let* ((name-re "\\(Andrei\\|Artur\\|Nikolai\\|Olga\\|Yulia\\|Daniel\\)")
         (pref-re "\\(TODO\\|WIP\\|SKIP\\|DONE\\)")
         (list-re (concat name-re "\\(\\+" name-re "\\)*"))
         (list-gen-re (concat " \\(" list-re "\\|All\\)")))
    (concat pref-re "\\(" list-gen-re "\\)?:")))


(defun org-export-html-colorize-minutes (str)
  (let ((todo-re (org-colorize-minutes-get-regexp)))
    (replace-regexp-in-string
     todo-re 
     (lambda (text)
       (let ((state (car (org-colorize-faces-todo-classify text))))
         (cond ((equal state "todo")
                (concat "<span style=\"color:#cd0000;font-weight:bold;\">"
                        text
                        "</span>"))
               ((equal state "done")
                (concat "<span style=\"color:#228b22;font-weight:bold;\">"
                        text
                        "</span>"))
               ((equal state "wait")
                (concat "<span style=\"color:#8b0000;font-weight:bold;\">"
                        text
                        "</span>"))
               ((equal state "skip")
                (concat "<span style=\"color:#009acd;font-weight:bold;\">"
                        text
                        "</span>"))
               ((equal state "wip")
                (concat "<span style=\"color:#cd2990;font-weight:bold;\">"
                        text
                        "</span>")))))
     str)))



(defun org-do-colorize-faces-todo (limit)
  "Run through the buffer and add overlays to colored text."
  (let ((todo-re (org-colorize-minutes-get-regexp)))
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
                'face `((t (:foreground "maroon3" :weight bold))))))))))


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

(defun org-html-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-html-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; org-minutes.el
    (setq output (org-export-html-colorize-minutes output))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (org-html-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-html-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))

;; Searches for the beginning of a text matching the REGEX and surrounding POINT
;; TODO: optimize regexp
(defun my-re-search-surrounding (pattern limit)
  (let ((pos (point))
        retval)
    (unless limit (setq limit (point-min)))
    (while (and (not retval) (>= pos limit))
      (setq retval (looking-at pattern))
      (unless retval
        (setq pos (1- pos))
        (goto-char pos)))
    retval))

(defun org-minutes-highlight-links--impl (short-link-pattern long-link-pattern)
  (let ((full-link-pattern (concat "\\[\\[" (format long-link-pattern short-link-pattern) "\\]\\[" short-link-pattern "\\]\\]"))
        (initial-position (point))
        (line-begin (line-beginning-position))
        match-begin
        short-link
        org-link-begin
        replacement)
    (when (and (my-re-search-surrounding short-link-pattern line-begin)
               (>= (match-end 0) initial-position))
      ; Remember where the matched string begins.
      (setq match-begin (point))
      ; Try to find a surrounding org-link and bail out if the link in question is already formatted.
      (setq org-link-begin (my-re-search-surrounding full-link-pattern line-begin))
      (when (or (not org-link-begin)
                (< (match-end 0) initial-position))
        (goto-char match-begin)
        (re-search-forward short-link-pattern)
        (setq short-link (match-string 0))
        (setq replacement (format (concat "[[" long-link-pattern "][%s]]") short-link short-link))
        (replace-match replacement t)
        t))))


; TODO: удалить повторы
(defun org-minutes-highlight-links ()
  (interactive)
  (unless (save-excursion (org-minutes-highlight-links--impl "rL[[:digit:]]\\{3,\\}" "https://reviews.llvm.org/%s"))
    (unless (save-excursion (org-minutes-highlight-links--impl "D[[:digit:]]\\{3,\\}" "https://reviews.llvm.org/%s"))
      (unless (save-excursion (org-minutes-highlight-links--impl "LCPT-\\([[:digit:]]+\\)" "https://jira01.devtools.intel.com/browse/%s"))
        (save-excursion (org-minutes-highlight-links--impl "CMPLRS-\\([[:digit:]]+\\)" "https://jira01.devtools.intel.com/browse/%s"))))))

(global-unset-key (kbd "C-x C-l"))
(define-key org-mode-map (kbd "C-x C-l") 'org-minutes-highlight-links)

; (text-scale-increase 2)


;; %s|CMPLRS-[[:digit:]]\{4,6\}|[[https://jira01.devtools.intel.com/browse/\0][\0]]|gc
;; %s|D[[:digit:]]\{4,6\}|[[https://reviews.llvm.org/\0][\0]]|gc
;; %s|rl[[:digit:]]\{4,6\}|[[https://reviews.llvm.org/\0][\0]]|gc
;; %s|CQ\([[:digit:]]\{6\}\)|[[https://jf.clearquest.intel.com/cqweb/restapi/CQMS.DPD.JF/DPD2/RECORD/DPD200\1?format=HTML\&noframes=true\&recordType=Defect][\0]]|gc 
