;; -*- lexical-binding: t -*-
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks nil)

;; Put ltxpng files into one place
(setq org-latex-preview-ltxpng-directory "/tmp/ltxpng/")

;; statistics is about something like this:
;; * TODO Conquer the world [1/2]
;; ** DONE ...
;; ** TODO ...
(setq org-provide-todo-statistics t)
(setq org-hierarchical-todo-statistics nil)
(setq org-hierarchical-checkbox-statistics nil)

;; when searching for something in a folded org-file
(setq org-show-hierarchy-above t)
;; working with a predefined set of tags
(setq org-fast-tag-selection-single-key t)

(setq org-M-RET-may-split-line '((default . nil)))

(add-to-list 'auto-mode-alist '("/TODO\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.TODO\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo\\'" . org-mode))

(setq y-skydrive-path (concat (getenv "HOME") "/SkyDrive"))
(setq yorg-path (concat y-skydrive-path "/GTD"))

;; org-capture
(setq org-default-notes-file (concat yorg-path "/Inbox.todo"))
(setq org-capture-templates
      '(("t" "Incoming [T]asks" entry (file org-default-notes-file)
         "* TODO %?\n  %i\n" :empty-lines 1 :prepend t)))
(setq org-reverse-note-order t)

;; agenda
(defun my-org-agenda-files-refresh ()
  (interactive)
  (setq org-agenda-files
        (apply #'append
               (mapcar (lambda (x) (directory-files-recursively x "^[[:alnum:]].*\\.todo\\'"))
                       (-filter #'file-directory-p
                                (list (concat y-skydrive-path "/PIM")
                                      "/cygdrive/c/Users/nbozheno/Intel/devel"
                                      yorg-path))))))
(my-org-agenda-files-refresh)

(setq org-agenda-todo-ignore-scheduled nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 14)

;; shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(push (list 'org-mode-map) evil-overriding-maps)

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-ctrl-k-protect-subtree t)

;; appearance
(setq org-hide-emphasis-markers t)
(setq org-src-fontify-natively t) ; fonts in src blocks
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
; indent text according to structure
(setq org-startup-indented t)
;(setq org-startup-with-inline-images t)

(setq org-todo-keywords
      '((sequence "PROJECT(p)" "|" "CLOSED(l)" "CANCEL(x)")
        (sequence "WAIT(w)" "TODO(t)" "|" "DONE(d)" "SKIP(s)")
        (sequence "FIXME(m)" "|" "FIXED(f)")))

(setq org-todo-keyword-faces
      '(("FIXME" . "red4")
        ("FIXED" . "#083708")
        ("WAIT" . "chocolate4")
        ("TODO" . "firebrick")
        ("DONE" . "dark green")
        ("SKIP" . "#0052c0")
        ("PROJECT" . "purple")
        ("CLOSED" . "dark green")
        ("CANCEL" . "#404070")))

(setq org-list-description-max-indent 5)

(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (define-key org-agenda-mode-map (kbd "C-C C-p") 'org-agenda-priority)))

(setq org-tag-alist '((:startgroup . nil)
                      ("office" . ?o)
                      ("home" . ?h)
                      ("sveta" . ?s)
                      (:endgroup . nil)))


(defun my-org-agenda-skip-check (type)
  (let* ((tags (org-get-tags-at)))
    (cond
     ;; "all" : skip nothing
     ((string= type "all")
      nil)
     ;; "office" : skip all tagged entries (except ones explicitely tagged with :office:)
     ((string= type "office")
      (or (member-ignore-case "home" tags) (member-ignore-case "sveta" tags)))
     ;; "home" : skip all entries unless they are tagged with :home:
     ((string= type "home")
      (not (member-ignore-case "home" tags)))
     ;; "home" : skip all entries unless they are tagged with :sveta:
     ((string= type "sveta")
      (not (member-ignore-case "sveta" tags)))
     (t
      (error "Invalid agenda type")))))

;; (macroexpand '(my-org-agenda-skip-macro "home" org-agenda-skip-subtree-if 'nottodo '("TODO" "WAIT")))
;; =>
;; (quote
;;  (if (my-org-agenda-skip-check "home")
;;      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;    (org-agenda-skip-subtree-if (quote nottodo) (quote ("TODO" "WAIT")))))
(defmacro my-org-agenda-skip-macro (type func &rest conditions)
  `(quote (if (my-org-agenda-skip-check ,type)
              (,func 'regexp ".*")
            ,(cons func conditions))))

;; (macroexpand '(my-org-agenda-custom-template "xh" "My Home Agenda" "home"))
;; =>
;; ("xh" "My Home Agenda"
;;  ((agenda ""
;;           ((org-agenda-overriding-header "Upcoming Agenda")
;;            (org-agenda-span 8)
;;            (org-agenda-skip-function
;;             (quote (if (my-org-agenda-skip-check "home")
;;                        (org-agenda-skip-entry-if (quote regexp) ".*")
;;                      (org-agenda-skip-entry-if (quote nottodo) (quote ("TODO" "WAIT"))))))))
;;
;;   (todo "PROJECT"
;;         ((org-agenda-overriding-header "Active Projects")
;;          (org-agenda-todo-ignore-scheduled (quote future))
;;          (org-agenda-skip-function
;;           (quote (if (my-org-agenda-skip-check "home")
;;                      (org-agenda-skip-entry-if (quote regexp) ".*")
;;                    (org-agenda-skip-entry-if (quote notregexp) ".*"))))))
;;
;;   (todo "PROJECT|TODO"
;;         ((org-agenda-overriding-header "Incoming Tasks")
;;          (org-agenda-todo-ignore-scheduled t)
;;          (org-agenda-skip-function
;;           (quote (if (my-org-agenda-skip-check "home")
;;                      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;                    (org-agenda-skip-subtree-if (quote todo) (quote ("PROJECT"))))))))
;;
;;   (todo "PROJECT|WAIT"
;;         ((org-agenda-overriding-header "Postponed Tasks")
;;          (org-agenda-todo-ignore-scheduled t)
;;          (org-agenda-skip-function
;;           (quote (if (my-org-agenda-skip-check "home")
;;                      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;                    (org-agenda-skip-subtree-if (quote todo) (quote ("PROJECT"))))))))))
(defmacro my-org-agenda-custom-template (shortcut description type)
  (list
   shortcut
   description
   `((agenda ""
             ((org-agenda-overriding-header "Upcoming Agenda")
              (org-agenda-span 8)
              (org-agenda-skip-function ,(macroexpand
                                          `(my-org-agenda-skip-macro
                                            ,type
                                            org-agenda-skip-entry-if 'nottodo '("TODO" "WAIT"))))))

     (todo "PROJECT"
           ((org-agenda-overriding-header "Active Projects")
            (org-agenda-todo-ignore-scheduled 'future)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-org-agenda-skip-macro
                                          ,type
                                          ;; skip nothing regexp
                                          org-agenda-skip-entry-if 'notregexp ".*")))))

     (todo "PROJECT|TODO" ;; PROJECT is necessary in this list to feed it into skip function
           ((org-agenda-overriding-header "Incoming Tasks")
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-org-agenda-skip-macro
                                          ,type
                                          org-agenda-skip-subtree-if 'todo '("PROJECT"))))))

     (todo "PROJECT|WAIT" ;; PROJECT is necessary in this list to feed it into skip function
           ((org-agenda-overriding-header "Postponed Tasks")
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-org-agenda-skip-macro
                                          ,type
                                          org-agenda-skip-subtree-if 'todo '("PROJECT"))))))
     )))

;; (("y" "My Agenda"
;;   ((agenda ""
;;            ((org-agenda-overriding-header "Upcoming Agenda")
;;             ...))
;;    ...
;;    ))
;;  ("o" "My Office Agenda"
;;   ((agenda ""
;;            ((org-agenda-overriding-header "Upcoming Agenda")
;;             ...))
;;    ...
;;    ))
;;  ("h" "My Home Agenda"
;;   ((agenda ""
;;            ((org-agenda-overriding-header "Upcoming Agenda")
;;             ...))
;;    ...
;;    ))
;;  ("j" "Joint Agenda"
;;   ((agenda ""
;;            ((org-agenda-overriding-header "Upcoming Agenda")
;;             ...))
;;    ...
;;    )))
(setq org-agenda-custom-commands
      `(
        ,(macroexpand
          '(my-org-agenda-custom-template "y" "My Agenda" "all"))
        ,(macroexpand
          '(my-org-agenda-custom-template "o" "My Office Agenda" "office"))
        ,(macroexpand
          '(my-org-agenda-custom-template "h" "My Home Agenda" "home"))
        ,(macroexpand
          '(my-org-agenda-custom-template "j" "Joint Agenda" "sveta"))
        ))


;; Allow bold/emphasis span up to 3+1 lines.
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
(setcar (nthcdr 4 org-emphasis-regexp-components) 2)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; Never insert a blank line between list entries automatically.
;; (setq org-blank-before-new-entry nil)
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

(setq org-highest-priority ?A
      org-lowest-priority  ?D
      org-default-priority ?C)

;; (add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c p") 'org-fill-paragraph)))

;(add-hook 'org-mode-hook 'org-indent-mode)

;; (setq org-agenda-custom-commands
;;       '(
;;         ("r" "Лента задач" todo :todo
;;          ((yorg-replace-org-todo-list (quote yorg-todo-list))))
;;         ("w" "Отложенные задачи" todo :wait
;;          ((yorg-replace-org-todo-list (quote yorg-todo-list))))
;;         ("i" "Inbox" todo :inbox
;;          ((yorg-replace-org-todo-list (quote yorg-todo-list))))
;;         ("c" "Currently clocked" todo :clock
;;          ((yorg-replace-org-todo-list (quote yorg-todo-list))))
;;         ))

;; (setq org-capture-templates
;;       '(
;;         ;; inboxes
;;         ("h" "Domestic chores (home / humdrum)" entry (id 764bbc57-d976-4eec-a9ad-50ec9c0523fa)
;;          "* TODO %?\n  %i\n  %a\n" :empty-lines 1 :prepend t)
;;         ("w" "Todo (work)" entry (id fa97f1f5-f127-4693-a727-99a7c4d7668d)
;;          "* TODO %?\n  %i\n  %a\n" :empty-lines 1 :prepend t)
;;         ("x" "Exceptions" entry (id 3ca98f66-42d2-4cfd-8420-a60904db8be4)
;;          "* TODO %?\n  %i\n  %a\n" :empty-lines 1 :prepend t)
;;         ("e" "Emacs" entry (id 8ea06b29-8452-459e-aa34-4ca91ea4ad7c)
;;          "* TODO %?\n  %i\n  %a\n" :empty-lines 1 :prepend t)
;;         ("u" "Unsorted" entry (file (concat yorg-path "/inbox.org")) ; несортированное разное
;;          "* TODO %?\n  %i\n  %a\n" :empty-lines 1 :prepend t)

;;         ;; journal
;;         ;;("j" "Journal" entry (file+datetree (concat yorg-path "/journal.org"))
;;         ;; "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1 :prepend t)
;;         ("j" "Journal" entry (file (concat yorg-path "/journal.org"))
;;          "* %?\n\n%U\n" :empty-lines 1 :prepend t)

;;         ;; current task
;;         ("c" "Currently clocked task" entry (clock)
;;          "* TODO %?  :note:\n  %i\n  %a" :empty-lines 1 :prepend t)
;;         ))

;; (setq org-agenda-files
;;       (let ((tmpbuf (generate-new-buffer "*tmp:org-agenda-files*"))
;;             retval)
;;         (unwind-protect
;;             (progn
;;               (call-process "find" nil tmpbuf nil
;;                             yorg-path "-name" "*.org" "-and" "-not" "-name" ".*")
;;               (split-string
;;                (with-current-buffer tmpbuf
;;                  (buffer-string))
;;                "\n"))
;;           (kill-buffer tmpbuf))))

;; (org-add-link-type
;;  "semantic" nil
;;  (lambda (path desc format)
;;    (cond
;;     ((eq format 'html)
;;      (format "<code>%s</code>" path desc))
;;     ((eq format 'latex)
;;      (format "\\%s{%s}" path desc))
;;     (t desc))))

;; refile
;; (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10))))
;; (setq org-refile-use-outline-path 'file)

;; (setq org-agenda-menu-show-matcher nil)
;; (setq org-agenda-overriding-columns-format "%TODO %3PRIORITY %25ITEM")

;; didn't understand
;; (setq org-hide-block-overlays t)

;; (setq org-use-property-inheritance t)
;(setq org-log-state-notes-insert-after-drawers t)
;(setq org-log-into-drawer nil)
; (setq org-log-repeat nil)
