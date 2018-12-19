;;
;; org-mode
;;

(defun my-init/org/pkg-config ()
  ;; Controls
  (setq org-special-ctrl-k t)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  ;; Appearance
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t) ; highlighting in src blocks
  (setq org-startup-indented t)
  (setq org-list-description-max-indent 5)

  ;; Allow bold/emphasis span up to 3+1 lines.
  ;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
  (setcar (nthcdr 4 org-emphasis-regexp-components) 3)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Dependencies
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

  ;; Refile
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; Outlook integration
  (setq outlook-executable
        "/cygdrive/c/Program Files (x86)/Microsoft Office/Office15/OUTLOOK.EXE")
  (org-add-link-type
   "outlook"
   (lambda (id) (w32-shell-execute "open" outlook-executable
                                   (concat "outlook:" id))))

) ; my-init/org/pkg-config


(use-package org
  :mode (("\\.todo$" . org-mode))
  :config (my-init/org/pkg-config))


;;
;; org-agenda
;;

(defun my-org-agenda-files-refresh ()
  (interactive)
  (let* ((envvar (or (getenv "ORG_AGENDA_PATH") ""))
         (path (split-string envvar ":" t))
         (files (apply
                 #'append
                 (mapcar
                  (lambda (x) (directory-files-recursively x "^[[:alnum:]].*\\.todo\\'"))
                  path)))
         (inbox (find-if (lambda (x) (string-match "/Inbox.todo\\'" x)) files)))
    (setq org-agenda-files files)
    (setq org-default-notes-file inbox)))


(defun my-init/org-agenda/pkg-config ()
  (my-org-agenda-files-refresh)

  ;; basic config
  (setq org-agenda-todo-ignore-scheduled nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-span 14)

  ;; org-capture
  (setq org-capture-templates
        '(("t" "Incoming [T]asks" entry (file org-default-notes-file)
           "* TODO %?\n  %i\n" :empty-lines 1 :prepend t)))
  (setq org-reverse-note-order t)
) ; my-init/org-agenda/pkg-config


(defun my-init/org-agenda/setup-calendar ()
  (setq calendar-week-start-day 1)

  (org-defkey org-read-date-minibuffer-local-map "\C-p"
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-day 1))))
  (org-defkey org-read-date-minibuffer-local-map "\C-n"
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-day 1))))

  (org-defkey org-read-date-minibuffer-local-map (kbd "C-'")
              (lambda () (interactive)
                ;; Go to the beginning of the prompt.
                (goto-char (line-beginning-position))
                (kill-line)
                (org-eval-in-calendar '(calendar-goto-today))
                (insert (format-time-string "%H:%M" (time-add (current-time) 900)))))

) ; my-init/org-agenda/setup-calendar


(defun my-init/org-agenda/skip-func (type)
  (let* ((tags (org-get-tags-at)))
    (cond
     ;; "all" : skip nothing
     ((string= type "all")
      nil)
     ;; "office" : skip all tagged entries (except ones explicitely tagged with :office:)
     ((string= type "office")
      (not (member-ignore-case "office" tags)))
     ;; "home" : skip :office: entries
     ((string= type "home")
      (not (member-ignore-case "home" tags)))
     ;; "sveta" : skip all entries unless they are tagged with :sveta:
     ((string= type "sveta")
      (not (member-ignore-case "sveta" tags)))
     (t
      (error "Invalid agenda type")))))


;; (macroexpand
;;  '(my-init/org-agenda/skip-macro "home"
;;      org-agenda-skip-subtree-if 'nottodo '("TODO" "WAIT")))
;; =>
;; (quote
;;  (if (my-init/org-agenda/skip-func "home")
;;      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;    (org-agenda-skip-subtree-if (quote nottodo) (quote ("TODO" "WAIT")))))
(defmacro my-init/org-agenda/skip-macro (type func &rest conditions)
  `(quote (if (my-init/org-agenda/skip-func ,type)
              (,func 'regexp ".*")
            ,(cons func conditions))))


;; (macroexpand '(my-init/org-agenda/custom-template "h" "My Home Agenda" "home"))
;; =>
;; ("h" "My Home Agenda"
;;  ((agenda ""
;;           ((org-agenda-overriding-header "Upcoming Agenda")
;;            (org-agenda-span 8)
;;            (org-agenda-skip-function
;;             (quote (if (my-init/org-agenda/skip-func "home")
;;                        (org-agenda-skip-entry-if (quote regexp) ".*")
;;                      (org-agenda-skip-entry-if (quote nottodo) (quote ("TODO" "WAIT"))))))))
;;
;;   (todo "PROJECT"
;;         ((org-agenda-overriding-header "Active Projects")
;;          (org-agenda-todo-ignore-scheduled (quote future))
;;          (org-agenda-todo-ignore-time-comparison-use-seconds t)
;;          (org-agenda-skip-function
;;           (quote (if (my-init/org-agenda/skip-func "home")
;;                      (org-agenda-skip-entry-if (quote regexp) ".*")
;;                    (org-agenda-skip-entry-if (quote notregexp) ".*"))))))
;;
;;   (todo "PROJECT|TODO"
;;         ((org-agenda-overriding-header "Incoming Tasks")
;;          (org-agenda-todo-ignore-scheduled t)
;;          (org-agenda-skip-function
;;           (quote (if (my-init/org-agenda/skip-func "home")
;;                      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;                    (org-agenda-skip-subtree-if (quote todo) (quote ("PROJECT"))))))))
;;
;;   (todo "PROJECT|WAIT"
;;         ((org-agenda-overriding-header "Postponed Tasks")
;;          (org-agenda-todo-ignore-scheduled t)
;;          (org-agenda-skip-function
;;           (quote (if (my-init/org-agenda/skip-func "home")
;;                      (org-agenda-skip-subtree-if (quote regexp) ".*")
;;                    (org-agenda-skip-subtree-if (quote todo) (quote ("PROJECT"))))))))))
(defmacro my-init/org-agenda/custom-template (shortcut description type)
  (list
   shortcut
   description
   `((agenda ""
             ((org-agenda-overriding-header "Upcoming Agenda")
              (org-agenda-span 8)
              (org-agenda-skip-function ,(macroexpand
                                          `(my-init/org-agenda/skip-macro
                                            ,type
                                            ;; TODO: add CURR keyword
                                            org-agenda-skip-entry-if 'todo '("PROJECT"))))))

     (todo "PROJECT"
           ((org-agenda-overriding-header "Active Projects")
            (org-agenda-todo-ignore-scheduled 'future)
            (org-agenda-todo-ignore-time-comparison-use-seconds t)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-init/org-agenda/skip-macro
                                          ,type
                                          ;; skip nothing regexp
                                          org-agenda-skip-entry-if 'notregexp ".*")))))

     (todo "PROJECT|TODO" ;; PROJECT is necessary in this list to feed it into skip function
           ((org-agenda-overriding-header "Incoming Tasks")
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-todo-ignore-deadlines 'all)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-init/org-agenda/skip-macro
                                          ,type
                                          org-agenda-skip-subtree-if 'todo '("PROJECT"))))))

     (todo "PROJECT|WAIT" ;; PROJECT is necessary in this list to feed it into skip function
           ((org-agenda-overriding-header "Postponed Tasks")
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-todo-ignore-deadlines 'all)
            (org-agenda-skip-function ,(macroexpand
                                        `(my-init/org-agenda/skip-macro
                                          ,type
                                          org-agenda-skip-subtree-if 'todo '("PROJECT"))))))
     )))


(defun my-init/org-agenda/setup-gtd ()
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

  (setq org-tag-alist '((:startgroup . nil)
                        ("home" . ?h)
                        ("office" . ?o)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("sveta" . ?s)
                        (:endgroup . nil)))

  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?C)

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
            '(my-init/org-agenda/custom-template "y" "My Agenda" "all"))
          ,(macroexpand
            '(my-init/org-agenda/custom-template "o" "My Office Agenda" "office"))
          ,(macroexpand
            '(my-init/org-agenda/custom-template "h" "My Home Agenda" "home"))
          ,(macroexpand
            '(my-init/org-agenda/custom-template "j" "Joint Agenda" "sveta"))
          ))
) ; my-init/org-agenda/setup-gtd


(use-package org-agenda
  :if (getenv "ORG_AGENDA_PATH")
  :bind (("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda)
         :map org-agenda-mode-map
         ("j" . 'org-agenda-next-line)
         ("k" . 'org-agenda-previous-line))
  :config
  (my-init/org-agenda/pkg-config)
  (my-init/org-agenda/setup-calendar)
  (my-init/org-agenda/setup-gtd))
