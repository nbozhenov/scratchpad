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
(if (file-exists-p yorg-path)
  (setq org-agenda-files (directory-files yorg-path t "\\.todo\\'")))
(setq org-agenda-todo-ignore-scheduled t)
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

;; TODO keywords
(setq org-todo-keywords
      '((sequence "FIXME"          ;; <- not a TODO item, just a remark that this area hasn't been
                                   ;;    fully explored
                  "NOTE(n)"        ;; <- nota bene (заметка), тонкое место, к-рое следует учесть
                                   ;;    при выполнении родительской задачи
                  "TODO(t)"        ;; <- текущая задача
                  "CURR(c)"        ;; <- work in progress
                  "WAIT(w)"        ;; <- к выполнению задачи можно будет приступить
                                   ;;    после выполнения некоторых условий
                                   ;;    (в эту категорию попадают заблокированные задачи)
              "|" "DONE(d)"
                  "SKIP(s)")))

(setq org-todo-keyword-faces
      '(("FIXME" . "dark orange")
        ; ("NOTE" . "dark red")
        ("NOTE" . "orange red")
        ("TODO" . "firebrick")
        ("CURR" . "purple")
        ("WAIT" . "chocolate4")
        ("SKIP" . (:foreground "DeepSkyBlue3" :weight bold))))

(setq org-list-description-max-indent 5)

(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (define-key org-agenda-mode-map (kbd "C-C C-p") 'org-agenda-priority)))

(setq org-agenda-custom-commands
      (quote
       (("h" "Home Agenda" ; taglist contains :home:
         ((agenda "" ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'notregexp ":home:"))))))
        ("o" "Office Agenda" ; taglist DOESN'T contain :home:
         ((agenda "" ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'regexp ":home:")))))))))

(setq org-agenda-custom-commands
      (quote
       (("h" "Home Agenda" ; taglist contains :home:
         ((agenda "" ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'notregexp ":home:"))))))
         ("s" "Joint Agenda" ; taglist contains :svetlana:
          ((agenda "" ((org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'notregexp ":svetlana:"))))))
         ("o" "Office Agenda" ; taglist contains neither :home:, nor :svetlana:
          ((agenda "" ((org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'regexp ":\\(home\\|svetlana\\):")))))))))


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
