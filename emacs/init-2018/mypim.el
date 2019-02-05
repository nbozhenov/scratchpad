(require 'org)

(defun my-pim/local-projects (project-list)
  (my-init/org-capture/pkg-config)
  (my-init/org-agenda/setup-gtd)
  (setq org-publish-project-alist nil)

  (let (curr-group-abbr)
    (dolist (proj project-list)
      (cond
       ; startgroup
       ((eq (car proj) :startgroup)
        (let ((group-name (nth 1 proj))
              (group-abbr (nth 2 proj)))
          (setq curr-group-abbr group-abbr)
          (my-pim/setup-capture-startgroup group-name group-abbr)
          (my-pim/setup-agenda-startgroup group-name group-abbr)))
       ; endgroup
       ((eq (car proj) :endgroup)
        (setq curr-group-abbr nil))
       ; project description
       (t
        (let ((name (nth 0 proj))  ; project name
              (abbr (nth 1 proj))  ; project abbreviation
              (sdir (nth 2 proj))  ; project base directory
              (pdir (nth 3 proj))) ; project publishing directory
          (when (file-exists-p sdir)
            (my-pim/setup-capture name (concat curr-group-abbr abbr) sdir)
            (my-pim/setup-agenda name (concat curr-group-abbr abbr) sdir)
            (my-pim/setup-publish name sdir pdir))))))))


(defun my-pim/setup-capture (name abbr sdir)
  (let ((new-entry `(,abbr ,name entry (file ,(concat sdir "/Inbox.todo"))
                           "* TODO %?\n  %i\n" :empty-lines 1 :prepend t)))
    (setq org-capture-templates (cons new-entry org-capture-templates))))


(defun my-pim/setup-capture-startgroup (name abbr)
  ;; FIXME: According to documentation, startgroup should precede group
  ;; elements, but in my implementation the list gets actually reversed.
  ;; However, reversing this list would also reverse the original
  ;; org-capture-templates value (if any), which is undesirable.
  (setq org-capture-templates (cons (list abbr name) org-capture-templates)))


(defun my-pim/setup-agenda (name abbr sdir)
  (let* ((agenda-files
          (directory-files-recursively sdir "\\.\\(todo\\|org\\)\\'"))
         (custom
          `(,abbr ,name todo "TODO|WAIT"
                  ((org-agenda-overriding-header ,(concat name ": Backlog"))
                   (org-agenda-files (quote ,agenda-files))))))
    (setq org-agenda-custom-commands
          (cons custom org-agenda-custom-commands))))


(defun my-pim/setup-agenda-startgroup (name abbr)
  (setq org-agenda-custom-commands
        (cons (cons abbr name) org-agenda-custom-commands)))


(defun my-pim/setup-publish (name sdir pdir)
  (let* ((pub-org-name (concat name "-org"))
         (pub-copy-name (concat name "-copy"))
         (pub-org (list pub-org-name
                        :base-directory sdir
                        :base-extension "org"
                        :recursive t
                        :publishing-directory pdir
                        :publishing-function #'org-html-publish-to-html
                        :section-numbers nil
                        :makeindex t
                        :table-of-contents t))
         (pub-copy (list pub-copy-name
                         :base-directory sdir
                         :base-extension "png\\|svg\\|css"
                         :recursive t
                         :publishing-directory pdir
                         :publishing-function #'org-publish-attachment))
         (pub-all (list name :components (list pub-org-name pub-copy-name))))
    (dolist (iter (list pub-copy pub-org pub-all))
      (setq org-publish-project-alist (cons iter org-publish-project-alist)))))


(defun my-pim/publish (project)
  (interactive
   (list (completing-read
          "Publish project: "
          (-filter
           (lambda (x) (let* ((name (car x)))
                         (not (or (string-suffix-p "-org" name)
                                  (string-suffix-p "-copy" name)))))
           org-publish-project-alist)
          nil t)))
  (require 'org)
  (org-publish project))


(let ((pim-cfg-file "~/.MyPIM.el"))
  (when (file-exists-p pim-cfg-file)
    (load-file pim-cfg-file)))
