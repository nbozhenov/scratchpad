(defun my-pim/local-projects (project-list)
  (setq org-publish-project-alist nil)
  (dolist (proj project-list)
    (let ((name (nth 0 proj))  ; project name
          (abbr (nth 1 proj))  ; project abbreviation
          (sdir (nth 2 proj))  ; project base directory
          (pdir (nth 3 proj))) ; project publishing directory
      (my-pim/setup-publish name sdir pdir))))


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
