(defun y-find-file (filename &optional wild)
  "Find new file for the current client when in server-client mode."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (if (and proc (processp proc))
        (switch-to-buffer
         (car (server-visit-files `((,filename)) proc nil)))
      (find-file filename wild))))

(global-set-key "\C-x\C-f" 'y-find-file)

