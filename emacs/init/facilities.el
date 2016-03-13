(defun toggle-tab-width ()
    "Toggle tab width between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 4 8))
    (redraw-display))

(defun load-emacs-tags ()
  "Load TAGS pertaining to emacs sources"
  (interactive)
  (setq tags-table-list 
	'("/home/yekka/.emacs.d/TAGS" 
	  "/media/XFiles/build/emacs-24.3/lisp/TAGS" 
	  "/media/XFiles/build/emacs-24.3/TAGS"))
  (setq tags-file-name nil))

(defun turn-debug-on ()
  (interactive)
  (set-debug t))

(defun turn-debug-off ()
  (interactive)
  (set-debug nil))

(defun set-debug (val)  
  (setq debug-on-error val
        debug-on-signal val
        debug-on-quit val))
  
