;;;; frequently used middle-level functions
(defun filter-all-symbols (predicate)
  "Returns a list of all interned symbols which satisfy the given PREDICATE"
  (let (filter-predicate filter-mapper filter-result)
    (fset 'filter-predicate (indirect-function predicate))
    (fset 'filter-mapper 
          (lambda (symbol)
            (when (filter-predicate symbol)
                (push symbol filter-result))))
    (mapatoms 'filter-mapper)
    filter-result))

;;; applied functions
(defun find-symbol-by-value (value)
  "Given value VALUE returns list of symbols referring to this value.
Looks for VALUE in every slot of every symbol in the obarray."
  (filter-all-symbols 
   (lambda (symbol)
     (or (when (boundp symbol)
           (eq value (symbol-value symbol)))
         (when (fboundp symbol)
           (eq value (symbol-function symbol)))
         (eq value (symbol-plist symbol))))))
;; (assert (equal nil (find-symbol-by-value nil)))
;; (setq very-strange-and-rare-name-for-list '(very-strange-and-rare-name-for-symbol))
;; (assert (equal '(tstvar) (find-symbol-by-value '(very-strange-and-rare-name-for-symbol))))
;; (assert (let (tstvar)
;;           (fset 'tstvar '(very-strange-and-rare-name-for-symbol))
;;           (equal '(tstvar) (find-symbol-by-value '(very-strange-and-rare-name-for-symbol)))))
;; (assert (let (tstvar)
;;           (setplist 'tstvar '(very-strange-and-rare-name-for-symbol))
;;           (equal '(tstvar) (find-symbol-by-value '(very-strange-and-rare-name-for-symbol)))))
;; (assert (equal nil (find-symbol-by-value '(very-strange-and-rare-name-for-symbol))))
;; (assert (let (tstvar1 tstvar2 tstvar3 sym-list)
;;           (fset 'tstvar1 '(very-strange-and-rare-name-for-symbol))
;;           (setplist 'tstvar2 '(very-strange-and-rare-name-for-symbol))
;;           (set 'tstvar3 '(very-strange-and-rare-name-for-symbol))
;;           (setq sym-list (find-symbol-by-value '(very-strange-and-rare-name-for-symbol)))
;;           (and (equal 3 (length sym-list))
;;                (member 'tstvar1 sym-list)
;;                (member 'tstvar2 sym-list)
;;                (member 'tstvar3 sym-list))))

(defun keymap-for-binding (binding)
  "Look up active keymaps for the given BINDING."
  (let ((current-maps (current-active-maps t))
        (result nil))
    (while (and current-maps (not result))
      (progn
        (if (lookup-key (car current-maps) binding)
            (setq result (car current-maps)))
        (setq current-maps (cdr current-maps))))
    result))

(defun survey-of-keymaps ()
  "Returns list of currently active keymaps."
  (interactive)
  (let ((list-of-lists (mapcar 'find-symbol-by-value (current-active-maps)))
        folder)
    (fset 'folder (lambda (accum next) (concat accum " " next)))
    (setq list-of-lists (mapcar 'pp-to-string list-of-lists))
    (message (y-foldl "" list-of-lists 'folder))))


