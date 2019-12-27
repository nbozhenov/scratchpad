;; -*- lexical-binding: t -*-
;;
;; Релевантные ссылки:
;; [id:b307ce7a-3f8a-4892-b82d-35596ecfb85e]
;;
;; TODO:
;; + утилизация маркеров
;; + проверка работы кеша
;; + сейчас дерево, полученное из org-element модифицируется сначала в yorg-parse-current-buffer,
;;   затем в yorg-agenda--cache-push-buffer. надо бы совсем его не модифицировать, а перенести
;;   нужные мне вещи во вьюхи. возможно, следует сделать, чтобы ф-ция abstract-tree-get сначала
;;   искала во вьюхе, а если не нашла, то в оригинальном дереве. прозрачный механизм перезаписи свойств.
;;

(require 'my-abstract-tree)
(require 'org-element)



;;;;;;;;;;;;;;;;;;
;; Declarations ;;
;;;;;;;;;;;;;;;;;;

(defconst yorg-not-done-keywords '("TODO" "WAIT"))
(defconst yorg-done-keywords '("DONE" "CANCELED"))
(defconst yorg-agenda-initial-prefix "──")
(defconst yorg-replace-org-todo-list nil) ; should be nil in global context


;;;;;;;;;;;;;
;; Parsing ;;
;;;;;;;;;;;;;

(defun yorg-find-keywords (ast action)
  "Calls (ACTION kw-node AST) for every top-level kw-node found."
  (let (sect result
        (sections (y-filter #'(lambda (x) (equal (org-element-type x) 'section))
                            (org-element-contents ast))))
    (dolist (sect sections)
      (let ((kw nil)
            (keywords (y-filter #'(lambda (x) (equal (org-element-type x) 'keyword))
                                (org-element-contents sect))))
        (dolist (kw keywords)
          (funcall action kw))))))


(defun yorg-has-not-done-checkbox (org-node)
  "Returns t if there is at least one not-done checkbox under the ORG-NODE (without recursion)."
  (let (element result
                (contents (org-element-contents org-node)))
    (dolist (element contents result)
      (setq result
            (org-element-map element 'plain-list
                             #'(lambda (x)
                                 (let ((structure (org-element-property :structure x)))
                                   (y-foldl nil structure
                                            #'(lambda (y z)
                                                (or y (let ((checkbox (nth 4 z)))
                                                        (and checkbox (not (equal "[X]" checkbox)))))))))
                             nil t 'headline)))))


(defun yorg-inbox-p (ast)
  (or (member "inbox" (org-element-property :tags ast))
      (equal "inbox" (org-element-property :title ast))))


(defun yorg-note-p--internal (ast)
  (and ast
       (or (org-element-property :todo-type ast)
           (yorg-note-p--internal (org-element-property :parent ast)))))


(defun yorg-note-p (ast)
  (and (member "note" (org-element-property :tags ast))
       (equal 'todo (yorg-note-p--internal (org-element-property :parent ast)))))


(defun yorg-incoming-p--internal (ast)
  (and ast
       (or (yorg-inbox-p ast)
           (yorg-incoming-p--internal (org-element-property :parent ast)))))


(defun yorg-incoming-p (ast)
  (and ast
       (not (org-element-property :scheduled ast))
       (yorg-incoming-p--internal (org-element-property :parent ast))))


(defun yorg-find-parent-headline--internal (ast)
  (and ast
       (or (when (equal 'headline (org-element-type ast))
             ast)
           (yorg-find-parent-headline--internal (org-element-property :parent ast)))))


(defun yorg-find-parent-headline (ast)
  (when ast
    (yorg-find-parent-headline--internal (org-element-property :parent ast))))


(defun yorg-make-use-of-yblockers (ast)
  (org-element-map ast
                   'property-drawer
                   (lambda (x)
                     (let ((properties (org-element-property :properties x))
                           entry)
                       (dolist (prop properties)
                         (when (and (string= "YBLOCKER" (car prop))
                                    (eval (read (cdr prop))))
                           (setq entry (yorg-find-parent-headline x))
                           (when entry
                             (org-element-put-property entry :todo-keyword "WAIT")
                             (org-element-put-property entry :todo-type 'todo))))))))


(defun yorg-parse-current-buffer ()
  "Parse current buffer and handle top-level keywords after parsing."

  (let* ((ast (org-element-parse-buffer 'greater-element nil))
         (contents (org-element-contents ast))
         element)

    ;; transform org-data into headline
    (setq ast `(headline nil ,@contents))
    (dolist (element contents)
      (org-element-put-property element :parent ast))

    ;; set up default category for new headline
    (org-element-put-property ast
                              :category
                              (if buffer-file-name
                                  (file-name-sans-extension
                                   (file-name-nondirectory buffer-file-name))
                                "???"))
    (org-element-put-property ast
                              :title
                              (if buffer-file-name
                                  (concat "file:" buffer-file-name)
                                "???"))
    (org-element-put-property ast :begin 1)

    ;; handle top-level keywords
    (yorg-find-keywords
     ast
     #'(lambda (kw)
         (let ((key (org-element-property :key kw))
               (value (org-element-property :value kw)))
           (cond
            ((equal "TITLE" key)
             (org-element-put-property ast :title value)
             (org-element-put-property ast :begin (org-element-property :begin kw)))
            ((equal "FILETAGS" key)
             (org-element-put-property ast :tags (split-string value ":" t)))
            ((equal "CATEGORY" key)
             (org-element-put-property ast :category value))
            ((equal "FILETODO" key)
             (org-element-put-property ast :todo-keyword value)
             (cond
              ((member value yorg-not-done-keywords)
               (org-element-put-property ast :todo-type 'todo))
              ((member value yorg-done-keywords)
               (org-element-put-property ast :todo-type 'done))))))))

    ;; unchecked checkboxes force node to have not-done-status
    (org-element-map ast 'headline
                     (lambda (x) (when (and (not (equal 'todo (org-element-property :todo-type x)))
                                            (yorg-has-not-done-checkbox x))
                                   (org-element-put-property x :todo-keyword "TODO")
                                   (org-element-put-property x :todo-type 'todo))))
    ast))


;;;;;;;;;;;;;;;;;
;; ID-matchers ;;
;;;;;;;;;;;;;;;;;

(defun id-string (id-link)
  (let* ((symbol (aref (aref id-link 0) 0))
         (full-name (symbol-name symbol))
         (prefix (substring full-name 0 3))
         (id (substring full-name 3)))
    (unless (string= (downcase prefix) "id:")
      (error (format "Bad reference: '%s'" full-name)))
    id))


(defun id-find (id-link)
  (let* ((id-name (id-string id-link))
         (entry (org-id-find id-name))
         filename position)
    (when entry
      (yorg-view-for-position (car entry) (cdr entry)))))


(defun id-generic-predicate (id-link predicate)
  (let ((view (id-find id-link)))
    (when view
      (abstract-tree-apply predicate view))))


(defun id-wait-p (id-link)
  (id-generic-predicate id-link (lambda (x) (equal "WAIT" (org-element-property :todo-keyword x)))))


(defun id-not-done-p (id-link)
  (id-generic-predicate id-link (lambda (x) (equal 'todo (org-element-property :todo-type x)))))


;;;;;;;;;;;;;;
;; Requests ;;
;;;;;;;;;;;;;;

(defun yorg-todo-view (view)
  (setq view (abstract-tree-copy view))
  ;; отрезать все wait, inbox, scheduled, deadline
  (setq view (abstract-tree-filter view (lambda (x)
                                          (and (not (equal "WAIT" (org-element-property :todo-keyword x)))
                                               (not (yorg-inbox-p x))
                                               (not (org-element-property :scheduled x))
                                               (not (org-element-property :deadline x))))))
  (abstract-tree-walk
   view
   (lambda (x y) (abstract-tree-map-up y (lambda (x y) (abstract-tree-put y :needed t))))
   (lambda (x) (and (equal 'todo (org-element-property :todo-type x))
                    (not (yorg-note-p x)))))

  (abstract-tree-filter view (lambda (x y) (abstract-tree-get y :needed))))


(defun yorg-wait-view (view)
  (setq view (abstract-tree-copy view))
  (abstract-tree-walk
   view
   (lambda (x y) (abstract-tree-map-up y (lambda (x y) (abstract-tree-put y :needed t)))
                 (setf (abstract-tree-children y) nil))
   (lambda (x) (equal "WAIT" (org-element-property :todo-keyword x))))
  (abstract-tree-filter view (lambda (x y) (abstract-tree-get y :needed))))


(defun yorg-inbox-view (view)
  (setq view (abstract-tree-copy view))
  (abstract-tree-walk
   view
   (lambda (x y) (abstract-tree-map-up y (lambda (x y) (abstract-tree-put y :needed t))))
   (lambda (x) (yorg-incoming-p x)))
  (abstract-tree-filter view (lambda (x y) (abstract-tree-get y :needed))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; yorg-agenda--cache ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; one should not use these functions directly
; the only interface functions are `yorg-ast-for' and `yorg-view-for'

(defvar yorg-agenda--cache nil)
(defstruct yorg-agenda--cache-entry buffer tick data)

(defun yorg-agenda--cache-validate ()
  "Remove stale data from db."
  (setq yorg-agenda--cache
        (delete-if-not #'(lambda (x)
                           (let ((buffer (yorg-agenda--cache-entry-buffer x))
                                 (tick (yorg-agenda--cache-entry-tick x)))
                             (and (buffer-live-p buffer)
                                  (equal tick (buffer-modified-tick buffer)))))
                       yorg-agenda--cache)))


(defun yorg-agenda--cache-push-buffer (buffer)
  "Include BUFFER into db.
Return brand-new cache entry."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let* ((data (yorg-parse-current-buffer))
               (cache-entry
                (make-yorg-agenda--cache-entry :buffer buffer
                                               :tick (buffer-modified-tick buffer)
                                               :data data)))
          (push cache-entry yorg-agenda--cache)
          ; запускать эту ф-цию можно только после того, как дерево попало в кеш,
          ; иначе --- бесконечная рекурсия
          (yorg-make-use-of-yblockers data)
          cache-entry)))))


(defun yorg-agenda--find-buffer (buffer)
  (find-if (lambda (x) (eq buffer (yorg-agenda--cache-entry-buffer x)))
           yorg-agenda--cache))


(defun yorg-ast-for (buffer)
  "Т.к. в полученном ast нет привязки к буферу, то нет смысла использовать имя
файла в качестве аргумента. Все равно надо как-то уметь самостоятельно определять
буфер, соответствующий этому файлу."
  (yorg-agenda--cache-validate)
  (yorg-agenda--cache-entry-data
   (or (yorg-agenda--find-buffer buffer)
       (yorg-agenda--cache-push-buffer buffer))))


(defun yorg-view-for (buffer-or-filename)
  "Основной способ разбора org-файлов.
Для каждой view-ноды определено свойство :buffer."
  (let* ((buffer (if (bufferp buffer-or-filename)
                     buffer-or-filename
                   (find-file-noselect buffer-or-filename)))
         (view (yorg-convert-ast-to-view
                (yorg-ast-for buffer))))
    (abstract-tree-walk view (lambda (x y) (abstract-tree-put y :buffer buffer)))
    view))


(defun yorg-view-for-position (buffer-or-filename position)
  (abstract-tree-find-first
   (yorg-view-for buffer-or-filename)
   (lambda (x y)
     (setq retval y)
     (<= position (org-element-property :begin x)))))


(defun yorg-currently-clocked (&rest args)
  (unless (org-clocking-p)
    (error "Nothing is being clocked"))
  (let* ((marker org-clock-marker))
    (or (yorg-view-for-position (marker-buffer marker)
                                (marker-position marker))
        (error "Currently clocked entry not found"))))


;;;;;;;;;;;;;;;;
;; formatting ;;
;;;;;;;;;;;;;;;;

(defun yorg-propagate-prefix (view)
  (when view
    (abstract-tree-put view :prefix yorg-agenda-initial-prefix))
  (abstract-tree-walk
   view
   (lambda (x handle)
     (let* ((my-prefix (abstract-tree-get handle :prefix))
            (prefix (cond
                     ((equal yorg-agenda-initial-prefix my-prefix)
                      (make-string (length yorg-agenda-initial-prefix) ?\s))
                     ((equal "└──" (substring my-prefix -3))
                      (concat (substring my-prefix 0 -3) "   "))
                     (t
                      (concat (substring my-prefix 0 -3) "│  "))))
            (new-prefix (concat prefix "├──"))
            (end-prefix (concat prefix "└──"))
            (children (abstract-tree-children handle))
            child)
       (while children
         (setq child (car children)
               children (cdr children))
         (abstract-tree-put child :prefix new-prefix))
       (when child
         (abstract-tree-put child :prefix end-prefix))))))


(defsubst yorg-convert-ast-to-view (ast)
  (abstract-tree-build ast
                       (lambda (x) (y-filter (lambda (x) (equal 'headline (org-element-type x)))
                                             (org-element-contents x)))))


(defun yorg-agenda-adorned-headline-internal (prefix entry)
  "Returns string with added properties for insertion into the agenda buffer.
WARNING! The current buffer must be the buffer the entry comes from!
DO-NOT-FORMAT flag turns off call to `org-agenda-format-item'."
  (let* ((props (list 'face nil
                      'done-face 'org-agenda-done
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'mouse-face 'highlight))
         (marker (set-marker (make-marker) (org-element-property :begin entry)))
         (todo-kw (org-element-property :todo-keyword entry))
         (txt (org-element-property :title entry)))
    (setq txt (org-agenda-format-item "" (concat todo-kw (if todo-kw " " "") txt)))
    (org-agenda-highlight-todo
     (org-add-props (concat prefix txt) props
       'org-marker marker 'org-hd-marker marker 'org-todo-regexp org-todo-regexp))))


(defun yorg-agenda-adorned-headline (prefix entry buffer)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (yorg-agenda-adorned-headline-internal prefix entry)))))


(defun yorg-convert-view-to-list (view)
  (let ((view (abstract-tree-copy view))
        retval)
    (yorg-propagate-prefix view)
    (abstract-tree-walk
     view
     (lambda (entry handle)
       (push
        (yorg-agenda-adorned-headline (abstract-tree-get handle :prefix)
                                      entry
                                      (abstract-tree-get handle :buffer))
        retval)))
    (nreverse retval)))


(defun yorg-aggregate-data-general (func)
  (let (agenda-file retval)
    (dolist (agenda-file (org-agenda-files t))
      (push (funcall func (yorg-view-for agenda-file))
            retval))
    (y-filter 'identity (nreverse retval))))
      

(defun yorg-aggregate-data (kind)
  (cond
   ((eq kind :todo)
    (yorg-aggregate-data-general (quote yorg-todo-view)))
   ((eq kind :wait)
    (yorg-aggregate-data-general (quote yorg-wait-view)))
   ((eq kind :inbox)
    (yorg-aggregate-data-general (quote yorg-inbox-view)))
   ((eq kind :clock)
    (list (yorg-currently-clocked)))
   (t (error "Bad MATCH in org-agenda-custom-commands"))))
         

(defun yorg-todo-list (kind &rest args)
  (let ((data (yorg-aggregate-data (car kind))))
    (org-agenda-prepare "TODO")
    (setq org-prefix-format-compiled '(nil ""))
    (insert (org-add-props "Selected entries:" nil 'face 'org-agenda-structure) "\n")
    (org-agenda-mark-header-line (point-min))
    (let (view entry)
      (dolist (view data)
        (dolist (entry (yorg-convert-view-to-list view))
          (insert entry "\n"))
        (insert "\n")))
    (goto-char (point-min))
    (org-agenda-fit-window-to-buffer)
    (org-agenda-finalize)
    (setq buffer-read-only t)))


(defadvice org-todo-list (around yorg activate)
  (if yorg-replace-org-todo-list
      (funcall yorg-replace-org-todo-list (ad-get-args 0))
    ad-do-it))


;;;;;;;;;;;
;; debug ;;
;;;;;;;;;;;

;; (setq yorg-parse-current-buffer-output
;;       (with-current-buffer "test.org.tst"
;;         (save-excursion
;;           (yorg-parse-current-buffer))))

;; (setq abstract-tree-build-output
;;       (yorg-convert-ast-to-view yorg-parse-current-buffer-output))

;; (yorg-view-for "~/Dropbox/PIM/MCST/eh.org")
