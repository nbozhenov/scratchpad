;; -*- lexical-binding: t -*-

(require 'cl)
(require 'my-abstract-tree)

;;;;;;;;;;;;;;;;;;;
;; testcover-fix ;;
;;;;;;;;;;;;;;;;;;;

(defun testcover-after (idx val)
  "Internal function for coverage testing.  Returns VAL after installing it in
`testcover-vector' at offset IDX."
  (declare (gv-expander (lambda (do)
                          (gv-letplace (getter setter) val
                            (funcall do getter
                                     (lambda (store)
                                       `(progn (testcover-after ,idx ,getter)
                                               ,(funcall setter store))))))))
  (cond
   ((eq (aref testcover-vector idx) 'unknown)
    (aset testcover-vector idx val))
   ((not (condition-case nil
             (equal (aref testcover-vector idx) val)
           (error t)))
    (aset testcover-vector idx 'ok-coverage)))
  val)


;;;;;;;;;;;;;;;;;
;; tree-makers ;;
;;;;;;;;;;;;;;;;;

(defun att-empty-tree ()
  nil)

(defun att-small-tree ()
  (make-abstract-tree :content 42
                      :plist '(:marked t :color 'green)))

;; 0 -> 5 -> 8 -> 2
(defun att-chain-tree ()
  (let ((first (make-abstract-tree :content 0))
        (second (make-abstract-tree :content 5))
        (third (make-abstract-tree :content 8))
        (fourth (make-abstract-tree :content 2)))
    (abstract-tree-attach-in-place third fourth)
    (abstract-tree-attach-in-place second third)
    (abstract-tree-attach-in-place first second)))

;; 0 -> 5
(defun att-chain-tree-cut ()
  (let ((first (make-abstract-tree :content 0))
        (second (make-abstract-tree :content 5)))
    (abstract-tree-attach-in-place first second)))

;;     "0-0" --> "0-0-0"
;;    /
;; "0"       "0-1-0"
;;    \     /
;;     "0-1" --> "0-1-1" --> "0-1-1-0"
;;          \
;;           "0-1-2"
(defun att-branchy-tree ()
  (let ((node-0 (make-abstract-tree :content "0"))
        (node-0-0 (make-abstract-tree :content "0-0"))
        (node-0-0-0 (make-abstract-tree :content "0-0-0"))
        (node-0-1 (make-abstract-tree :content "0-1"))
        (node-0-1-0 (make-abstract-tree :content "0-1-0"))
        (node-0-1-1 (make-abstract-tree :content "0-1-1"))
        (node-0-1-1-0 (make-abstract-tree :content "0-1-1-0"))
        (node-0-1-2 (make-abstract-tree :content "0-1-2")))
    (abstract-tree-attach-in-place node-0-1-1 node-0-1-1-0)
    (abstract-tree-attach-in-place node-0-1 node-0-1-0)
    (abstract-tree-attach-in-place node-0-1 node-0-1-1)
    (abstract-tree-attach-in-place node-0-1 node-0-1-2)
    (abstract-tree-attach-in-place node-0-0 node-0-0-0)
    (abstract-tree-attach-in-place node-0 node-0-0)
    (abstract-tree-attach-in-place node-0 node-0-1)))

;;
;;
;; "0"
;;    \
;;     "0-1" --> "0-1-1" --> "0-1-1-0"
;;          \
;;           "0-1-2"
(defun att-branchy-tree-cut ()
  (let ((node-0 (make-abstract-tree :content "0"))
        (node-0-1 (make-abstract-tree :content "0-1"))
        (node-0-1-1 (make-abstract-tree :content "0-1-1"))
        (node-0-1-1-0 (make-abstract-tree :content "0-1-1-0"))
        (node-0-1-2 (make-abstract-tree :content "0-1-2")))
    (abstract-tree-attach-in-place node-0-1-1 node-0-1-1-0)
    (abstract-tree-attach-in-place node-0-1 node-0-1-1)
    (abstract-tree-attach-in-place node-0-1 node-0-1-2)
    (abstract-tree-attach-in-place node-0 node-0-1)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxillary functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun att-walk-many-node--children (func &rest children)
  (or (not (y-filter 'identity children))
      (progn
        (apply 'att-walk-many-node func (mapcar 'car children))
        (apply 'att-walk-many-node--children func (mapcar 'cdr children)))))

(defun att-walk-many-node (func &rest trees)
  "Мутная семантика, потому поместил функцию в тесты.
Изначально планировались функции abstract-tree-zip-node/content, но
1) не понятно, зачем они нужны за пределами тестов;
2) не понятно, как формировать plist для результирующего дерева.
Так что пусть пока поживут в тестах."
  (when (eval `(or ,@trees))
    (apply func trees))
  (apply 'att-walk-many-node--children func
         (mapcar #'(lambda (x) (and x (abstract-tree-children x)))
                 trees)))

(defun att-walk-many-content (func &rest trees)
  (apply 'att-walk-many-node
         (lambda (&rest x) (apply func (mapcar 'abstract-tree-content trees)))
         trees))

(defun att-first-child (tree)
  (car (abstract-tree-children tree)))

(defun att-content-equal--children (lhs rhs)
  (or (and (not lhs) (not rhs))
      (and (att-content-equal (car lhs) (car rhs))
           (att-content-equal--children (cdr lhs) (cdr rhs)))))

(defun att-content-equal (lhs rhs)
  "Compares for equality only contents of trees LHS and RHS.
Не понятно, как сравнивать родителей lhs и rhs и стоит ли сравнивать их plist-ы.
Поэтому пока пусть полежит в тестах."
  (or (and (not lhs) (not rhs))
      (and lhs rhs
           (equal (abstract-tree-content lhs)
                  (abstract-tree-content rhs))
           (att-content-equal--children (abstract-tree-children lhs)
                                        (abstract-tree-children rhs)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-size ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(assert (eq 0 (abstract-tree-size (att-empty-tree))))
(assert (eq 1 (abstract-tree-size (att-small-tree))))
(assert (eq 4 (abstract-tree-size (att-chain-tree))))
(assert (eq 2 (abstract-tree-size (att-chain-tree-cut))))
(assert (eq 8 (abstract-tree-size (att-branchy-tree))))
(assert (eq 5 (abstract-tree-size (att-branchy-tree-cut))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-copy ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let (make-list-func)
  (dolist (make-list-func (list 'att-empty-tree
                                'att-small-tree
                                'att-chain-tree
                                'att-chain-tree-cut
                                'att-branchy-tree
                                'att-branchy-tree-cut))
    (let* ((original (funcall make-list-func))
           (copy (abstract-tree-copy original)))
      (att-walk-many-node #'(lambda (lhs rhs)
                              (if (or lhs rhs)
                                  (assert (not (eq lhs rhs)))
                                (assert (eq lhs rhs)))
                              (assert (att-content-equal lhs rhs))
                              (assert (equal (abstract-tree-size lhs)
                                             (abstract-tree-size rhs))))
                          copy original))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-walk/map ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (tmp)
  (dolist (tmp '((att-empty-tree nil nil)
                 (att-small-tree (42) (84))
                 (att-chain-tree (0 5 8 2) (0 10 16 4))
                 (att-chain-tree-cut (0 5) (0 10))
                 (att-branchy-tree ("0" "0-0" "0-0-0" "0-1" "0-1-0" "0-1-1" "0-1-1-0" "0-1-2")
                                   ("00" "0-00-0" "0-0-00-0-0" "0-10-1" "0-1-00-1-0"
                                    "0-1-10-1-1" "0-1-1-00-1-1-0" "0-1-20-1-2"))
                 (att-branchy-tree-cut ("0" "0-1" "0-1-1" "0-1-1-0" "0-1-2")
                                       ("00" "0-10-1" "0-1-10-1-1" "0-1-1-00-1-1-0" "0-1-20-1-2"))))
    (let* ((tree (funcall (car tmp)))
           (etalon-map (caddr tmp))
           (etalon-walk (cadr tmp))
           (new-map (abstract-tree-map tree (lambda (x) (if (stringp x)
                                                            (concat x x)
                                                          (+ x x)))))
           result0 result1 result2)
      (when new-map
        (assert (not (abstract-tree-parent new-map))))
      (abstract-tree-walk new-map
                          #'(lambda (c x)
                              (assert (not (abstract-tree-plist x)))
                              (push (abstract-tree-content x) result0)))
      (abstract-tree-walk tree #'(lambda (c x) (push (abstract-tree-content x) result1)))
      (abstract-tree-walk tree #'(lambda (x) (push x result2)))
      (att-walk-many-node #'(lambda (x y)
                              (assert (and x y))
                              (let* ((old (abstract-tree-content x))
                                     (new (abstract-tree-content y))
                                     (etalon (if (stringp old) (concat old old) (+ old old))))
                                (assert (equal etalon new))))
                          tree new-map)
      (assert (equal etalon-map (nreverse result0)))
      (assert (equal etalon-walk (nreverse result1)))
      (assert (equal etalon-walk (nreverse result2))))))

(let (tmp)
  (dolist (tmp '((att-empty-tree (lambda (x y) (setf (abstract-tree-content y) t))
                                 (lambda (x) nil)
                                 nil)
                 (att-small-tree (lambda (x y) (setf (abstract-tree-content y) (+ x x)))
                                 (lambda (x) t)
                                 (84))
                 (att-chain-tree (lambda (x y) (setf (abstract-tree-content y) 0))
                                 (lambda (x) (member x (list 5 2)))
                                 (0 0 8 0))
                 (att-chain-tree-cut (lambda (x y) (abstract-tree-put y :color (quote green)))
                                     (lambda (x) t)
                                     (0 5))
                 (att-branchy-tree (lambda (x y) (setf (abstract-tree-content y) (concat x x)))
                                   (lambda (x y) (member (abstract-tree-content y)
                                                         (list "0" "0-0-0" "0-1-0" "0-1-2")))
                                   ("00" "0-0" "0-0-00-0-0" "0-1" "0-1-00-1-0" "0-1-1" "0-1-1-0" "0-1-20-1-2"))))
    (let* ((tree (funcall (car tmp)))
           (action (cadr tmp))
           (predicate (caddr tmp))
           (etalon (cadddr tmp))
           result)
      (abstract-tree-walk tree action predicate)
      (abstract-tree-walk tree (lambda (x) (push x result)))
      (setq result (nreverse result))
      (assert (equal etalon result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-filter ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (tmp)
  (dolist (tmp (list
                (list #'(lambda (x) t)
                      (att-empty-tree)
                      (att-empty-tree))
                (list #'(lambda (x) nil)
                      (att-empty-tree)
                      (att-empty-tree))
                (list #'(lambda (x) t)
                      (att-branchy-tree)
                      (att-branchy-tree))
                (list #'(lambda (x) nil)
                      (att-branchy-tree)
                      (att-empty-tree))
                (list #'(lambda (x) (member x '(0 5)))
                      (att-chain-tree)
                      (att-chain-tree-cut))
                (list #'(lambda (x) (not (member x '("0-0" "0-1-0"))))
                      (att-branchy-tree)
                      (att-branchy-tree-cut))))
    (let ((func (car tmp))
          (original (cadr tmp))
          (etalon (caddr tmp)))
      (assert (att-content-equal
               etalon
               (abstract-tree-filter original func))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-map-up ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (tmp)
  (dolist (tmp (list
                (list (att-empty-tree)
                      #'(lambda (x) 1)
                      nil)
                (list (att-small-tree)
                      #'(lambda (x) (+ x x))
                      '(84))
                (list (car (abstract-tree-children (att-chain-tree)))
                      #'(lambda (x) (+ x 5))
                      '(10 5))
                (list (att-first-child (att-first-child (att-first-child (att-branchy-tree-cut))))
                      #'(lambda (x) x)
                      '("0-1-1-0" "0-1-1" "0-1" "0"))))
    (let ((tree (car tmp))
          (func (cadr tmp))
          (etalon (caddr tmp)))
      (assert (equal etalon (abstract-tree-map-up tree func))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-attach ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((lhs-list (list (att-small-tree)
                      (att-chain-tree)
                      (att-branchy-tree)))
      (rhs-list (list (att-empty-tree)
                      (att-small-tree)
                      (att-chain-tree)
                      (att-branchy-tree)))
      lhs rhs)
  (dolist (lhs lhs-list)
    (dolist (rhs rhs-list)
      (let* ((lhs (abstract-tree-copy lhs))
             (rhs (abstract-tree-copy rhs))
             (attached (abstract-tree-attach lhs rhs)))
        (abstract-tree-attach-in-place lhs rhs)
        (assert (not (eq lhs attached)))
        (assert (att-content-equal lhs attached))))))
      

;;;;;;;;;;;;;;;;;;;;;;;
;; att-content-equal ;;
;;;;;;;;;;;;;;;;;;;;;;;
 
(let ((makers (list 'att-empty-tree
                    'att-small-tree
                    'att-chain-tree
                    'att-chain-tree-cut
                    'att-branchy-tree
                    'att-branchy-tree-cut)))
  (while makers
    (let* ((mkr (car makers))
           (sample (funcall mkr))
           (sample2 (funcall mkr))
           mkr2)
      (assert (att-content-equal sample sample))
      (assert (att-content-equal sample sample2))
      (assert (att-content-equal sample2 sample))
      (dolist (mkr2 (cdr makers))
        (let ((wrong (funcall mkr2)))
          (assert (not (att-content-equal wrong sample)))
          (assert (not (att-content-equal sample wrong))))))
    (setq makers (cdr makers))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-put/get ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((makers (list 'att-chain-tree
                    'att-chain-tree-cut
                    'att-branchy-tree
                    'att-branchy-tree-cut))
      tmp)
  (dolist (tmp makers)
    (let ((tree (att-first-child (funcall tmp))))
      (assert (eq nil (abstract-tree-get tree :value)))
      (assert (eq nil (abstract-tree-get tree :color)))
      (abstract-tree-put tree :value 42)
      (abstract-tree-put tree :color 'green)
      (assert (eq 42 (abstract-tree-get tree :value)))
      (assert (eq 'green (abstract-tree-get tree :color)))
      (abstract-tree-put tree :value nil)
      (abstract-tree-put tree :color nil)
      (assert (eq nil (abstract-tree-get tree :value)))
      (assert (eq nil (abstract-tree-get tree :color))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-find-children/first ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (tmp)
  (dolist (tmp (list
                (list (att-empty-tree)
                      (lambda (x) t)
                       nil)
                (list (att-small-tree)
                      (lambda (x) t)
                      (list 42))
                (list (att-chain-tree)
                      (lambda (x y) (member (abstract-tree-content y) (list 2 5 8)))
                      (list 5 8 2))
                (list (att-chain-tree-cut)
                      (lambda (x) nil)
                      nil)
                (list (att-branchy-tree)
                      (lambda (x) (> (length x) 3))
                      (list "0-0-0" "0-1-0" "0-1-1" "0-1-1-0" "0-1-2"))
                (list (att-branchy-tree-cut)
                      (lambda (x y) (equal "0-1" (and (abstract-tree-parent y)
                                                      (abstract-tree-content (abstract-tree-parent y)))))
                      (list "0-1-1" "0-1-2"))))
    (let* ((tree (car tmp))
           (predicate (cadr tmp))
           (etalon (caddr tmp))
           (result-first (abstract-tree-find-first tree predicate))
           (result-all (abstract-tree-find-children tree predicate)))
      (when result-first
        (setq result-first (abstract-tree-content result-first)))
      (when result-all
        (setq result-all (mapcar 'abstract-tree-content result-all)))
      (assert (equal etalon result-all))
      (assert (equal (car etalon) result-first)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract-tree-find-ancestor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((tree (att-empty-tree)))
  (assert (not (abstract-tree-find-ancestor tree (lambda (x) t)))))

(let ((ancestor (abstract-tree-find-first (att-branchy-tree) (lambda (x) (equal "0-1-1-0" x))))
      (etalon (list "0-1-1" "0-1" "0")))
  (dolist (tmp etalon)
    (setq ancestor (abstract-tree-find-ancestor ancestor (lambda (x) t)))
    (assert (equal (abstract-tree-content ancestor) (car etalon)))
    (setq etalon (cdr etalon)))
  (assert ancestor)
  (assert (not (abstract-tree-find-ancestor ancestor (lambda (x) t)))))

(let ((tree (abstract-tree-find-first (att-branchy-tree) (lambda (x) (equal "0-1-1-0" x)))))
  (assert (not (abstract-tree-find-ancestor tree (lambda (x y) (< 10 (length (abstract-tree-children y))))))))

(let ((ancestor (abstract-tree-find-first (att-branchy-tree) (lambda (x) (equal "0-1-1-0" x))))
      (etalon (list "0-1-1" "0")))
  (dolist (tmp etalon)
    (setq ancestor (abstract-tree-find-ancestor ancestor (lambda (x) (member (length x) (list 1 5)))))
    (assert (equal (abstract-tree-content ancestor) (car etalon)))
    (setq etalon (cdr etalon)))
  (assert (not (abstract-tree-find-ancestor ancestor (lambda (x) t)))))


