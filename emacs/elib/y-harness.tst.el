;; -*- lexical-binding: t -*-

(require 'cl)
(require 'y-harness)


;;;;;;;;;;;;;
;; y-arity ;;
;;;;;;;;;;;;;

;; Проверка валидных случаев

(defun t0 ())
(defun t1 (a))
(defun t2 (a b))
(defun t3 (&optional a))
(defun t4 (a &optional b))
(defun t5 (a b &optional c d e))
(defun t6 (&rest a))
(defun t7 (&optional a &rest b))
(defun t8 (&optional a b &rest c))
(defun t9 (a &optional b c d &rest d))

(dolist (test '((t0 . (0 . 0))
                (t1 . (1 . 1))
                (t2 . (2 . 2))
                (t3 . (0 . 1))
                (t4 . (1 . 2))
                (t5 . (2 . 5))
                (t6 . (0 . many))
                (t7 . (0 . many))
                (t8 . (0 . many))
                (t9 . (1 . many))))
  (assert (equal (y-arity (car test)) (cdr test))))

;;
;; Проверка облома на невалидных сигнатурах
;;
(defun e0 (&rest a &optional b))
(defun e1 (&optional &rest a))
(defun e2 (&optional a &rest))
(defun e3 (&optional a &rest b c))

(dolist (test '(e0 e1 e2 e3))
  (assert
   (condition-case nil
       (progn
         (y-arity test)
         nil)
     (error t))))


;;;;;;;;;;;;;
;; y-curry ;;
;;;;;;;;;;;;;

(let ((f (y-curry '+ 5)))
  (assert (equal '(0 . many) (y-arity f)))
  (assert (equal 47 (funcall f 42))))

(let ((abc 0))
  (let* ((f0 (lambda (x) (setq abc x)))
         (f1 (y-curry f0 42)))
    (assert (equal abc 0))
    (assert (equal '(0 . 0) (y-arity f1)))
    (funcall f1)
    (assert (equal abc 42))))

(let* ((res nil)
       (f (y-curry 'mapc (lambda (x) (push (+ x 1) res)))))
  (assert (equal '(1 . 1) (y-arity f)))
  (funcall f [0 4 13])
  (assert (equal '(14 5 1) res)))

(let* ((f (y-curry (indirect-function 'mapconcat)
                   (lambda (x) (format "%d" x))
                   [ 10 100 1000 ]
                   ""))
       (result (funcall f)))
  (assert (equal '(0 . 0) (y-arity f)))
  (assert (equal result "101001000")))


;;;;;;;;;;;;;;;;;;
;; ym-get-value ;;
;;;;;;;;;;;;;;;;;;

(let ((value (y-wrap-value 42)))
  (assert (equal 42 (ym-get-value value)))
  (assert (equal 42 (ym-get-value value))))

(let* ((abc 0)
       (proc (lambda (x) (setq abc 13) (+ x 10)))
       (value (y-lazy-value proc 32)))
  (assert (equal abc 0))
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 13))
  (setq abc 0)
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 0))
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 0)))

(progn
  (assert
   (condition-case nil
       (let ((proc (lambda (x) (+ x 10))))
         (y-lazy-value proc)
         nil)
     (error t))))

(progn
  (assert
   (condition-case nil
       (let ((proc (lambda (x y &rest args) (apply '+ x y args))))
         (y-lazy-value proc 10)
         nil)
     (error t))))

(let* ((abc 0)
       (proc (lambda (x) (setq abc 13) (+ x 10)))
       (value (y-greedy-value proc 32)))
  (assert (equal abc 0))
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 13))
  (setq abc 0)
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 13))
  (setq abc 0)
  (assert (equal 42 (ym-get-value value)))
  (assert (equal abc 13)))

(progn
  (assert
   (condition-case nil
       (let ((proc (lambda (x) (+ x 10))))
         (y-greedy-value proc)
         nil)
     (error t))))

(progn
  (assert
   (condition-case nil
       (let ((proc (lambda (x y &rest args) (apply '+ x y args))))
         (y-greedy-value proc 10)
         nil)
     (error t))))

