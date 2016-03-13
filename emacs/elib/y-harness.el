;; -*- lexical-binding: t -*-

;;
;; Определение арности функции
;; Возвращает cons cell составом (min-arg-num . max-arg-num)
;; Вместо числа может быть символ many.
;;
(defun y-arity (function)
  (let ((arglist (help-function-arglist function))
        (index 0)
        optional rest min max)

    (dolist (arg arglist)
      (cond
       ((eq '&rest arg)
        (when rest
          (error "Internal error"))
        (setq rest index))
       ((eq '&optional arg)
        (when optional
          (error "Internal error"))
        (setq optional index))
       (t
        (incf index))))

    (when (or (and rest
                   (not (= rest (1- index))))
              (and optional rest
                   (>= optional rest)))
      (error "Internal error"))

    (cond
     ((and (not optional) (not rest))
      (setq min index
            max index))
     ((and (not optional) rest)
      (setq min rest
            max 'many))
     ((and optional (not rest))
      (setq min optional
            max index))
     ((and optional rest)
      (setq min optional
            max 'many)))

    (cons min max)))
    

;;
;; Построение каррированной функции.
;; Функция y-arity корректно работает с полученными каррированными функциями,
;; чего не скажешь о стандартной функции apply-partially.
;; В случае (setq fun (y-curry 'map '1+ [0 1 2])) реально вычисления случатся
;; только в момент вызова (funcall fun)
;; 
(defun y-curry (function &rest args)
  (let* ((function (indirect-function function))
         (arity (y-arity function))
         (min (car arity))
         (max (cdr arity))
         (nargs (length args))
         newmin newmax)

    (when (and (not (eq 'many max))
               (> nargs max))
      (error "Too many arguments"))

    (if (< min nargs)
        (setq newmin 0)
      (setq newmin (- min nargs)))

    (if (eq 'many max)
        (setq newmax 'many)
      (setq newmax (- max nargs)))

    (let (decl-list use-list tmp)
      (dotimes (i newmin)
        (setq tmp (make-symbol ""))
        (push tmp decl-list)
        (push tmp use-list))

      (cond
       ((eq newmin newmax)
        (push nil use-list))

       ((eq 'many newmax)
        (setq tmp (make-symbol ""))
        (push '&rest decl-list)
        (push tmp decl-list)
        (push tmp use-list))

       (t
        (dotimes (i (- newmax newmin))
          (setq tmp (make-symbol ""))
          (push '&optional decl-list)
          (push tmp decl-list)
          (push tmp use-list))
        (push nil use-list)))

      (setq decl-list (nreverse decl-list)
            use-list  (nreverse use-list))
            
      `(closure (t) ,decl-list (apply ',function ,@(mapcar (lambda (x) `',x) args) ,@use-list)))))


;;
;; Абстрактное значение
;; Приведенный интерфейс легко может накрывать обычные значения, ленивые значения
;; жадные значения и любую более сложную логику перевычисления значения
;;
(defsubst ym-get-value (handle)
  (funcall (car handle) handle))

(defsubst y-wrap-value (value)
  "Константное значение"
  (list (lambda (x) value)))

(defun y-lazy-value (func &rest args)
  "Вычисляется при первом обращении и запоминается"
  (let* ((curried (apply 'y-curry func args))
         (arity (car (y-arity curried)))
         (proc (lambda (handle)
                 (let ((value (funcall curried)))
                   (setcar handle (lambda (x) value))
                   value))))
    (unless (or (eq arity 'many) (eq arity 0))
      (error "Too few arguments"))
    (list proc)))

(defun y-greedy-value (func &rest args)
  "Пересчитывается при каждом обращении"
  (let* ((curried (apply 'y-curry func args))
         (arity (car (y-arity curried))))
    (unless (or (eq arity 'many) (eq arity 0))
      (error "Too few arguments"))
    (list (lambda (x) (funcall curried)))))


(provide 'y-harness)
