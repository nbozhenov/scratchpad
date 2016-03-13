;; -*- lexical-binding: t -*-

(require 'y-harness)

;;
;; Набор функций (макросов), аналогичных функциями, существующим в стандартной
;; библиотеке хаскеля.
;;
;; (расположены в алфавитном порядке)
;;
(defsubst y-filter (predicate sequence)
  (let (retval)
    (mapc #'(lambda (x)
              (when (funcall predicate x)
                (push x retval)))
          sequence)
    (nreverse retval)))


(defsubst y-foldl (binop init sequence)
  (mapc #'(lambda (x)
            (setq init (funcall binop init x)))
        sequence)
  init)


(defsubst y-foldl1 (binop sequence)
  (let ((accum (elt sequence 0)))
    (if (listp sequence)
        (progn
          (unless sequence
            (error "Empty list"))
          (y-foldl binop accum (cdr sequence)))
      (let ((index 1)
            (len (length sequence)))
        (while (< index len)
          (setq accum (funcall binop accum (aref sequence index))
                index (1+ index)))
        accum))))


(defalias 'y-map 'mapcar)


(provide 'y-hs)
