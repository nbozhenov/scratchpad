;; -*- lexical-binding: t -*-

(require 'cl)
(require 'y-hs)

;;;;;;;;;;;;;;
;; y-filter ;;
;;;;;;;;;;;;;;

(assert (equal nil (y-filter (lambda (x) t) [])))
(assert (equal '(a b c) (y-filter (lambda (x) t) [a b c])))
(assert (equal nil (y-filter 'not [t t t t])))
(assert (equal '(t t t t) (y-filter (lambda (x) x) [t t t t])))
(assert (equal '(5 4) (y-filter (lambda (x) (> x 3)) '(0 5 1 4 2))))


;;;;;;;;;;;;;
;; y-foldl ;;
;;;;;;;;;;;;;

(assert (equal 0 (y-foldl '+ 0 [])))
(assert (equal 3 (y-foldl (lambda (x y) (* x y)) 3 ())))
(assert (equal 21 (y-foldl '+ 0 [0 1 2 3 4 5 6])))
(assert (equal 256 (y-foldl (lambda (x y) (* x y)) 4 [1 2 4 8])))
(assert (equal 1 (y-foldl '- 64 (list 32 16 8 4 2 1))))
(assert (equal 1 (y-foldl '- 64 [32 16 8 4 2 1])))


;;;;;;;;;;;;;;
;; y-foldl1 ;;
;;;;;;;;;;;;;;

(dolist (test '((+ [0] 0)
                ((lambda (x y) (* x y)) (3) 3)
                (+ [0 0 1 2 3 4 5 6] 21)
                ((lambda (x y) (* x y)) [4 1 2 4 8] 256)
                (- (64 32 16 8 4 2 1) 1)
                (- [64 32 16 8 4 2 1] 1)))
  (let ((binop (car test))
        (seq (cadr test))
        (res (caddr test)))
    (assert (equal res (y-foldl1 binop seq)))))


(dolist (test '((+ [])
                (- ())))
  (let ((binop (car test))
        (seq (cadr test)))
    (assert
     (condition-case nil
         (progn
           (y-foldl1 binop seq)
           nil)
       (error t)))))


;;;;;;;;;;;
;; y-map ;;
;;;;;;;;;;;

(dolist (test '((1+ () ())
                (1+ [] ())
                (1+ (41) (42))
                (1+ [41] (42))
                ((lambda (x) (/ x 4)) (0 4 16 64 256) (0 1 4 16 64))
                ((lambda (x) (/ x 4)) [0 4 16 64 256] (0 1 4 16 64))))
  (let ((fun (car test))
        (seq (cadr test))
        (res (caddr test)))
    (assert (equal res (y-map fun seq)))))
        
