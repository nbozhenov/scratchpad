;; -*- lexical-binding: t -*-
;; 
(require 'y-defer)


(let* ((abc 0)
       (fun (lambda (x) (setq abc x) 42))
       call)
  (setq call (y-defer fun 10))
  (assert (eq 0 abc))
  (assert (eq 42 (ym-get call)))
  (assert (eq 10 abc))
  (setq abc 0)
  (assert (eq 42 (ym-get call)))
  (assert (eq 0 abc)))


(let ((defcall (y-defer '+ 21 21)))
  (assert (eq 42 (ym-get defcall))))
