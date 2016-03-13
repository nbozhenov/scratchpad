;; у eieio проблемы с лексическим связыванием

;;
;; Кажется, здесь не осталось чего-нибудь стоящего
;; Вместо того, что находится здесь, нужно использовать интерфейс ym-get-value
;;
;; 07.03.2014
;;

(require 'eieio)


(defmacro y-defer (fun &rest args)
  `(y-deferred-call ,(format "%s" `(,fun ,@args)) :call (y-curry ,fun ,@args)))


(defclass y-deferred-call ()
  ((call :initarg :call
         :protection :protected
         :documentation "Call or its result")
   (done :initform nil
         :protection :protected
         :documentation "shows whether the calculation has been done already."))
  :documentation "Deferred calculation")


(defmethod ym-get ((def y-deferred-call))
  "Get the result of the calculations."
  (with-slots (call done) def
    (unless done
      (setq call (funcall call)
            done t))
    call))


(provide 'y-defer)


(defstruct defmemfun--internal-hash-data :value :ticks)
(defvar defmemfun--internal-hash (make-hash-table :test eql))

(defmacro defmemfun (func-name arg-list &rest body)
  `(progn
     (puthash ',func-name ,(make-hash-table :weakness key :test eql) ,defmemfun--internal-hash)
     (defun ,func-name ,arg-list-plain
       (let ((,memhash-entry (gethash (list ,@arg-list-plain))))))))


  
(defmemfun ast-for-buffer (buffer
                           &optional greater-element
                           &rest args)
  :stamp (buffer-modified-tick buffer)
  (declare abc)
  "Documentation"
  (some-calculations))



(defun ast-for-buffer (buffer &optional greater-element &rest args)
  "Documentation"
  (declare abc)
  (let* ((mem-key (apply 'list 'ast-for-buffer buffer greater-element args))
         (mem-value (gethash mem-key fun-hash))
         (tick (buffer-modified-tick buffer)))
    (if (and mem-value
             (equal (defmemfun--internal-hash-data-ticks mem-value)
                    (list tick)))
        (defmemfun--internal-hash-data-value mem-value)
      (let ((result (some-calculations)))
        (puthash mem-key
                 (make-defmemfun--internal-hash-data :value result
                                                     :ticks (list tick)))
        result))))
             


