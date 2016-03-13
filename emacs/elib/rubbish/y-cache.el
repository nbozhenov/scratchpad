;; у eieio проблемы с лексическим связыванием

(require 'eieio)

(defmacro y-dbg-do (&rest body)
  nil)

(defmacro y-dbg-do (&rest body)
  `,@body)


(defclass yc-hash ()
  ((storage :initform (make-hash-table :test equal))))

(defmethod ym-get ((hash yc-hash) key)
  (with-slots (storage) hash
    (gethash key storage)))

(defmethod ym-put ((hash yc-hash) key value props)
  (with-slots (storage) hash
    (puthash key (cons value props) hash)))

(defmethod ym-size ((hash yc-hash))
  (with-slots (storage) hash
    (hash-table-count storage)))


(defclass yc-value-wrapper ()
  ((value :initarg :value)))

(defmethod ym-get ((wrapper yc-value-wrapper))
  (oref wrapper :value))


(defclass yc-stale-checker ()
  ((stamp)
   (check :initarg :check)))

(defmethod ym-stale-p ((checker yc-stale-checker))
  (with-slots (stamp check) checker
    (if (boundp stamp)
        (funcall check stamp

(defclass yc-lazy-worker ()
  ((do-work :initarg :do-work
   (last-result)
   (stale-checker :initarg :stale-checker
                  :initform yo-never-stale))))

;;
;; Построение объекта, реализующего концепцию ленивых вычислений.
;;
;; Общий вид вызова:
;; (y-make-lazy-value calc-procedure
;;          &optional :reuse reuse-policy
;;              &rest param-procedures
;;
;; Параметры:
;;   calc-procedure   :: Процедура (без параметров), чей результат должен быть
;;                       возвращен при запросе.
;;   reuse-policy     :: Политика переиспользования результата. Возможные значения:
;;                       :always -- вычисление будет выполнено не более 1 раза (DEFAULT)
;;                       :never -- вычисления повторяются при каждом обращении
;;                       procedure -- некоторая процедура, которой на вход будут
;;                         переданы предыдущее вычисленное значение и результаты
;;                         вызова остальных параметров-процедур. Вычисления будут
;;                         повторены, если процедура вернет nil.
;;   param-procedures :: Процедуры, чей результат будет использоваться как доп.
;;                       параметр для процедуры reuse-policy.
;;
;; Возвращаемое значение:
;;   Некоторый opaque объект, позволяющий получить значение с помощью вызова
;;   (y-extract-value obj)
;;
;; Примечания:
;; + reuse-policy и param-procedures вызываются при каждом обращении (даже первом)
;; + При первом обращении к объекту осуществляется вызов calc-procedure, но не осущ-ся
;;   вызовы reuse-policy и param-procedures, но не всегда -- вызов calc-procedure.
;;   вызовы reuse-policy и param-procedures. При последующих вызовах всегда осущ-ся
;;
;;
;; INTERNALS
;;
;; value -- массив
;; первым параметром: y-calculate-once (change to next)
;;                    y-get-stored-value
;;                    value :: [ proc, last-value ]
;;
;;                    y-calculate-always (do not modify function)
;;                    value :: [ proc ]
;;
;;                    y-calculate-prepare (change to next)
;;                    y-maybe-recalculate (call policy with args and then maybe recalculate value)
;;                    value :: [ proc, last-value, last-stamp, policy-proc, list-of-arg-funs ]
;;
(defmacro y-make-lazy-value (calc &optional reuse-kw reuse-proc &rest params)
  (cond
   ((and (equal nil params)
         (or (and (equal nil reuse-kw)
                  (equal nil reuse-proc))
             (and (equal :reuse reuse-kw)
                  (equal :always reuse-proc))))
    [ #'(lambda (arr)
          (aset arr 0 #'(lambda (x) (aref x 1)))
          (aset arr 1 (funcall calc)))
      nil ])
   ((and (equal nil params)
         (equal :reuse reuse-kw)
         (equal :never reuse-proc))
    [ #'(lambda (x) (funcall calc)) ])
   ((and (equal :reuse reuse-kw)
         reuse-proc)
    [ #'(lambda (arr)
          (aset arr 2 (apply (aref arr 3) (aref arr 4)))
          (aset arr 0 #'(lambda (x)
                          ;; Проблема: для парсинга буфера нужно get-tick-tratata
                          ;; а для построения дерева нужно сравнить пред. дерево с новым
                          ;; > все равно нет нужды подавать старый результат вызова в policy
                          ;; > в любом случае не на нем будет основываться результат ее работы.
                          ;; Проблема: дважды будет запрашиваться ast (ast конечно ленивое, но все же дважды)
    
  

(defsubst y-extract-value (value-arr)
  (funcall (aref value_arr 0) value-arr))





(y-make-lazy-worker (lambda () 42)
                    :recalc :never)

(y-make-lazy-worker (lambda () (ast-parse-buffer "agenda.org"))
                    :recalc :always)

(y-make-lazy-worker (lambda () (yorg-build-wait-view buf-parser))
                    :recalc (lambda (x y) (eql x y))
                    :param  (lambda () (ym-get buf-parser)))

(defmethod ym-get ((worker yc-lazy-worker))
  (with-slots (do-work last-result stale-checker) worker
    (if (ym-stale-p stale-checker)
        (prog1
            (setq last-result (funcall do-work))
          (ym-new-result stale-checker last-result))
      last-result)))


(defclass yc-lazy-worker ()
  ((storage :initform (yc-hash))
   (do-work :initarg :do-work)
   (get-key :initarg :get-key
            :initform 'list)
   (get-stamp :initarg :get-stamp
              :initform 'or)))

(defmethod ym-get ((worker yc-lazy-worker) &rest args)

  (with-slots (storage do-work get-key get-stamp) worker
    (let* ((key (apply get-key args))
           (stamp (apply get-stamp args))
           (found (ym-get storage key))
           (value (car found))
           (props (cdr found)))

      (y-dbg-do 
       (y-dbg-msg (format "%s\n" `(ym-get ,worker ,@args)))
       (y-dbg-msg (format "key = %s\n" key))
       (y-dbg-msg (format "stamp = %s\n" stamp))
       (y-dbg-msg (format "hash-size = %s\n" (ym-size storage)))
       (y-dbg-msg (format "found = %s\n" found)))
      
      (if (and found
               (equal stamp (assoc :stamp props)))

          (progn
            (y-dbg-do
             (y-dbg-msg "Found value is good enough.\n\n"))
            value)

        (prog1
            (setq value (apply do-work args))
          (ym-put storage key value (list :stamp stamp))
          (y-dbg-do
           (y-dbg-msg (format "Insert new value = %s\n" value))
           (y-dbg-msg (format "hash-size = %s\n\n" (ym-size storage)))))))))


(yc-lazy-worker "buffer parser"
                :do-work (lambda (x) (org-element-parse-buffer x 'greater-element))
                :get-key (lambda (x) (if (buffer-p x)
                                         x
                                       (get-buffer x)))
                :get-stamp (lambda (x) (buffer-modified-tick x)))

(yc-lazy-worker "view builder"
                
