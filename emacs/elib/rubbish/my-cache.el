;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass my-cache ()
  "Lambdas as parameters:
   + (get-value key)
   + (get-stamp key)
   + (valid-p cache key stamp) ;; has sensible default value"
  ((storage :initarg :storage
            :initform (make-hash-table))
   (get-value :initarg :get-value)
   (get-stamp :initarg :get-stamp)
   (valid-p   :initarg :valid-p
              :initform (lambda (cache key stamp)
                          (eq (funcall (oref cache :get-stamp) key) stamp)))))

(defmethod m-drop ((cache my-cache) key)
  "Drop one element and return nil"
  (remhash key (oref cache :storage))
  nil)

(defmethod m-drop-all ((cache my-cache))
  "Drop all entries and return nil"
  (clrhash (oref cache :storage))
  nil)

(defmethod m-insert ((cache my-cache) key value)
  "Returns VALUE."
  (m-drop cache key)
  (puthash key (cons (funcall (oref cache :get-stamp) key)
                     value)
           (oref cache :storage))
  value)

(defmethod m-may-be-get ((cache my-cache) key)
  "Returns VALUE if KEY is within CACHE and is valid; nil otherwise.
When invalid VALUE found in cache, it is dropped."
  (let ((found (gethash key (oref cache :storage)))
        retval)
    (when found
      (if (funcall (oref cache :valid-p) cache key (car found))
          (cdr found)
        (m-drop cache key)))))

(defmethod m-get ((cache my-cache) key)
  (or (m-may-be-get cache key)
      (m-insert cache key (funcall (oref cache :get-value) key))))

(defmethod m-validate ((cache my-cache))
  (maphash (lambda (key value) (m-may-be-get cache key))))
                                 

(provide 'my-cache)
