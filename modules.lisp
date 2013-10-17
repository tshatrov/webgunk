(in-package :webgunk/modules)

(defgeneric get-results (obj &optional reset)
  (:documentation "Get results from the current page"))

(defgeneric next-page (obj)
  (:documentation "Switch to the next page"))

(defgeneric prev-page (obj)
  (:documentation "Switch to the previous page"))

(defclass authorizable-module ()
  ((cookie-jar :initform (make-instance 'drakma:cookie-jar)
               :initarg :cookie-jar :reader get-cookie-jar)))

(defgeneric authorize (obj &key)
  (:documentation "Authorize user for the given object"))

(defmacro with-cookie-jar (obj &body body)
  `(let ((*webgunk-cookie-jar* (get-cookie-jar ,obj)))
     ,@body))


  
