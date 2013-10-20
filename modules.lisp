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

(defgeneric add-cookie (obj cookie)
  (:method ((obj authorizable-module) cookie)
    (push cookie (drakma:cookie-jar-cookies (slot-value obj 'cookie-jar)))))

(defgeneric clear-cookies (obj)
  (:method ((obj authorizable-module))
    (setf (drakma:cookie-jar-cookies (slot-value obj 'cookie-jar)) nil)))

(defgeneric authorize (obj &key)
  (:documentation "Authorize user for the given object"))

(defmacro with-cookie-jar (obj &body body)
  `(let ((*webgunk-cookie-jar* (get-cookie-jar ,obj)))
     ,@body))

(defgeneric make-request (obj uri &rest args)
  (:documentation "Make request using this object's cookie jar")
  (:method ((obj authorizable-module) uri &rest args)
    (with-cookie-jar obj
      (apply #'http-request uri args))))
  


  
