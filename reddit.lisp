(in-package :webgunk/reddit)

;;; reddit specific

(defun get-reddit-json-url (url)
  (multiple-value-bind (plist base-url params) (url-params url)
    (declare (ignore plist))
    (flet ((complete-url (base-url suffix)
             (let ((rest (when params (list "?" params))))
               (apply #'concatenate 'string base-url suffix rest))))
      (if (ends-with-subseq ".json" base-url)
          url
          (if (ends-with #\/ base-url)
              (complete-url base-url ".json")
              (complete-url base-url "/.json"))))))

(defun get-reddit-data (url &key (keys '("title" "url")) cookie-jar)
  (let ((json (jsown:parse (http-request (get-reddit-json-url url) :cookie-jar cookie-jar))))
    (values 
     (jsown-filter json "data" "children" map "data" keys)
     (jsown-filter json "data" "after")
     (jsown-filter json "data" "before")
     )))

(defclass reddit-browser (authorizable-module)
  ((base-url :initform "http://www.reddit.com" :initarg :url :reader base-url)
   (current-url :reader current-url)
   (keys :initform (list "title" "url") :initarg :keys :accessor keys)
   ;;(count :initform 0 :reader count)
   (after :initform :unknown)
   (before :initform :unknown)
   ))

(defmethod initialize-instance :after ((obj reddit-browser) &key)
  (setf (slot-value obj 'current-url) (base-url obj)))

(defmethod get-results ((obj reddit-browser) &optional reset)
  (with-slots (current-url base-url) obj
    (when reset
      (setf current-url base-url))
    (multiple-value-bind (results after before)
        (get-reddit-data current-url :keys (keys obj) :cookie-jar (get-cookie-jar obj))
      (setf (slot-value obj 'after) after
            (slot-value obj 'before) before)
      results)))

(defmethod next-page ((obj reddit-browser))
  (with-slots (after current-url base-url) obj
    (cond ((not after) nil)
          ((eql after :unknown)
           (get-results obj))
          (t (setf current-url
                   (append-param-str base-url (concatenate 'string "after=" after)))
             (get-results obj)))))
