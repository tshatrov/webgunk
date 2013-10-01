(in-package :webgunk/google)

;;; google search specific

(defun decode-google-url (href)
  (if (and (> (length href) 5)
           (equal (subseq href 0 5) "/url?"))
      (get-url-param href "q")
      href))

(defclass google-search ()
  ((term :initarg :term :accessor term)
   (start :initform 0 :reader start)
   (base-url :initform "http://www.google.com/search" :reader base-url)
   (link-css-selector :initform "h3.r a" :reader link-css-selector)))

(defmethod get-results ((obj google-search) &optional reset)
  (let ((parameters (list (cons "q" (term obj)))))
    (when reset
      (setf (slot-value obj 'start) 0))
    (when (> (start obj) 0)
      (push (cons "start" (write-to-string (start obj))) parameters))
    (let* ((document (parse-url (base-url obj) :parameters parameters))
           (links (css:query (link-css-selector obj) document)))
      (mapcar (lambda (link) 
                (cons (node-text link) 
                      (decode-google-url (dom:get-attribute link "href"))))
              links))))

(defmethod next-page ((obj google-search))
  (incf (slot-value obj 'start) 10)
  (get-results obj))
