
(in-package :webgunk)

;; jsown extension

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun val-map (values keys)
    "Recursively maps values across keys, both values and keys can be a list structure or an atom"
    (cond 
      ((null values) nil)
      ((eql (car values) :obj)
       (etypecase keys 
         (null nil)
         (atom (jsown:val values keys))
         (list (cons (val-map values (car keys)) (val-map values (cdr keys))))))
      (t (cons (val-map (car values) keys) (val-map (cdr values) keys)))))

  (defun make-jsown-filter (value first-spec &rest other-specs)
    (case first-spec
      (cl:map (let ((tmpvar (gensym "mapped-obj")))
                `(mapcar (lambda (,tmpvar) ,(apply #'make-jsown-filter tmpvar other-specs)) ,value)))
      (otherwise (let ((intermediate-computation `(val-map ,value ,first-spec)))
                   (if other-specs
                       (apply #'make-jsown-filter intermediate-computation other-specs)
                       intermediate-computation))))))

(defmacro jsown-filter (value &rest specs)
  "Fancy filtering for jsown-parsed objects.
spec can be one of the following:
[object] key to find.  will transform into (jsown:val value key)
[cl:map] use this modifier with an [object] modifier after it, to filter all elements in the list."
  (apply #'make-jsown-filter value specs))

;; DOM helpers

(defun get-attributes (element)
  "Get alist of element's attributes"
  (mapcar (lambda (attr) (cons (dom:name attr) (dom:value attr)))
       (dom:items (dom:attributes element))))

(defun strip-whitespace (str)
  ;;remove initial whitespace
  (setf str (cl-ppcre:regex-replace "^\\s+" str ""))
  ;;remove trailing whitespace
  (setf str (cl-ppcre:regex-replace "\\s+$" str ""))
  
  ;;remove initial/trailing whitespace in multiline mode
  (setf str (cl-ppcre:regex-replace-all "(?m)^[^\\S\\r\\n]+|[^\\S\\r\\n]+$" str ""))
  (setf str (cl-ppcre:regex-replace-all "(?m)[^\\S\\r\\n]+\\r$" str (make-string 1 :initial-element #\Return)))
  
  ;;replace more than two whitespaces with one
  (setf str (cl-ppcre:regex-replace-all "[^\\S\\r\\n]{2,}" str " "))
  
  ;;remove solitary linebreaks
  (setf str (cl-ppcre:regex-replace-all "([^\\r\\n])(\\r\\n|\\n)([^\\r\\n])" str '(0 " " 2)))
  
  ;;replace more than one linebreak with one
  (setf str (cl-ppcre:regex-replace-all "(\\r\\n|\\n){2,}" str '(0)))
  str)

(defun node-text (node &rest args &key test (strip-whitespace t))
  (let (values result)
    (when (or (not test) (funcall test node))
      (dom:do-node-list (node (dom:child-nodes node))
        (let ((val (case (dom:node-type node)
                     (:element (apply #'node-text node args))
                     (:text (dom:node-value node)))))
          (push val values))))
    (setf result (apply #'concatenate 'string (nreverse values)))
    (if strip-whitespace (strip-whitespace result) result)))

;; HTTP request / URL helpers

(defun http-request (&rest args)
  "A wrapper around DRAKMA:HTTP-REQUEST which converts octet array
which it sometimes returns to normal string"
  (let ((result (apply #'drakma:http-request args)))
    (if (and (arrayp result)
             (equal (array-element-type result) '(unsigned-byte 8)))
        (flexi-streams:octets-to-string result)
        result)))

(defun parse-url (url &rest args)
  "Parse HTTP request response to CXML-DOM"
  (let ((response (apply #'http-request url args)))
    (chtml:parse response (cxml-dom:make-dom-builder))))

(defun make-params (params)
  "Converts plist-style params to alist form"
  (if (and params (keywordp (car params)))
      (loop for (pname pvalue) on params by #'cddr
         collect (cons (string-downcase (symbol-name pname))
                       pvalue))
      params))

(defun parse-request (url params &rest args)
  "params can be in :key1 value1 :key2 value2 format"
  (let ((parameters (make-params params)))
    (apply #'parse-url url :parameters parameters args)))

(defun url-params (url &key only-params &aux (qpos (position #\? url)))
  "Returns alist of url get-parameters. 
Also returns base url and query as string as second and third return values respectively"
  (let* ((query (if qpos (subseq url (1+ qpos)) (if only-params url "")))
         (split-query (split-sequence #\& query :remove-empty-subseqs t)))
    (values
     (mapcar
      (lambda (part &aux (eqpos (position #\= part)))
        (if eqpos
            (let ((key (subseq part 0 eqpos))
                  (value (subseq part (1+ eqpos))))
              (cons key (do-urlencode:urldecode value :queryp t :lenientp t)))
            (cons part part)))
      split-query)
     (if qpos (subseq url 0 qpos) (if only-params nil url))
     query)))

(defun get-url-param (url param)
  "Get specific get-param from url"
  (cdr (assoc (princ-to-string param) (url-params url) :test 'equalp)))

(defun append-param-str (url param-str)
  "Adds extra param-str (\"key=value\") to url. Not very smart."
  (if (url-params url)
      (concatenate 'string url "&" param-str)
      (concatenate 'string url "?" param-str)))

(defun url-params-to-string (params &key (encode t))
  "Converts alist/plist of parameters to string"
  (setf params (make-params params))
  (format nil "~{~a~^&~}" 
          (mapcar
           (lambda (pair)
             (let ((key (string-downcase (princ-to-string (car pair))))
                   (value (cdr pair)))
               (format nil "~A~:[~;=~A~]" key value 
                       (if encode (do-urlencode:urlencode (princ-to-string value) :queryp t) value))))
           params)))

(defun make-url (url params &key (overwrite t) (encode t))
  "Makes url from existing url and parameters. 
Can either overwrite or append parameters already present in url"
  (setf params (make-params params))
  (multiple-value-bind (old-params base-url) (url-params url)
    (when overwrite
      (setf old-params
            (loop for pair in old-params
               unless (assoc (car pair) params :test #'equalp :key #'princ-to-string)
               collect pair)))
    (let ((paramstr (url-params-to-string (nconc old-params params) :encode encode)))
      (if (= (length paramstr) 0)
          base-url
          (concatenate 'string base-url "?" paramstr)))))

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

(defgeneric get-results (obj &optional reset)
  (:documentation "Get results from the current page"))

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
   

;;; reddit specific

"http://www.reddit.com/r/programming/.json"

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

(defclass reddit-browser ()
  ((base-url :initform "http://www.reddit.com" :initarg :url :reader base-url)
   (current-url :reader current-url)
   (cookie-jar :initform nil :initarg :cookie-jar :reader cookie-jar)
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
        (get-reddit-data current-url :keys (keys obj) :cookie-jar (cookie-jar obj))
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



    
    
  
   
