(in-package :webgunk/tumblr)

(defclass tumblr-post ()
  ((url :initarg :url :reader url)
   (post-id :reader post-id)
   (blog-url :reader blog-url)
   (notes-code :initform nil)
   (notes-next :initform nil)
   (notes :initform nil :accessor notes)
   ))


(defmethod initialize-instance :after ((obj tumblr-post) &key)
  (multiple-value-bind (base-url groups) (ppcre:scan-to-strings "(.+)/post/(\\d+)" (url obj))
    (declare (ignore base-url))
    (setf (slot-value obj 'blog-url) (elt groups 0)
          (slot-value obj 'post-id) (elt groups 1))))


(defclass tumblr-note ()
  ((text :initarg :text :reader note-text)
   (type :initarg :type :reader note-type) ; :like or :reblog
   (user :initarg :user :reader note-user)
   (from :initarg :from :initform nil :reader note-from)
   (link :initarg :link :initform nil :reader note-link)
   (comment :initarg :comment :initform nil :reader note-comment)))


(defmethod print-object ((obj tumblr-note) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a ~a~@[ :FROM ~a~]~:[~; :WITHCOMMENT~]"
            (note-type obj) (note-user obj) (note-from obj) (note-comment obj))))

(defun make-note-from-node (node)
  (loop with note-type and note-user and note-comment and note-link and note-from
     for class in (class-list node)
     do (cond 
          ((equal class "more_notes_link_container") (return))
          ((equal class "like")
           (setf note-type :like))
          ((equal class "original_post")
           (setf note-type :original))
          ((equal class "reblog")
           (setf note-type :reblog)
           (let ((source-node (car (css:query "a.source_tumblelog" node))))
             (when source-node
               (setf note-from (node-text source-node)))))
          ((equal class "with_commentary")
           (let ((comment-node (car (css:query "blockquote > a" node))))
             (when comment-node
               (setf note-comment (node-text comment-node))
               (setf note-link (dom:get-attribute comment-node "href")))))
          (t (let ((user (nth-value 1 (starts-with-subseq "tumblelog_" class :return-suffix t))))
               (if user (setf note-user user)))))
     finally (return (make-instance 'tumblr-note 
                                    :text (node-text node) :type note-type :user note-user
                                    :comment note-comment :link note-link :from note-from
                                    ))))
     

;; tumblrReq.open('GET','/notes/801#########/wwwwwwwww?from_c=##########',true);
(defmethod get-notes-code ((obj tumblr-post))
  (let* ((resp (http-request (url obj)))
         (regex (format nil "/notes/~a/(\\w+)" (post-id obj))))
    (multiple-value-bind (note-url groups) (ppcre:scan-to-strings regex resp)
      (declare (ignore note-url))
      (setf (slot-value obj 'notes-code) (elt groups 0)))))


(defmethod get-notes-once ((obj tumblr-post))
  (with-slots (notes-code notes-next) obj
    (when (not notes-code)
      (get-notes-code obj))
    (let* ((notes-url (format nil "~a/notes/~a/~a~@[?from_c=~a~]"
                              (blog-url obj) (post-id obj) notes-code notes-next))
           (document (parse-url notes-url))
           (notes-list (car (css:query "ol.notes" document)))
           (next-link (car (css:query "a.more_notes_link" document))))
      (loop for note-node across (dom:child-nodes notes-list)
           for note = (make-note-from-node note-node)
           when note do (push note (notes obj)))
      (when next-link
        (let* ((onclick (cdr (assoc "onclick" (get-attributes next-link) :test #'equalp)))
               (next (elt (nth-value 1 (ppcre:scan-to-strings "\\?from_c=(\\d+)" onclick)) 0)))
          (setf notes-next next))))))

        
(defmethod get-all-notes ((obj tumblr-post) &key (progress t))
  (loop while (get-notes-once obj) if progress do (princ ".") 
     finally (when progress (terpri))))
      

(defun get-post-notes (url &key (progress t))
  (let ((post (make-instance 'tumblr-post :url url)))
    (get-all-notes post :progress progress)
    post))

  
(defmethod print-comments ((obj tumblr-post))
  (loop for note in (notes obj)
       if (note-comment note) do
       (format t "~a reblogged from ~a and added:~%" (note-user note) (note-from note))
       (format t "~a~%~a~%~%" (note-comment note) (note-link note))))
