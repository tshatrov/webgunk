(in-package :webgunk/test)

(define-test strip-whitespace-test
  (assert-equal "aaa" (strip-whitespace "   aaa       "))
  (assert-equal "aa
a" (strip-whitespace "aa  
 
  a"))
  (assert-equal "a a a" (strip-whitespace "a  a  
a   "))
  )

(define-test http-request-test
  (let* ((url "http://duckduckgo.com/html/")
         (document (parse-request url '(:q "common lisp")))
         (links (css:query "div.links_main a.large" document))
         (attrs (get-attributes (car links))))
    (assert-true links)
    (assert-equal "large" (cdr (assoc "class" attrs :test #'equal)))
    (assert-equal "Common Lisp" (node-text (car (css:query "#zero_click_heading" document))))
    ))

(define-test url-params-test
  (assert-equal '(nil "http://www.google.com" "")
                (multiple-value-list (url-params "http://www.google.com")))

  (assert-equal '((("q" . "common lisp") ("hl" . "en"))
                  "/images"
                  "q=common+lisp&hl=en")
                (multiple-value-list (url-params "/images?q=common+lisp&hl=en")))
  (assert-equal '((("q" . "common lisp") ("hl" . "hl"))
                  nil
                  "q=common+lisp&hl")
                (multiple-value-list (url-params "q=common+lisp&hl" :only-params t)))
  )

