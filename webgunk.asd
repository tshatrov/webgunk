(asdf:defsystem #:webgunk
  :serial t
  :description "Tools and glue for web scraping"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:drakma
               #:cxml
               #:closure-html
               #:css-selectors
               #:do-urlencode
               #:split-sequence
               #:flexi-streams
               #:jsown
               #:alexandria
               #:lisp-unit
               )
  :components ((:file "package")
               (:file "webgunk")
               (:file "tests")))

