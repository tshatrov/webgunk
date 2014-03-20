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
               (:file "webgunk")))

(asdf:defsystem #:webgunk/test
  :serial t
  :description "Webgunk tests"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk)
  :components ((:file "tests")))

(asdf:defsystem #:webgunk/modules
  :serial t
  :description "Common functions for Webgunk modules"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk)
  :components ((:file "modules")))

(asdf:defsystem #:webgunk/google
  :serial t
  :description "Webgunk Google module"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk/modules)
  :components ((:file "google")))

(asdf:defsystem #:webgunk/reddit
  :serial t
  :description "Webgunk reddit module"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk/modules)
  :components ((:file "reddit")))

(asdf:defsystem #:webgunk/tumblr
  :serial t
  :description "Webgunk tumblr module"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:webgunk/modules)
  :components ((:file "tumblr")))
