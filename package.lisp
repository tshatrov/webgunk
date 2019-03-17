;;;; package.lisp

(defpackage :webgunk
  (:use :cl :alexandria)
  (:export :jsown-filter :get-attributes :class-list :strip-whitespace
           :http-request :parse-url :parse-request :parse :node-text
           :url-params :get-url-param :append-param-str
           :url-params-to-string :make-url :get-url-file-name
           :*webgunk-cookie-jar*)
  (:import-from :split-sequence :split-sequence))

(defpackage :webgunk/test
  (:use :cl :webgunk :lisp-unit))

(defpackage :webgunk/modules
  (:use :cl :webgunk)
  (:export :get-results :next-page :prev-page :authorize :authorizable-module
           :add-cookie :clear-cookies :get-cookie-jar :with-cookie-jar :make-request))

(defpackage :webgunk/google
  (:use :cl :webgunk :webgunk/modules))

(defpackage :webgunk/reddit
  (:use :cl :alexandria :webgunk :webgunk/modules))

(defpackage :webgunk/tumblr
  (:use :cl :alexandria :webgunk :webgunk/modules))
