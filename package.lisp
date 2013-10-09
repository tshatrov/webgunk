;;;; package.lisp

(defpackage :webgunk
  (:use :cl :alexandria)
  (:export :jsown-filter :get-attributes :strip-whitespace :node-text
           :http-request :parse-url :parse-request
           :url-params :get-url-param :append-param-str 
           :url-params-to-string :make-url)
  (:import-from :split-sequence :split-sequence))

(defpackage :webgunk/test
  (:use :cl :webgunk :lisp-unit))

(defpackage :webgunk/modules
  (:use :cl :webgunk)
  (:export :get-results :next-page :prev-page))

(defpackage :webgunk/google
  (:use :cl :webgunk :webgunk/modules))

(defpackage :webgunk/reddit
  (:use :cl :alexandria :webgunk :webgunk/modules))
