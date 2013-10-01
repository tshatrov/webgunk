(in-package :webgunk/modules)

(defgeneric get-results (obj &optional reset)
  (:documentation "Get results from the current page"))

(defgeneric next-page (obj)
  (:documentation "Switch to the next page"))

(defgeneric prev-page (obj)
  (:documentation "Switch to the previous page"))
