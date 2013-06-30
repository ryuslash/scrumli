(defpackage #:scrumli-config (:export #:*base-directory*))
(defparameter scrumli-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:scrumli
  :serial t
  :description "Scrum with Lisp"
  :author "Tom Willemse"
  :license "AGPLv3"
  :depends-on (:restas :sexml :postmodern :cl-json :drakma)
  :components ((:file "defmodule")
               (:file "pg-datastore")
               (:file "util")
               (:file "scrumli")))
