;; scrumli --- A simple scrum web application
;; Copyright (C) 2013  Tom Willemse

;; scrumli is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; scrumli is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with scrumli.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:scrumli-config (:export #:*base-directory*))
(defparameter scrumli-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:scrumli
  :serial t
  :description "Scrum with Lisp"
  :author "Tom Willemse"
  :license "AGPLv3"
  :depends-on (:restas :sexml :postmodern :cl-json :drakma
                       :closure-template :md5)
  :defsystem-depends-on (:closure-template)
  :components ((:closure-template "templates/scrumli")
               (:file "defmodule")
               (:file "pg-datastore")
               (:file "util")
               (:file "scrumli")))
