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

(restas:define-policy datastore
  (:interface-package #:scrumli.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:scrumli.datastore)

  (define-method init ()
    "Initiate the datastore.")

  (define-method get-all-stories ()
    "Get all of the stories in the datastore.")

  (define-method get-story (id)
    "Get a story from the datastore.")

  (define-method get-tasks-for-story (id)
    "Get the tasks associated with a story.")

  (define-method post-story (role necessity title content reporter)
    "Post a new story.")

  (define-method post-task (story-id description reporter)
    "Post a new task for a story.")

  (define-method story-get-state (type id)
    "Get the state of a story.")

  (define-method story-set-state (type id state)
    "Set the state of a story.")

  (define-method story-change-priority (type id dir)
    "Change the priority of a story in direction DIR."))

(restas:define-module #:scrumli
  (:use #:cl #:restas #:json #:scrumli.datastore #:drakma)
  (:export #:start-scrumli))

(defpackage #:scrumli.pg-datastore
  (:use #:cl #:postmodern #:scrumli.policy.datastore)
  (:export #:pg-datastore))

(in-package #:scrumli)

(defparameter *static-directory*
  (merge-pathnames #P"static/" scrumli-config:*base-directory*))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (sexml:with-compiletime-active-layers
     (sexml:standard-sexml sexml:xml-doctype)
   (sexml:support-dtd
    (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
    :<)))
(<:augment-with-doctype "html" "" :auto-emit-p t)
