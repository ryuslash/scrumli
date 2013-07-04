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

  (define-method story-get-state (id)
    "Get the state of a story.")

  (define-method story-set-state (id state)
    "Set the state of a story.")

  (define-method story-change-priority (id dir)
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
