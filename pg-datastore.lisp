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

(in-package #:scrumli.pg-datastore)

(defclass pg-datastore ()
  ((connection-spec :initarg :connection-spec
                    :accessor connection-spec)))

(defclass story ()
  ((id :col-type serial :reader story-id)
   (state :col-type string :reader state :initform "TODO")
   (role :col-type string :reader role :initarg :role)
   (necessity :col-type string :reader necessity :initarg :necessity)
   (title :col-type string :reader title :initarg :title)
   (priority :col-type integer :reader priority :initarg :priority)
   (content :col-type string :reader content :initarg :content)
   (reporter :col-type string :reader reporter :initarg :reporter)
   (assignee :col-type string :reader assignee :initarg :assignee))
  (:metaclass dao-class)
  (:keys id))

(defclass task ()
  ((id :col-type serial :reader story-id)
   (state :col-type string :reader state :initform "TODO")
   (description :col-type string :reader description :initarg :description)
   (priority :col-type integer :reader priority :initarg :priority)
   (reporter :col-type string :reader reporter :initarg :reporter)
   (assignee :col-type string :reader assignee :initarg :assignee)
   (story-id :col-type integer :reader story-id :initarg :story-id))
  (:metaclass dao-class)
  (:keys id))

(deftable task
  (!dao-def)
  (!foreign 'story 'story-id 'id))

(defmethod datastore-init ((datastore pg-datastore))
  (with-connection (connection-spec datastore)
    (unless (table-exists-p 'story)
      (execute (dao-table-definition 'story)))
    (unless (table-exists-p 'task)
      (execute (dao-table-definition 'task)))))

(defmethod datastore-get-all-stories ((datastore pg-datastore))
  (with-connection (connection-spec datastore)
    (query (:order-by (:select :* :from 'story) 'priority) :alists)))

(defmethod datastore-get-story ((datastore pg-datastore) id)
  (with-connection (connection-spec datastore)
    (append (query (:select :* :from 'story :where (:= 'id id)) :alist)
            `((tasks . ,(datastore-get-tasks-for-story datastore id))))))

(defmethod datastore-get-tasks-for-story ((datastore pg-datastore) id)
  (with-connection (connection-spec datastore)
    (query (:order-by (:select :* :from 'task :where (:= 'story-id id))
                      'priority)
           :alists)))

(defmethod datastore-post-story
    ((datastore pg-datastore) role necessity title content reporter)
  (with-connection (connection-spec datastore)
    (let ((obj (make-instance
                'story :role role :necessity necessity :title title
                :priority (+ 1 (or (query (:select
                                           (:coalesce (:max 'priority) 0)
                                           :from 'story) :single)
                                   0))
                :content content :assignee "" :reporter reporter)))
      (save-dao obj))))

(defmethod datastore-post-task
    ((datastore pg-datastore) story-id description reporter)
  (with-connection (connection-spec datastore)
    (let ((obj (make-instance
                'task :description description
                :priority (+ 1 (query (:select
                                       (:coalesce (:max 'priority) 0)
                                       :from 'task) :single))
                :reporter reporter :story-id (parse-integer story-id)
                :assignee "")))
      (save-dao obj))))

(defmethod datastore-story-get-state ((datastore pg-datastore) type id)
  (with-connection (connection-spec datastore)
    (query (:select 'state :from type :where (:= 'id id)) :single)))

(defmethod datastore-story-set-state
    ((datastore pg-datastore) type id state)
  (with-connection (connection-spec datastore)
    (execute (:update type :set 'state state :where (:= 'id id)))))

(defmethod datastore-story-change-priority
    ((datastore pg-datastore) type id dir)
  (with-connection (connection-spec datastore)
    (let* ((current-priority (query (:select 'priority :from type
                                             :where (:= 'id id))
                                    :single))
           (next-priority (funcall (ecase dir (:up #'-) (:down #'+))
                                   current-priority 1))
           (max-priority (query (:select (:max 'priority) :from type)
                                :single)))
      (execute (:update type :set 'priority current-priority
                        :where (:= 'priority next-priority)))
      (execute (:update type :set
                        'priority (max 1 (min next-priority max-priority))
                        :where (:= 'id id))))))
