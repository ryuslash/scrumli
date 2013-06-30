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

(defmethod datastore-init ((datastore pg-datastore))
  (with-connection (connection-spec datastore)
    (unless (table-exists-p 'story)
      (execute (dao-table-definition 'story)))))

(defmethod datastore-get-all-stories ((datastore pg-datastore))
  (with-connection (connection-spec datastore)
    (query (:order-by (:select :* :from 'story) 'priority) :alists)))

(defmethod datastore-get-story ((datastore pg-datastore) id)
  (with-connection (connection-spec datastore)
    (query (:select :* :from 'story :where (:= 'id id)) :alist)))

(defmethod datastore-post-story
    ((datastore pg-datastore) role necessity title content reporter)
  (format t "~s:~s:~s:~s:~s~%" role necessity title content reporter)
  (with-connection (connection-spec datastore)
    (let ((obj (make-instance
                'story :role role :necessity necessity :title title
                :priority (+ 1 (or (query (:select
                                           (:coalesce (:max 'priority) 0)
                                           :from 'story) :single)
                                   0))
                :content content :assignee "" :reporter reporter)))
      (save-dao obj))))

(defmethod datastore-story-get-state ((datastore pg-datastore) id)
  (with-connection (connection-spec datastore)
    (query (:select 'state :from 'story :where (:= 'id id)) :single)))

(defmethod datastore-story-set-state ((datastore pg-datastore) id state)
  (with-connection (connection-spec datastore)
    (execute (:update 'story :set 'state state :where (:= 'id id)))))

(defmethod datastore-story-change-priority
    ((datastore pg-datastore) id dir)
  (with-connection (connection-spec datastore)
    (let* ((current-priority (query (:select 'priority :from 'story
                                             :where (:= 'id id))
                                    :single))
           (next-priority (funcall (ecase dir (:up #'-) (:down #'+))
                                   current-priority 1)))
      (execute (:update 'story :set 'priority current-priority
                        :where (:= 'priority next-priority)))
      (execute (:update 'story :set 'priority next-priority
                        :where (:= 'id id))))))
