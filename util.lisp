(in-package #:scrumli)

(defun start-scrumli (&key
                        (port 8080)
                        (datastore 'scrumli.pg-datastore:pg-datastore)
                        datastore-init)
  (setf *datastore* (apply #'make-instance datastore datastore-init))
  (init)
  (start '#:scrumli :port port))
