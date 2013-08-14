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

(in-package #:scrumli)

(defvar *scrumli-host* "http://localhost:5000"
  "The host currently running Scrumli. Used by Mozilla Persona.")

(defvar *scrumli-bootstrap-css-location*
  "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css"
  "The location of the twitter bootstrap CSS file.")

(defvar *scrumli-bootstrap-js-location*
  "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
  "The location of the twitter bootstrap JS file.")

(defvar *scrumli-font-awesome-css-location*
  "//netdna.bootstrapcdn.com/font-awesome/3.1.1/css/font-awesome.min.css"
  "The location of the font awesome CSS file.")

(defvar *scrumli-jquery-js-location*
  "//code.jquery.com/jquery-2.0.0.min.js"
  "The location of the jQuery JS file.")

(defvar *scrumli-react-js-location*
  "//cdnjs.cloudflare.com/ajax/libs/react/0.3.2/react.min.js"
  "The location of the React JS file.")

(defvar *scrumli-jsxtransformer-js-location*
  "//cdnjs.cloudflare.com/ajax/libs/react/0.3.2/JSXTransformer.js"
  "The location of the JSX Transformer JS file.")

(defun logged-in-p ()
  (gethash :username (getf (env *request*) :clack.session)))

(defun page-title (title)
  (concatenate 'string title " | scrumli"))

(defun md5-hash (str)
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-string str) 'list))))

(defun verify-credentials (audience assertion)
  (let ((response
         (http-request "https://verifier.login.persona.org/verify"
                       :method :post :content-type "application/json"
                       :content (encode-json-to-string
                                 `(("assertion" . ,assertion)
                                   ("audience" . ,audience)))
                       :want-stream t)))
    (decode-json response)))

(defclass scrumli-app (<app>) ())

(defvar *app* (make-instance 'scrumli-app))

(defun make-tpl-parameters (&rest args)
  (append (list :prefix (script-name *request*)) args))

(setf (route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (if (logged-in-p)
            (scrumli-templates:main
             (make-tpl-parameters
              :title (page-title "Backlog")
              :csss (list *scrumli-bootstrap-css-location*
                          *scrumli-font-awesome-css-location*
                          (concatenate 'string (script-name *request*) "static/css/scrumli.css"))
              :jss (list *scrumli-jquery-js-location*
                         *scrumli-bootstrap-js-location*
                         *scrumli-react-js-location*
                         *scrumli-jsxtransformer-js-location*
                         (concatenate 'string (script-name *request*) "js/bridge.js"))
              :username (gethash :username (getf (env *request*) :clack.session))
              :usermd5 (md5-hash (gethash :username (getf (env *request*) :clack.session)))
              :ulogout (concatenate 'string (script-name *request*) "logout")
              :umainjs (concatenate 'string (script-name *request*) "static/js/main.js")))
            (redirect *response*
                      (concatenate 'string (script-name *request*)
                                   "login")))))

(setf (route *app* "/login")
      (lambda (params)
        (declare (ignore params))
        (if (not (logged-in-p))
            (scrumli-templates:login
             (make-tpl-parameters
              :title (page-title "Login")
              :csss (list *scrumli-bootstrap-css-location*
                          *scrumli-font-awesome-css-location*)
              :jss (list *scrumli-bootstrap-js-location*
                         "https://login.persona.org/include.js"
                         (concatenate 'string (script-name *request*)
                                      "js/bridge.js")
                         (concatenate 'string (script-name *request*)
                                      "static/js/login.js"))))
            (redirect *response* (if (equal (script-name *request*) "")
                                     "/" (script-name *request*))))))

(setf (route *app* "/login" :method :post)
      (lambda (params)
        (let ((result (verify-credentials
                       *scrumli-host* (getf params :|assertion|))))
          (if (equal (cdr (assoc :status result)) "okay")
              (progn
                (setf (gethash :username
                               (getf (env *request*) :clack.session))
                      (cdr (assoc :email result)))
                (redirect *response*
                          (if (equal (script-name *request*) "")
                              "/" (script-name *request*))))
              '(403)))))

(setf (route *app* "/logout")
      (lambda (params)
        (declare (ignore params))
        (if (logged-in-p)
            (setf (gethash :username
                           (getf (env *request*) :clack.session)) nil))
        (redirect *response* (concatenate 'string (script-name *request*)
                                          "login"))))

(setf (route *app* "/stories")
      (lambda (params)
        (declare (ignore params))
        (if (logged-in-p)
            (list 200 '(:content-type "text/json")
                  (encode-json-to-string (get-all-stories)))
            '(403))))

(setf (route *app* "/stories/mine")
      (lambda (params)
        (declare (ignore params))
        (if (logged-in-p)
            (list 200 '(:content-type "text/json")
                  (encode-json-to-string
                   (get-stories-for
                    (gethash :username (getf (env *request*) :clack.session)))))
            '(403))))

(setf (route *app* "/stories/new" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let ((role (getf params :|role|))
                  (necessity (getf params :|necessity|))
                  (headline (getf params :|headline|))
                  (content (getf params :|content|)))
              (post-story role necessity headline content
                          (gethash :username (getf (env *request*) :clack.session)))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/stories/tasks/new" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let ((story-id (getf params :|storyId|))
                  (description (getf params :|description|)))
              (post-task story-id description
                         (gethash :username (getf (env *request*) :clack.session)))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/stories/state" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let* ((id (getf params :|id|))
                   (current-state (story-get-state 'story id))
                   (next (ecase (intern current-state :scrumli)
                           (todo "DOING")
                           (doing "DONE")
                           (done "TODO"))))
              (story-set-state 'story id next)
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string `((status . "ok")
                                             (state . ,next)))))
            '(403))))

(setf (route *app* "/tasks/state" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let* ((id (getf params :|id|))
                   (current-state (story-get-state 'task id))
                   (next (ecase (intern current-state :scrumli)
                           (todo "DOING")
                           (doing "DONE")
                           (done "TODO"))))
              (story-set-state 'task id next)
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string `((status . "ok")
                                             (state . ,next)))))
            '(403))))

(setf (route *app* "/stories/:dir" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let ((id (getf params :|id|))
                  (dir (getf params :dir)))
              (story-change-priority
               'story id (intern (string-upcase dir) :keyword))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/tasks/:dir" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (let ((id (getf params :|id|)))
              (story-change-priority
               'task id (intern (string-upcase (getf params :dir)) :keyword))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/stories/:id")
      (lambda (params)
        (if (logged-in-p)
            (list 200 '(:content-type "text/json")
                  (encode-json-to-string (get-story (getf params :id))))
            '(403))))

(setf (route *app* "/story/assignee" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (progn
              (set-assignee 'story (getf params :|id|) (getf params :|assignee|))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/task/assignee" :method :post)
      (lambda (params)
        (if (logged-in-p)
            (progn
              (set-assignee 'task (getf params :|id|) (getf params :|assignee|))
              (list 200 '(:content-type "text/json")
                    (encode-json-to-string '((status . "ok")))))
            '(403))))

(setf (route *app* "/js/bridge.js")
      (lambda (params)
        (declare (ignore params))
        (list 200 '(:content-type "text/javascript")
              (ps (var base-url (lisp (if (equal (script-name *request*) "")
                                          "/" (script-name *request*))))))))

(defun get-app ()
  (builder
   (<clack-middleware-static> :path "/static/" :root "static/")
   (<clack-middleware-session>
    :state (make-instance 'clack.session.state.cookie:<clack-session-state-cookie>))
   (<clack-middleware-postmodern>
    :database "scrumli" :user "slash" :password nil :host "localhost")
   *app*))
