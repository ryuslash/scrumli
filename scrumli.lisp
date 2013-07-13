(in-package #:scrumli)

(defvar *scrumli-host* "http://localhost:8080"
  "The host currently running Scrumli. Used by Mozilla Persona.")

(defvar *scrumli-bootstrap-css-location*
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css"
  "The location of the twitter bootstrap CSS file.")

(defvar *scrumli-bootstrap-js-location*
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
  "The location of the twitter bootstrap JS file.")

(defvar *scrumli-font-awesome-css-location*
  "http://netdna.bootstrapcdn.com/font-awesome/3.1.1/css/font-awesome.min.css"
  "The location of the font awesome CSS file.")

(defvar *scrumli-jquery-js-location*
  "http://code.jquery.com/jquery-2.0.0.min.js"
  "The location of the jQuery JS file.")

(defvar *scrumli-react-js-location*
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/react.min.js"
  "The location of the React JS file.")

(defvar *scrumli-jsxtransformer-js-location*
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/JSXTransformer.js"
  "The location of the JSX Transformer JS file.")

(defun logged-in-p ()
  (hunchentoot:session-value :username))

(defun page-title (title)
  (<:title (concatenate 'string title " | scrumli")))

(defun css (&rest sheets)
  (apply 'concatenate 'string
         (mapcar (lambda (s)
                   (<:link :href s :rel "stylesheet" :type "text/css"))
                 sheets)))

(defun js (&rest scripts)
  (apply 'concatenate 'string
         (mapcar (lambda (s)
                   (<:script :type "text/javascript" :src s))
                 scripts)))

(defmacro navbar (&body body)
  `(<:div :class "navbar navbar-static-top navbar-inverse"
          (<:div :class "navbar-inner"
                 (<:div :class "container"
                        (<:a :class "brand" "scrumli")
                        ,@body))))

(define-route main ("")
  (if (logged-in-p)
      (<:html
       (<:head (page-title "Backlog")
               (css *scrumli-bootstrap-css-location*
                    *scrumli-font-awesome-css-location*)
               (js *scrumli-bootstrap-js-location*
                   *scrumli-jquery-js-location*
                   *scrumli-react-js-location*
                   *scrumli-jsxtransformer-js-location*))
       (<:body
        (navbar (<:div :class "pull-right"
                       (<:span :class "navbar-text"
                               (hunchentoot:session-value :username))
                       (<:ul :class "nav pull-right"
                             (<:li :class "divider-vertical")
                             (<:li (<:a :href (genurl 'logout-page)
                                        "Logout"))
                             (<:li :class "divider-vertical"))))
        (<:div :class "container"
               (<:h1 "Backlog")
               (<:div :id "content")
               (<:script :type "text/jsx" :src (genurl 'main-js)))))
      (redirect 'login-page)))

(defmacro serve-static (name relpath)
  `(define-route ,name (,relpath :content-type "application/ecmascript")
     (merge-pathnames ,relpath *static-directory*)))

(serve-static main-js "js/main.js")
(serve-static login-js "js/login.js")

(define-route stories-json ("stories" :content-type "text/json")
  (if (logged-in-p)
      (encode-json-to-string (get-all-stories))
      403))

(defmacro with-post-parameters (parameters &body body)
  `(let ,(mapcar (lambda (p)
                   (list (intern (string-upcase p))
                         `(hunchentoot:post-parameter ,p)))
                 parameters)
     ,@body))

(define-route stories-new ("stories/new" :method :post
                                         :content-type "text/json")
  (if (logged-in-p)
      (with-post-parameters ("role" "necessity" "headline" "content")
        (post-story role necessity headline content
                    (hunchentoot:session-value :username))
        (encode-json-to-string '((status . "ok"))))
      403))

(define-route tasks-new ("stories/tasks/new" :method :post)
  (if (logged-in-p)
      (with-post-parameters ("storyId" "description")
        (post-task storyid description
                   (hunchentoot:session-value :username))
        200)
      403))

(define-route stories-state ("stories/state" :method :post
                                             :content-type "text/json")
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id"))
             (current-state (story-get-state 'story id))
             (next (ecase (intern current-state :scrumli)
                     (todo "DOING")
                     (doing "DONE")
                     (done "TODO"))))
        (story-set-state 'story id next)
        (encode-json-to-string `((status . "ok") (state . ,next))))
      403))

(define-route task-state ("tasks/state" :method :post
                                        :content-type "text/json")
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id"))
             (current-state (story-get-state 'task id))
             (next (ecase (intern current-state :scrumli)
                     (todo "DOING")
                     (doing "DONE")
                     (done "TODO"))))
        (story-set-state 'task id next)
        (encode-json-to-string `((status . "ok") (state . ,next))))
      403))

(define-route stories-priority ("stories/:dir" :method :post
                                               :content-type "text/json")
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id")))
        (story-change-priority
         'story id (intern (string-upcase dir) :keyword))
        (encode-json-to-string '((status . "ok"))))
      403))

(define-route task-priority ("tasks/:dir" :method :post)
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id")))
        (story-change-priority
         'task id (intern (string-upcase dir) :keyword))
        200)
      403))

(define-route login-page ("login")
  (if (not (logged-in-p))
      (<:html :lang "en"
              (<:head (<:meta :charset "utf-8")
                      (page-title "Login")
                      (css *scrumli-bootstrap-css-location*)
                      (js *scrumli-bootstrap-js-location*
                          "https://login.persona.org/include.js"
                          (genurl 'login-js)))
              (<:body
               (navbar (<:ul :class "nav pull-right"
                             (<:li :class "divider-vertical")
                             (<:li (<:a :href "javascript:login()"
                                        "Login"))
                             (<:li :class "divider-vertical")))
               (<:div :class "container"
                      (<:br)
                      (<:div :class "hero-unit"
                             (<:h1 "Scrumli")
                             (<:p "As a " (<:em "developer") " I "
                                  (<:em "love") " to " (<:em "scrum")
                                  "...")
                             (<:a :class "btn btn-primary btn-large"
                                  :href "javascript:login()"
                                  "Login")))
               (<:form :id "login-form" :method "POST" :action ""
                       (<:input :id "assertion-field" :type "hidden"
                                :name "assertion" :value ""))))
      (redirect 'main)))

(define-route logout-page ("logout")
  (if (logged-in-p)
      (setf (hunchentoot:session-value :username) nil))
  (redirect 'login-page))

(defun verify-credentials (audience assertion)
  (let ((response
         (http-request "https://verifier.login.persona.org/verify"
                       :method :post :content-type "application/json"
                       :content (encode-json-to-string
                                 `(("assertion" . ,assertion)
                                   ("audience" . ,audience)))
                       :want-stream t)))
    (decode-json response)))

(define-route login-page/post ("login" :method :post)
  (let ((result (verify-credentials
                 *scrumli-host*
                 (hunchentoot:post-parameter "assertion"))))
    (if (equal (cdr (assoc :status result)) "okay")
        (progn
          (hunchentoot:start-session)
          (setf (hunchentoot:session-value :username)
                (cdr (assoc :email result)))
          (redirect 'main))
        403)))

(define-route scrumli-story ("stories/:id" :content-type "json")
  (if (logged-in-p)
      (encode-json-to-string (get-story id))
      403))
