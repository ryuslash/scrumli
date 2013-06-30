(in-package #:scrumli)

(defvar *scrumli-host* "http://localhost:8080"
  "The host currently running Scrumli. Used by Mozilla Persona.")

(defvar *scrumelo-bootstrap-css-location*
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css"
  "The location of the twitter bootstrap CSS file.")

(defvar *scrumelo-bootstrap-js-location*
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
  "The location of the twitter bootstrap JS file.")

(defvar *scrumelo-font-awesome-css-location*
  "http://netdna.bootstrapcdn.com/font-awesome/3.1.1/css/font-awesome.min.css"
  "The location of the font awesome CSS file.")

(defvar *scrumelo-jquery-js-location*
  "http://code.jquery.com/jquery-2.0.0.min.js"
  "The location of the jQuery JS file.")

(defvar *scrumelo-react-js-location*
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/react.min.js"
  "The location of the React JS file.")

(defvar *scrumelo-jsxtransformer-js-location*
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/JSXTransformer.js"
  "The location of the JSX Transformer JS file.")

(defun logged-in-p ()
  (hunchentoot:session-value :username))

(define-route main ("")
  (if (logged-in-p)
      (<:html
       (<:head (<:title "Hello!")
               (<:link :href *scrumelo-bootstrap-css-location*
                       :rel "stylesheet" :type "text/css")
               (<:link :href *scrumelo-font-awesome-css-location*
                       :rel "stylesheet" :type "text/css")
               (<:script :type "text/javascript"
                         :src *scrumelo-bootstrap-js-location*)
               (<:script :type "text/javascript"
                         :src *scrumelo-jquery-js-location*)
               (<:script :type "text/javascript"
                         :src *scrumelo-react-js-location*)
               (<:script :type "text/javascript"
                         :src *scrumelo-jsxtransformer-js-location*))
       (<:body
        (<:a :href "/logout" "Logout") " "
        (hunchentoot:session-value :username)
        (<:div :class "container"
               (<:h1 "Backlog")
               (<:div :id "content")
               (<:script :type "text/jsx" :src "js/main.js"))))
      (redirect 'login-page)))

(define-route react-ui ("js/main.js"
                        :content-type "application/ecmascript")
  (merge-pathnames "js/main.js" *static-directory*))

(define-route login-js ("js/login.js"
                        :content-type "application/ecmascript")
  (merge-pathnames "js/login.js" *static-directory*))

(define-route stories-json ("stories" :content-type "text/json")
  (if (logged-in-p)
      (with-output-to-string (out)
        (encode-json (get-all-stories) out))
      403))

(define-route stories-new ("stories/new" :method :post)
  (if (logged-in-p)
      (let ((role (hunchentoot:post-parameter "role"))
            (necessity (hunchentoot:post-parameter "necessity"))
            (title (hunchentoot:post-parameter "headline"))
            (content (hunchentoot:post-parameter "content")))
        (post-story role necessity title content
                    (hunchentoot:session-value :username))
        200)
      403))

(define-route stories-state ("stories/state" :method :post)
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id"))
             (current-state (story-get-state id)))
        (story-set-state id (ecase (intern current-state :scrumli)
                              (todo "DOING")
                              (doing "DONE")
                              (done "TODO")))
        200)
      403))

(define-route stories-priority ("stories/:dir" :method :post)
  (if (logged-in-p)
      (let* ((id (hunchentoot:post-parameter "id")))
        (story-change-priority id (intern (string-upcase dir) :keyword))
        200)
      403))

(define-route login-page ("login")
  (if (not (logged-in-p))
      (<:html :lang "en"
              (<:head (<:meta :charset "utf-8")
                      (<:title "Login")
                      (<:script :src "https://login.persona.org/include.js")
                      (<:script :src "/js/login.js"))
              (<:body
               (<:form :id "login-form" :method "POST" :action ""
                       (<:input :id "assertion-field" :type "hidden"
                                :name "assertion" :value ""))
               (<:p (<:a :href "javascript:login()" "Login"))))
      (redirect 'main)))

(define-route logout-page ("logout")
  (if (logged-in-p)
      (progn
        (setf (hunchentoot:session-value :username) nil)))
  (redirect 'login-page))

(defun json-to-string (obj)
  (with-output-to-string (out)
    (encode-json obj out)))

(defun verify-credentials (audience assertion)
  (let ((response
         (http-request "https://verifier.login.persona.org/verify"
                       :method :post :content-type "application/json"
                       :content (json-to-string
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

(define-route scrumelo-story ("stories/:id")
  (if (logged-in-p)
      (with-output-to-string (out)
        (encode-json (get-story id) out))
      403))
