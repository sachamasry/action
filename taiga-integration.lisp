(in-package :cl-user)
(defpackage :action/taiga-integration
  (:use :cl)
  (:import-from :action
                #:+action-data-directory+)
  (:import-from :ubiquitous
                #:restore
                #:value
                #:designator-pathname)
  (:import-from :cl-strings
                #:join
                #:split
                #:clean)
  (:import-from :alexandria
                #:when-let)
  (:import-from :dexador
                #:head
                ;; #:get
                #:post
                #:put)
  (:import-from :jonathan
                #:parse)
  (:export #:get-authorisation-token
           #:construct-api-url
           #:taiga-bridge-test
           #:taiga-integration-active-p))
(in-package :action/taiga-integration)

;; Connect to saved configuration
(restore
 (designator-pathname
  (merge-pathnames +action-data-directory+
                   "action-taiga-bridge.conf")
  :lisp))

(defparameter +taiga-url-protocol+ "http")
(defparameter +taiga-url+ "127.0.0.1")
(defparameter +taiga-url-port+ "8000")
(defparameter +taiga-api-endpoint+ "api")
(defparameter +taiga-api-version+ "v1")
(defparameter +taiga-username+ "admin")
(defparameter +taiga-password+ "123123")
(defparameter +taiga-auth-token+ ())

(defvar *default-project-id* ())

(defun taiga-integration-active-p ()
  "Is Taiga integration activated for this dataset?"
  (value 'taiga 'integration 'active))

(defun construct-api-url (slug &rest parameters)
  "Construct the full Taiga request URL.

Given the static Taiga URL components:

PROTOCOL, URL, PORT, API-ENDPOINT, API-VERSION,
construct a legal complete URI, attach to that the slug
and the parameters, if any.

If parameters are given, ensure they're even (key and value pairs),
attaching key and value with an equals sign, prepending all parameters
with a questionmark '?' separating key-value pairs with
an ampersand '&' character."
  (concatenate 'string
               (value :taiga-url-protocol) "://"
               (value :taiga-url)
               (if (value :taiga-url-port)
                   (concatenate 'string ":" (value :taiga-url-port)))
               "/"
               (value :taiga-api-endpoint) "/"
               (value :taiga-api-version) "/"
               slug
               (when
                   (and parameters
                        (evenp (length parameters)))
                 (format nil "?~{~a=~a~^&~}"
                         parameters))))

(defun taiga-bridge-test ()
  "Test connectivity to Taiga."
  (when-let (response
             (dex:get (construct-api-url "")))
    (format t "~a~%~%" response)
    t))
          

(defun get-authorisation-token ()
  "Returns a valid Taiga authentication token.

Using the provided static username and password,
the constructed Taiga API URL, the result is parsed,
with the value of :|auth_token| returned.

If a bad request error results, due to incorrect
username or password, returns nil, for failure.

Otherwise, the parsed authentication token is returned and
the +TAIGA-AUTH-TOKEN+ is set to this value."
  (if +taiga-auth-token+
      +taiga-auth-token+
      (when-let (http-response
                 (handler-bind
                     ((dexador.error:http-request-bad-request
                        #'dex:ignore-and-continue))
                   (post
                    (construct-api-url "auth")
                    :content (list (cons "username" (value :taiga-username))
                                   (cons "password"  (value :taiga-password))
                                   (cons "type" "normal")))))
        (when-let (auth-token
                   (parse http-response
                          :keywords-to-read '("auth_token")
                          :junk-allowed t))
          (setf +taiga-auth-token+ (second auth-token))))))

(defun get-project-list ()
  ""
  (when-let (parsed-response
             (parse
              (handler-bind
                  ((dexador.error:http-request-bad-request
                     #'dex:ignore-and-continue))
                (dex:get (action/taiga-integration:construct-api-url "projects") 
                     :headers '(("Authorization" . (format nil "Bearer ~a" *token*)))))))
    (mapcar #'(lambda (project) (list :name (getf project :|name|)
                                      :id (getf project :|id|)))
            parsed-response)))

(defun get-project-id (name)
  ""
  (when (and (stringp name)
             (plusp (length name)))
    (first
     (remove-if #'null
                (mapcar #'(lambda (sublist) (when
                                                (string-equal name
                                                              (getf sublist :name))
                                              (getf sublist :id)))
                        (get-project-list))))))
