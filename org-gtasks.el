;;; org-gtasks.el

;; Copyright (C) 2018 Julien Masson

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'json)
(require 'request-deferred)
(require 'cl-lib)

(defgroup org-gtasks nil "Org sync with Google Tasks"
  :tag "Org google tasks"
  :group 'org)

(defcustom org-gtasks-client-id nil
  "Client ID for OAuth."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-client-secret nil
  "Google tasks secret key for OAuth."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-access-token nil
  "Google tasks access token"
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-refresh-token nil
  "Google tasks refresh token"
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-file nil
  "Org file where we store Google tasks"
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-file-header nil
  "Org file header"
  :group 'org-gtasks
  :type 'string)

(defconst org-gtasks-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gtasks-token-url "https://www.googleapis.com/oauth2/v3/token"
  "Google OAuth2 server URL.")

(defconst org-gtasks-resource-url "https://www.googleapis.com/auth/tasks"
  "URL used to request access to tasks resources.")

(defconst org-gtasks-key-url (concat "?key=" org-gtasks-client-secret))

(defconst org-gtasks-default-url "https://www.googleapis.com/tasks/v1/lists/@default/tasks")

(defvar org-gtasks-tasks nil)

(defun org-gtasks-json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))

(defun org-gtasks-request-authorization ()
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url (concat org-gtasks-auth-url
                      "?client_id=" (url-hexify-string org-gtasks-client-id)
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
                      "&scope=" (url-hexify-string org-gtasks-resource-url)))
  (read-string "Enter the code your browser displayed: "))

(defun org-gtasks-get-access-token ()
  "Refresh OAuth access at TOKEN-URL."
  (deferred:$
    (request-deferred
     org-gtasks-token-url
     :type "POST"
     :data `(("client_id" . ,org-gtasks-client-id)
             ("client_secret" . ,org-gtasks-client-secret)
             ("refresh_token" . ,org-gtasks-refresh-token)
             ("grant_type" . "refresh_token"))
     :parser 'org-gtasks-json-read
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Got error: %S" error-thrown))))
    (deferred:nextc it
      (lambda (response)
	(let ((data (request-response-data response)))
	  (setq org-gtasks-access-token (plist-get data :access_token)))))
    (deferred:nextc it
      (lambda (tmp)
        (org-gtasks-pull)))))

(defun org-gtasks-request-token ()
  "Request OAuth access at TOKEN-URL."
  (request
   org-gtasks-token-url
   :type "POST"
   :data `(("client_id" . ,org-gtasks-client-id)
           ("client_secret" . ,org-gtasks-client-secret)
           ("code" . ,(org-gtasks-request-authorization))
           ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
           ("grant_type" . "authorization_code"))
   :parser 'org-gtasks-json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (when data
		 (setq org-gtasks-access-token (plist-get data :access_token))
		 (setq org-gtasks-refresh-token (plist-get data :refresh_token)))))
   :error (cl-function
	   (lambda (&key error-thrown &allow-other-keys)
             (message "Got error: %S" error-thrown)))))

(defun org-gtasks-ensure-token ()
  (unless (or org-gtasks-access-token org-gtasks-refresh-token)
    (org-gtasks-request-token)))

(defun org-gtasks-format-iso2org (str)
  (format-time-string "%Y-%m-%d %a %H:%M" (date-to-time str)))

(defun org-gtasks-format-org2iso (year mon day hour min)
  (let ((seconds (time-to-seconds (encode-time 0 min hour day mon year))))
    (concat (format-time-string "%Y-%m-%dT%H:%M" (seconds-to-time seconds))
	    ":00Z")))

(defun org-gtasks-task (plst)
  (let* ((id  (plist-get plst :id))
	 (title  (plist-get plst :title))
	 (notes  (plist-get plst :notes))
	 (status (if (string= "completed" (plist-get plst :status))
		     "DONE"
		   "TODO"))
	 (completed (plist-get plst :completed)))
    (concat (format "* %s %s\n" status title)
	    (when completed
	      (format "  CLOSED: [%s]\n" (org-gtasks-format-iso2org completed)))
	    "  :PROPERTIES:\n"
	    "  :ID: " id "\n"
	    "  :END:\n"
	    (when notes) notes (when notes "\n"))))

(defun org-gtasks-write-file ()
  (with-current-buffer (find-file-noselect org-gtasks-file)
    (erase-buffer)
    (when org-gtasks-file-header
      (insert org-gtasks-file-header))
    (insert (mapconcat 'identity
		       (mapcar (lambda (lst)
				 (org-gtasks-task lst))
			       org-gtasks-tasks) ""))
    (org-set-startup-visibility)
    (save-buffer)))

(defun org-gtasks-pull ()
  (interactive)
  (org-gtasks-ensure-token)
  (deferred:$
    (request-deferred
     org-gtasks-default-url
     :type "GET"
     :params `(("access_token" . ,org-gtasks-access-token)
               ("key" . ,org-gtasks-client-secret)
               ("singleEvents" . "True")
	       ("orderBy" . "startTime")
               ("grant_type" . "authorization_code"))
     :parser 'org-gtasks-json-read
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Got error: %S" error-thrown))))
    (deferred:nextc it
      (lambda (response)
        (let
            ((temp (request-response-data response))
             (status (request-response-status-code response))
             (error-msg (request-response-error-thrown response)))
          (cond
           ;; If there is no network connectivity, the response will
           ;; not include a status code.
           ((eq status nil)
            (message "Please check your network connectivity"))
           ;; Receiving a 403 response could mean that the tasks
           ;; API has not been enabled. When the user goes and
           ;; enables it, a new token will need to be generated. This
           ;; takes care of that step.
           ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                        status))
	    (message "Received HTTP 401")
	    (message "OAuth token expired. Now trying to refresh-token")
            (deferred:next
              (lambda() (org-gtasks-get-access-token))))
           ((eq 403 status)
            (message "Received HTTP 403")
            (message "Ensure you enabled the Tasks API through the Developers Console, then try again."))
           ;; We got some 2xx response, but for some reason no
           ;; message body.
           ((and (> 299 status) (eq temp nil))
	    (message "Received HTTP: %s" (number-to-string status))
	    (message "Error occured, but no message body."))
           ((not (eq error-msg nil))
	    ;; Generic error-handler meant to provide useful
	    ;; information about failure cases not otherwise
	    ;; explicitly specified.
	    (message "Status code: %s" (number-to-string status))
	    (message "%s" (pp-to-string error-msg)))
           ;; Fetch was successful.
           (t
	    (setq org-gtasks-tasks (plist-get (request-response-data response) :items))
	    (org-gtasks-write-file))))))))

(defun org-gtasks-post (title notes status id completed)
  (let ((data-list `(("title" . ,title)
                     ("notes" . ,notes)
                     ("status" . ,status))))
    (when completed
      (add-to-list 'data-list `("completed" . ,completed)))
    (org-gtasks-ensure-token)
    (request
     (concat
      org-gtasks-default-url
      (when id
	(concat "/" id)))
     :type (if id "PATCH" "POST")
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode data-list)
     :params `(("access_token" . ,org-gtasks-access-token)
               ("key" . ,org-gtasks-client-secret)
               ("grant_type" . "authorization_code"))
     :parser 'org-gtasks-json-read
     :error (cl-function
             (lambda (&key response &allow-other-keys)
               (let ((status (request-response-status-code response))
                     (error-msg (request-response-error-thrown response)))
		 (cond
                  ((eq status 401)
		   (message "Received HTTP 401")
		   (message "OAuth token expired. Now trying to refresh-token")
		   (org-gtasks-get-access-token))
                  (t
		   (message "Status code: %s" (number-to-string status))
		   (message "%s" (pp-to-string error-msg))))))))))

(defun org-gtasks-find-id (id)
  (plist-get (seq-find (lambda (task)
			 (string= (plist-get task :id) id))
		       org-gtasks-tasks) :id))

(defun org-gtasks-push ()
  (interactive)
  (with-current-buffer (find-file-noselect org-gtasks-file)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
	(let* ((id (org-gtasks-find-id (org-element-property :ID hl)))
	       (title (org-element-interpret-data
		       (org-element-property :title hl)))
	       (closed (org-element-property :closed hl))
	       (completed (when closed
			    (org-gtasks-format-org2iso
			     (plist-get (cadr closed) :year-start)
			     (plist-get (cadr closed) :month-start)
			     (plist-get (cadr closed) :day-start)
			     (plist-get (cadr closed) :hour-start)
			     (plist-get (cadr closed) :minute-start))))
	       (status (if (string= (org-element-property :todo-type hl) "done")
			   "completed"
			 "needsAction"))
	       (notes (if (plist-get (cadr hl) :contents-begin)
			  (replace-regexp-in-string "\\(.*\n\\)*.*END:\n"
						    ""
						    (buffer-substring-no-properties
						     (plist-get (cadr hl) :contents-begin)
						     (plist-get (cadr hl) :contents-end)))
			"")))
	  (org-gtasks-post title notes status id completed))))))


(provide 'org-gtasks)
