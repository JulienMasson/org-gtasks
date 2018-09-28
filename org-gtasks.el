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
(require 'request)
(require 'cl-lib)

(defconst org-gtasks-token-url "https://www.googleapis.com/oauth2/v3/token"
  "Google OAuth2 server URL.")

(defconst org-gtasks-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gtasks-resource-url "https://www.googleapis.com/auth/tasks"
  "URL used to request access to tasks resources.")

(defconst org-gtasks-default-url "https://www.googleapis.com/tasks/v1")

(defvar org-gtasks-accounts nil)

(cl-defstruct org-gtasks
  (name nil :read-only t)
  directory
  client-id
  client-secret
  access-token
  refresh-token
  tasklists)

(cl-defstruct tasklist
  title
  file
  id
  tasks)

(defun org-gtasks-json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))

(defun org-gtasks-request-authorization (account)
  (browse-url (concat org-gtasks-auth-url
                      "?client_id=" (url-hexify-string (org-gtasks-client-id account))
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
                      "&scope=" (url-hexify-string org-gtasks-resource-url)))
  (read-string "Enter the code your browser displayed: "))

(defun org-gtasks-parse-errors (response &optional func &rest args)
  (let ((data (request-response-data response))
	(status (request-response-status-code response))
	(error-msg (request-response-error-thrown response)))
    (cond
     ((eq status nil)
      (message "Please check your network connectivity"))
     ((eq 401 (or (plist-get (plist-get data :error) :code)
                  status))
      (message "OAuth token expired. refresh access token")
      (setf (org-gtasks-access-token account) (org-gtasks-get-access-token account))
      (if (functionp func)
	  (apply func args)))
     ((eq 403 status)
      (message "Ensure you enabled the Tasks API through the Developers Console"))
     ((and (> 299 status) (eq data nil))
      (message "Received HTTP: %s" (number-to-string status))
      (message "Error occured, but no message body."))
     ((not (eq error-msg nil))
      (message "Status code: %s" (number-to-string status))
      (message "%s" (pp-to-string error-msg))))))

(defun org-gtasks-get-refresh-token (account)
  (let* ((response (request
		    org-gtasks-token-url
		    :type "POST"
		    :data `(("client_id" . ,(org-gtasks-client-id account))
			    ("client_secret" . ,(org-gtasks-client-secret account))
			    ("code" . ,(org-gtasks-request-authorization account))
			    ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
			    ("grant_type" . "authorization_code"))
		    :parser 'org-gtasks-json-read
		    :error (cl-function
			    (lambda (&key error-thrown &allow-other-keys)
			      (message "Got error: %S" error-thrown)))
		    :sync t))
	 (data (request-response-data response)))
    (when (plist-member data :refresh_token)
      (plist-get data :refresh_token))))

(defun org-gtasks-get-access-token (account)
  (let* ((response (request
		    org-gtasks-token-url
		    :type "POST"
		    :data `(("client_id" . ,(org-gtasks-client-id account))
			    ("client_secret" . ,(org-gtasks-client-secret account))
			    ("refresh_token" . ,(org-gtasks-refresh-token account))
			    ("grant_type" . "refresh_token"))
		    :parser 'org-gtasks-json-read
		    :error (cl-function
			    (lambda (&key error-thrown &allow-other-keys)
			      (message "Got error: %S" error-thrown)))
		    :sync t))
	 (data (request-response-data response)))
    (when (plist-member data :access_token)
      (plist-get data :access_token))))

(defun org-gtasks-read-local-refresh-token (account)
  (let* ((dir (org-gtasks-directory account))
	 (file (concat dir ".refresh_token")))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(buffer-string)))))

(defun org-gtasks-get-local-refresh-token (account)
  (let* ((dir (org-gtasks-directory account))
	 (file (concat dir ".refresh_token"))
	 (refresh-token (org-gtasks-get-refresh-token account)))
    (unless (file-exists-p file)
      (find-file-noselect file))
    (with-temp-file file
      (insert refresh-token))))

(defun org-gtasks-check-token (account)
  (let ((refresh-token (org-gtasks-refresh-token account))
	(local-refresh-token (org-gtasks-read-local-refresh-token account))
	(access-token (org-gtasks-access-token account)))
    (unless refresh-token
      (if local-refresh-token
	  (setf (org-gtasks-refresh-token account) local-refresh-token)
	(org-gtasks-get-local-refresh-token account)
	(setf (org-gtasks-refresh-token account) (org-gtasks-read-local-refresh-token account))))
    (unless access-token
      (setf (org-gtasks-access-token account) (org-gtasks-get-access-token account)))))

(defun org-gtasks-get-tasks (account tasklist)
  (let* ((id (tasklist-id tasklist))
	 (url (format "%s/lists/%s/tasks" org-gtasks-default-url id))
	 (response (request
		    url
		    :type "GET"
		    :params `(("access_token" . ,(org-gtasks-access-token account))
			      ("key" . ,(org-gtasks-client-secret account))
			      ("singleEvents" . "True")
			      ("orderBy" . "startTime")
			      ("grant_type" . "authorization_code"))
		    :parser 'org-gtasks-json-read
		    :sync t))
	 (data (request-response-data response)))
    (org-gtasks-parse-errors response #'org-gtasks-get-tasks account tasklist)
    (when (plist-member data :items)
      (setf (tasklist-tasks tasklist) (plist-get data :items)))))

(defun org-gtasks-get-taskslists (account)
  (setf (org-gtasks-tasklists account) nil)
  (let* ((url (concat org-gtasks-default-url "/users/@me/lists"))
	 (response (request
		    url
		    :type "GET"
		    :params `(("access_token" . ,(org-gtasks-access-token account))
			      ("key" . ,(org-gtasks-client-secret account))
			      ("singleEvents" . "True")
			      ("orderBy" . "startTime")
			      ("grant_type" . "authorization_code"))
		    :parser 'org-gtasks-json-read
		    :sync t))
	 (data (request-response-data response)))
    (org-gtasks-parse-errors response #'org-gtasks-get-taskslists account)
    (when (plist-member data :items)
      (setf (org-gtasks-tasklists account)
	    (mapcar (lambda (item)
		      (let* ((title (plist-get item :title))
			     (file (format "%s.org" title))
			     (id (plist-get item :id)))
			(make-tasklist :title title :file file :id id)))
		    (plist-get data :items))))))

(defun org-gtasks-write-to-org (account tasklist)
  (let* ((default-directory (org-gtasks-directory account))
	 (file (tasklist-file tasklist))
	 (title (tasklist-title tasklist))
	 (header (format "#+FILETAGS: :%s:\n" title))
	 (tasks (tasklist-tasks tasklist)))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (insert header)
      (insert (mapconcat 'identity
			 (mapcar (lambda (lst)
				   (org-gtasks-task lst))
				 tasks) ""))
      (goto-char (point-min))
      (org-sort-entries nil ?o)
      (org-set-startup-visibility)
      (save-buffer))))

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

(defun org-gtasks-push-task (account url action data-list)
  (request
   url
   :type action
   :headers '(("Content-Type" . "application/json"))
   :data (json-encode data-list)
   :params `(("access_token" . ,(org-gtasks-access-token account))
	     ("key" . ,(org-gtasks-client-secret account))
	     ("grant_type" . "authorization_code"))
   :parser 'org-gtasks-json-read
   :error (cl-function
	   (lambda (&key response &allow-other-keys)
	     (let ((status (request-response-status-code response))
		   (error-msg (request-response-error-thrown response)))
	       (message "Status code: %s" (number-to-string status))
	       (message "%s" (pp-to-string error-msg)))))))

(defun org-gtasks-delete-task (account tasklist-id task-id)
  (request
   (format "%s/lists/%s/tasks/%s" org-gtasks-default-url tasklist-id task-id)
   :type "DELETE"
   :headers '(("Content-Type" . "application/json"))
   :params `(("access_token" . ,(org-gtasks-access-token account))
	     ("key" . ,(org-gtasks-client-secret account))
	     ("grant_type" . "authorization_code"))
   :parser 'org-gtasks-json-read
   :error (cl-function
	   (lambda (&key response &allow-other-keys)
	     (let ((status (request-response-status-code response))
		   (error-msg (request-response-error-thrown response)))
	       (message "Status code: %s" (number-to-string status))
	       (message "%s" (pp-to-string error-msg)))))))

(defun org-gtasks-find-action (tasks id)
  (if (seq-find (lambda (task)
		  (string= (plist-get task :id) id))
		tasks)
      "PATCH"
    "POST"))

(defun org-gtasks-push-tasklist (account tasklist)
  (let ((default-directory (org-gtasks-directory account))
	(file (tasklist-file tasklist))
	(title (tasklist-title tasklist))
	(tasks (tasklist-tasks tasklist))
	list-id)
    (with-current-buffer (find-file-noselect file)
      (org-element-map (org-element-parse-buffer) 'headline
	(lambda (hl)
	  (let* ((url (format "%s/lists/%s/tasks" org-gtasks-default-url (tasklist-id tasklist)))
		 (id (org-element-property :ID hl))
		 (action (org-gtasks-find-action tasks id))
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
			  ""))
		 (data-list `(("title" . ,title)
			      ("notes" . ,notes)
			      ("status" . ,status))))
	    (push id list-id)
	    (when completed
	      (add-to-list 'data-list `("completed" . ,completed)))
	    (when (string= action "PATCH")
	      (setq url (format "%s/%s" url id)))
	    (org-gtasks-push-task account url action data-list)))))
    ;; push deleted tasks
    (mapc (lambda (task)
	    (let ((task-id (plist-get task :id))
		  (tasklist-id (tasklist-id tasklist)))
	      (unless (member task-id list-id)
		(org-gtasks-delete-task account tasklist-id task-id))))
	  tasks)))

(defun org-gtasks-create-tasklist (account name)
  (let* ((response (request
		    (concat org-gtasks-default-url "/users/@me/lists")
		    :type "POST"
		    :headers '(("Content-Type" . "application/json"))
		    :data (json-encode `(("title" . ,name)))
		    :params `(("access_token" . ,(org-gtasks-access-token account))
			      ("key" . ,(org-gtasks-client-secret account))
			      ("grant_type" . "authorization_code"))
		    :parser 'org-gtasks-json-read
		    :error (cl-function
			    (lambda (&key response &allow-other-keys)
			      (let ((status (request-response-status-code response))
				    (error-msg (request-response-error-thrown response)))
				(message "Status code: %s" (number-to-string status))
				(message "%s" (pp-to-string error-msg)))))
		    :sync t))
	 (data (request-response-data response)))
    (when (and (plist-member data :title)
	       (plist-member data :id))
      (let ((title (plist-get data :title))
	    (id (plist-get data :id)))
	(push (make-tasklist :title title :id id)
	      (org-gtasks-tasklists account))))))

(defun org-gtasks-delete-tasklist (account id)
  (request
   (format "%s/users/@me/lists/%s" org-gtasks-default-url id)
   :type "DELETE"
   :headers '(("Content-Type" . "application/json"))
   :params `(("access_token" . ,(org-gtasks-access-token account))
	     ("key" . ,(org-gtasks-client-secret account))
	     ("grant_type" . "authorization_code"))
   :parser 'org-gtasks-json-read
   :error (cl-function
	   (lambda (&key response &allow-other-keys)
	     (let ((status (request-response-status-code response))
		   (error-msg (request-response-error-thrown response)))
	       (message "Status code: %s" (number-to-string status))
	       (message "%s" (pp-to-string error-msg)))))))

(defun org-gtasks-check-tasklists (account)
  (let* ((dir (org-gtasks-directory account))
	 (tasklists (org-gtasks-tasklists account))
	 (local-files (seq-filter (lambda (str)
				    (string-match "\\.org$" str))
				  (directory-files dir)))
	 (files (mapcar 'tasklist-file tasklists)))
    ;; create taskslist
    (mapc (lambda (file)
	    (unless (seq-find (lambda (f)
				(string= f file))
			      files)
	      (org-gtasks-create-tasklist account (file-name-base file))))
	  local-files)
    ;; delete tasklist
    (mapc (lambda (tasklist)
	    (let ((file (tasklist-file tasklist))
		  (id (tasklist-id tasklist)))
	      (unless (seq-find (lambda (f)
				  (string= f file))
				local-files)
		(org-gtasks-delete-tasklist account id)
		(setf (org-gtasks-tasklists account)
		      (delete tasklist tasklists)))))
	  tasklists)))

(defun org-gtasks-fetch (account)
  (org-gtasks-check-token account)
  (org-gtasks-get-taskslists account)
  (let ((tasklists (org-gtasks-tasklists account)))
    (when tasklists
      (mapc (lambda (tasklist)
	      (org-gtasks-get-tasks account tasklist))
	    tasklists)
      (message "Fetch %s done" (org-gtasks-name account)))))

(defun org-gtasks-push (account)
  ;; FIXME, force to refresh access token since org-gtasks-push-task doesn't handle
  ;; properly the refresh ...
  (setf (org-gtasks-access-token account) nil)
  (org-gtasks-fetch account)
  (org-gtasks-check-tasklists account)
  (let ((tasklists (org-gtasks-tasklists account)))
    (when tasklists
      (mapc (lambda (tasklist)
	      (org-gtasks-push-tasklist account tasklist))
	    tasklists)
      (message "Push %s done" (org-gtasks-name account)))))

(defun org-gtasks-pull (account)
  (org-gtasks-fetch account)
  (let ((tasklists (org-gtasks-tasklists account)))
    (when tasklists
      (mapc (lambda (tasklist)
	      (org-gtasks-write-to-org account tasklist))
	    tasklists)
      (message "Pull %s done" (org-gtasks-name account)))))

(defvar org-gtasks-actions
  '(("Push" . org-gtasks-push)
    ("Pull" . org-gtasks-pull)))

(defun org-gtasks ()
  (interactive)
  (let* ((name (completing-read "Select Account: "
				(mapcar (lambda (account)
					  (org-gtasks-name account))
					org-gtasks-accounts)))
	 (account (seq-find (lambda (account)
			      (string= (org-gtasks-name account) name))
			    org-gtasks-accounts))
	 (action (completing-read (format "Action (%s): " name)
				  (mapcar 'car org-gtasks-actions)))
	 (func (assoc-default action org-gtasks-actions)))
    (if (functionp func)
	(funcall func account))))

(defun org-gtasks-account-eq (a1 a2)
  (when (and (org-gtasks-p a1) (org-gtasks-p a2))
    (or (string= (org-gtasks-name a1) (org-gtasks-name a2))
	(string= (org-gtasks-directory a1) (org-gtasks-directory a2)))))

(defun org-gtasks-register-account (&rest plist)
  (let ((name (plist-get plist :name))
	(directory (file-name-as-directory
		    (expand-file-name (plist-get plist :directory))))
	(client-id (plist-get plist :client-id))
	(client-secret (plist-get plist :client-secret)))
    (unless (file-directory-p directory)
      (make-directory directory))
    (add-to-list 'org-gtasks-accounts
		 (make-org-gtasks :name name
				  :directory directory
				  :client-id client-id
				  :client-secret client-secret)
		 t 'org-gtasks-account-eq)))


(provide 'org-gtasks)
