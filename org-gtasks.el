;;; org-gtasks.el --- Export/import all Google Tasks to org files.

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/org-gtasks
;; Version: 0.1
;; Maintainer: massonju.eseo
;; Copyright (C) :2018-2019 Julien Masson all rights reserved.
;; Created: :30-08-18
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (org "8.2.4"))
;; Keywords: convenience,

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'deferred)
(require 'json)
(require 'request-deferred)

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

(defconst org-gtasks-token-url "https://www.googleapis.com/oauth2/v3/token"
  "Google OAuth2 server URL.")

(defconst org-gtasks-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gtasks-resource-url "https://www.googleapis.com/auth/tasks"
  "URL used to request access to tasks resources.")

(defconst org-gtasks-default-url "https://www.googleapis.com/tasks/v1")

(defconst org-gtasks-links-drawer-re
  (concat "\\("
          "^[ \t]*:links:[ \t]*$"
          "\\)[^\000]*?\\("
          "^[ \t]*:end:[ \t]*$"
          "\\)\n?")
  "Matches an entire org-gtasks links drawer.")

(defvar org-gtasks-actions '(("Push"   . org-gtasks-push)
			     ("Pull"   . org-gtasks-pull)
			     ("Add"    . org-gtasks-add)
			     ("Remove" . org-gtasks-remove)))

(defvar org-gtasks-accounts nil)

;; utils
(defun array-to-list (array)
  (mapcar 'identity array))

(defun org-gtasks-json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))

(defun org-gtasks-local-files (account)
  (cl-remove-if-not (lambda (file) (string-match "\\.org$" file))
		    (directory-files (org-gtasks-directory account))))

(defun org-gtasks-find-account-by-name (name)
  (cl-find-if (lambda (account) (string= (org-gtasks-name account) name))
	      org-gtasks-accounts))

(defun org-gtasks-find-account-by-dir (dir)
  (cl-find-if (lambda (account) (string= (org-gtasks-directory account) dir))
	      org-gtasks-accounts))

(defun org-gtasks-find-tasklist (tasklists title)
  (cl-find-if (lambda (tasklist) (string= (tasklist-title tasklist) title))
	      tasklists))

;; request
(defun org-gtasks-parse-errors (account response &optional on-token-refresh)
  (let ((data (request-response-data response))
	(status (request-response-status-code response))
	(error-msg (request-response-error-thrown response))
	return-status)
    (cond
     ((null status)
      (error "Please check your network connectivity"))
     ((eq 401 (or status (plist-get (plist-get data :error) :code)))
      (message "OAuth token expired. refresh access token")
      (if-let ((access-token (org-gtasks-get-access-token account)))
	  (progn
	    (setf (org-gtasks-access-token account) access-token)
	    (if on-token-refresh (funcall on-token-refresh account)))
	(error "Cannot get access token")))
     ((eq 403 status)
      (error "Ensure you enabled the Tasks API through the Developers Console"))
     ((and (> status 299) (null data))
      (error "Error HTTP status: %s" (number-to-string status)))
     ((not (null error-msg))
      (error "Error %s: %s" (number-to-string status) (pp-to-string error-msg)))
     (t (setq return-status t)))
    return-status))

(cl-defun org-gtasks-request (account url &key type (headers nil) (data nil) (params nil))
  (let (org-gtasks-request-data
	(d (deferred:$ (request-deferred url
					 :type type
					 :headers headers
					 :data data
					 :params params
					 :parser 'org-gtasks-json-read)
	     (deferred:nextc it
	       `(lambda (response)
		  (when (org-gtasks-parse-errors ,account response)
		    (setq org-gtasks-request-data (request-response-data response))))))))
    (deferred:sync! d)
    org-gtasks-request-data))

(cl-defun org-gtasks-request-async (account url cb &key type (headers nil) (data nil)
					    (params nil))
  (let ((on-token-refresh `(lambda (account)
			     (when-let ((access-token (org-gtasks-access-token account))
					(params ',params))
			       (when (assoc "access_token" params)
				 (setcdr (assoc "access_token" params) access-token)
				 (org-gtasks-request-async account ,url ,cb
							   :type ,type
							   :headers ',headers
							   :data ',data
							   :params params))))))
    (deferred:$ (request-deferred url
				  :type type
				  :headers headers
				  :data data
				  :params params
				  :parser 'org-gtasks-json-read)
      (deferred:nextc it
	`(lambda (response)
	   (when (org-gtasks-parse-errors ,account response ,on-token-refresh)
	     (funcall ,cb (request-response-data response))))))))

;; token
(defun org-gtasks-request-authorization (account)
  (browse-url (concat org-gtasks-auth-url
                      "?client_id=" (url-hexify-string (org-gtasks-client-id account))
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
                      "&scope=" (url-hexify-string org-gtasks-resource-url)))
  (read-string "Enter the code your browser displayed: "))

(defun org-gtasks-get-refresh-token (account)
  (let ((data (org-gtasks-request
	       account
	       org-gtasks-token-url
	       :type "POST"
	       :data `(("client_id" . ,(org-gtasks-client-id account))
		       ("client_secret" . ,(org-gtasks-client-secret account))
		       ("code" . ,(org-gtasks-request-authorization account))
		       ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
		       ("grant_type" . "authorization_code")))))
    (when (plist-member data :refresh_token)
      (plist-get data :refresh_token))))

(defun org-gtasks-get-access-token (account)
  (let ((data (org-gtasks-request
	       account
	       org-gtasks-token-url
	       :type "POST"
	       :data `(("client_id" . ,(org-gtasks-client-id account))
		       ("client_secret" . ,(org-gtasks-client-secret account))
		       ("refresh_token" . ,(org-gtasks-refresh-token account))
		       ("grant_type" . "refresh_token")))))
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

;; pull
(defun org-gtasks-request-fetch (account url cb)
  (org-gtasks-request-async account url cb
			    :type "GET"
			    :params `(("access_token" . ,(org-gtasks-access-token account))
				      ("singleEvents" . "True")
				      ("orderBy"      . "startTime")
				      ("grant_type"   . "authorization_code"))))

(defun org-gtasks-task (plst)
  (let* ((id  (plist-get plst :id))
	 (title  (plist-get plst :title))
	 (due (plist-get plst :due))
	 (notes  (plist-get plst :notes))
	 (links  (plist-get plst :links))
	 (status (if (string= "completed" (plist-get plst :status))
		     "DONE"
		   "TODO"))
	 (completed (plist-get plst :completed)))
    (concat (format "* %s %s\n" status title)
	    (when due
	      (format "  DEADLINE: <%s>\n" (org-gtasks-format-iso2org due)))
	    (when completed
	      (format "  CLOSED: [%s]\n" (org-gtasks-format-iso2org completed)))
	    "  :PROPERTIES:\n"
	    "  :ID: " id "\n"
	    "  :END:\n"
	    (when notes (concat notes "\n"))
            (when links
              (concat "\n  :links:\n"
                      (mapconcat (lambda (link)
				   (let* ((type (plist-get link :type))
					  (org-link (plist-get link :link))
					  (desc (plist-get link :description))
					  (str (org-make-link-string org-link desc)))
				     (format "  - %s: %s\n" type str)))
				 links "")
                      "  :end:\n")))))

(defun org-gtasks-write-to-org (account tasklist)
  (let* ((default-directory (org-gtasks-directory account))
	 (file (tasklist-file tasklist))
	 (title (tasklist-title tasklist))
	 (header (format "#+FILETAGS: :%s:\n" title))
	 (tasks (tasklist-tasks tasklist))
	 (save-silently t))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (insert header)
      (dolist (task tasks)
	(insert (org-gtasks-task task)))
      (goto-char (point-min))
      (when tasks
        (org-sort-entries nil ?o)
        (org-set-startup-visibility))
      (save-buffer))))

(defun org-gtasks-tasks-cb (account tasklist data)
  (setf (tasklist-tasks tasklist) (array-to-list (plist-get data :items)))
  (org-gtasks-write-to-org account tasklist))

(defun org-gtasks-fetch-tasks (account tasklist)
  (let ((title (tasklist-title tasklist))
	(id (tasklist-id tasklist)))
    (message "Pulling %s ..." (propertize title 'face 'success))
    (org-gtasks-request-fetch account
			      (format "%s/lists/%s/tasks" org-gtasks-default-url id)
			      (apply-partially #'org-gtasks-tasks-cb account tasklist))))

(defun org-gtasks-tasklists-cb (account data)
  (when (plist-member data :items)
    (dolist (item (array-to-list (plist-get data :items)))
      (let* ((title (plist-get item :title))
	     (file (format "%s.org" title))
	     (id (plist-get item :id))
	     (tasklist (make-tasklist :title title :file file :id id)))
	(push tasklist (org-gtasks-tasklists account))
	(org-gtasks-fetch-tasks account tasklist)))))

(defun org-gtasks-fetch-tasklists (account)
  (setf (org-gtasks-tasklists account) nil)
  (org-gtasks-request-fetch account
			    (concat org-gtasks-default-url "/users/@me/lists")
			    (apply-partially #'org-gtasks-tasklists-cb account)))

(defun org-gtasks-pull-current ()
  (interactive)
  (if-let* ((file (buffer-file-name))
            (title (file-name-base file))
            (dir (file-name-directory file))
            (account (org-gtasks-find-account-by-dir dir))
            (tasklists (org-gtasks-tasklists account))
            (tasklist (org-gtasks-find-tasklist tasklists title)))
      (org-gtasks-fetch-tasks account tasklist)
    (message "Cannot pull current tasklist")))

(defun org-gtasks-pull (account &optional listname)
  (let* ((tasklists (org-gtasks-tasklists account))
	 (titles (mapcar 'tasklist-title tasklists))
	 (collection (append (list "ALL") titles))
	 (target (if (null listname)
                     (completing-read "Pull: " collection)
                   listname)))
    (if (string= target "ALL")
	(org-gtasks-fetch-tasklists account)
      (when-let ((tasklist (org-gtasks-find-tasklist tasklists target)))
	(org-gtasks-fetch-tasks account tasklist)))))

;; push
(defun org-gtasks-format-iso2org (str)
  (format-time-string "%Y-%m-%d %a %H:%M" (date-to-time str)))

(defun org-gtasks-format-org2iso (year mon day hour min)
  (let ((seconds (time-to-seconds (encode-time 0 (if min min 0) (if hour hour 0)
					       day mon year))))
    (concat (format-time-string "%Y-%m-%dT%H:%M" (seconds-to-time seconds))
	    ":00Z")))

(defun org-gtasks-find-type (tasks id)
  (if (cl-find-if (lambda (task) (string= (plist-get task :id) id)) tasks)
      "PATCH"
    "POST"))

(defun org-gtasks-task-completed (tasks id)
  (cl-find-if (lambda (task)
		(when (string= (plist-get task :id) id)
		  (plist-member task :completed)))
	      tasks))

(defun org-gtasks-extract-notes (begin end)
  (if (and begin end)
      (save-excursion
	(goto-char begin)
	(when (re-search-forward org-property-drawer-re end t)
	  (forward-char))
	(if (= (point) end)
	    ""
	  (buffer-substring-no-properties (point) (- end 1))))
    ""))

(defun org-gtasks-clean-notes (notes)
  ;; Strip :links: drawer - the links property should not be
  ;; inserted into the notes field. Currently links is a
  ;; read-only field:
  ;; https://developers.google.com/tasks/v1/reference/tasks#resource
  (replace-regexp-in-string org-gtasks-links-drawer-re "" notes))

(defun org-gtasks-build-tasks-data (tasklist)
  (let ((title (tasklist-title tasklist))
	(file (tasklist-file tasklist))
	(tasklist-id (tasklist-id tasklist))
	(tasks (tasklist-tasks tasklist))
	initial-data tasklist-data)
    ;; init data with all tasks id with type DELETE
    (dolist (task tasks)
      (let* ((task-id (plist-get task :id))
	     (url (format "%s/lists/%s/tasks/%s" org-gtasks-default-url
			  tasklist-id task-id)))
	(push (cons (plist-get task :id) (list url "DELETE" nil))
	      initial-data)))
    ;; loop over org elements
    (with-current-buffer (find-file-noselect file)
      (org-element-map (org-element-parse-buffer) 'headline
	(lambda (hl)
	  (when (= (org-element-property :level hl) 1)
	    (let* ((id (org-element-property :ID hl))
		   (type (org-gtasks-find-type tasks id))
		   (url (concat (format "%s/lists/%s/tasks" org-gtasks-default-url
					tasklist-id)
				(if (string= type "PATCH") (concat "/" id))))
		   (title (substring-no-properties (org-element-interpret-data
						    (org-element-property :title hl))))
		   (org2iso (lambda (e)
			      (org-gtasks-format-org2iso
			       (plist-get e :year-start)
			       (plist-get e :month-start)
			       (plist-get e :day-start)
			       (plist-get e :hour-start)
			       (plist-get e :minute-start))))
		   (deadline (org-element-property :deadline hl))
		   (due (when deadline (funcall org2iso (cadr deadline))))
		   (closed (org-element-property :closed hl))
		   (completed (when closed (funcall org2iso (cadr closed))))
		   (status (if (string= (org-element-property :todo-type hl) "done")
			       "completed"
			     "needsAction"))
		   (notes (org-gtasks-extract-notes (plist-get (cadr hl) :contents-begin)
						    (plist-get (cadr hl) :contents-end)))
                   (notes (org-gtasks-clean-notes notes))
		   (data-list `(("title" . ,title)
				("notes" . ,notes)
				("status" . ,status))))
	      (when (and completed (not (org-gtasks-task-completed tasks id)))
		(add-to-list 'data-list `("completed" . ,completed)))
	      (add-to-list 'data-list `("due" . ,due))
	      (if (assoc id initial-data)
		  (setcdr (assoc id initial-data) (list url type data-list))
		(push (list url type data-list) tasklist-data)))))))
    (cons title (append tasklist-data (mapcar #'cdr initial-data)))))

(defun org-gtasks-request-push (account url cb type data)
  (org-gtasks-request-async account url cb
			    :type    type
			    :headers '(("Content-Type" . "application/json"))
			    :data    (if data (json-encode data))
			    :params  `(("access_token" . ,(org-gtasks-access-token account))
				       ("grant_type"   . "authorization_code"))))

(defun org-gtasks-push-tasks (account tasks tasklists done &optional data)
  (if tasks
      (pcase-let ((`(,url ,type ,data) (pop tasks)))
	(org-gtasks-request-push account url
				 (apply-partially #'org-gtasks-push-tasks
						  account tasks tasklists done)
				 type data))
    (org-gtasks-push-tasklists account tasklists done)))

(defun org-gtasks-push-tasklists (account tasklists done)
  (if tasklists
      (let* ((default-directory (org-gtasks-directory account))
	     (tasklist (pop tasklists))
	     (data (org-gtasks-build-tasks-data tasklist)))
	(message "Pushing %s ..." (propertize (car data) 'face 'success))
	(org-gtasks-push-tasks account (cdr data) tasklists done))
    (message "Push done")
    (funcall done)))

(defun org-gtasks-push-current ()
  (interactive)
  (if-let* ((file (buffer-file-name))
            (title (file-name-base file))
            (dir (file-name-directory file))
            (account (org-gtasks-find-account-by-dir dir))
            (tasklists (org-gtasks-tasklists account))
            (tasklist (org-gtasks-find-tasklist tasklists title)))
      (org-gtasks-push-tasklists account (list tasklist)
				 (apply-partially #'org-gtasks-fetch-tasks
						  account tasklist))
    (message "Cannot push current tasklist")))

(defun org-gtasks-push (account &optional listname)
  (let* ((tasklists (org-gtasks-tasklists account))
	 (collection (mapcar 'tasklist-title tasklists))
	 (collection (if (> (length collection) 1)
                         (append (list "ALL") collection)
                       collection))
	 (target (if (null listname)
                     (completing-read "Push: " collection)
                   listname)))
    (if (string= target "ALL")
	(org-gtasks-push-tasklists account tasklists
				   (apply-partially #'org-gtasks-fetch-tasklists
						    account))
      (when-let ((tasklist (org-gtasks-find-tasklist tasklists target)))
	(org-gtasks-push-tasklists account (list tasklist)
				   (apply-partially #'org-gtasks-fetch-tasks
						    account tasklist))))))

;; add
(defun org-gtasks-build-add-data (added)
  (let (data)
    (dolist (file added)
      (push (list (concat org-gtasks-default-url "/users/@me/lists")
		  "POST"
		  `(("title" . ,(file-name-base file))))
	    data))
    data))

(defun org-gtasks-push-tasklists-cb (account add-data tasklist &optional data)
  (org-gtasks-fetch-tasks account tasklist)
  (org-gtasks-add-tasklists account add-data))

(defun org-gtasks-add-tasklists-cb (account add-data &optional data)
  (when-let* ((title (plist-get data :title))
	      (file (format "%s.org" title))
	      (id (plist-get data :id))
	      (tasklist (make-tasklist :title title :file file :id id)))
    (setf (org-gtasks-tasklists account)
	  (append (list tasklist) (org-gtasks-tasklists account)))
    (org-gtasks-push-tasklists account (list tasklist)
			       (apply-partially #'org-gtasks-push-tasklists-cb
						account add-data tasklist))))

(defun org-gtasks-add-tasklists (account add-data &optional data)
  (if add-data
      (pcase-let ((`(,url ,type ,data) (pop add-data)))
	(org-gtasks-request-push account url
				 (apply-partially #'org-gtasks-add-tasklists-cb
						  account add-data)
				 type data))
    (message "Add done")))

(defun org-gtasks-add (account)
  (let* ((local-files (org-gtasks-local-files account))
	 (tasklists (org-gtasks-tasklists account))
	 (files (mapcar 'tasklist-file tasklists))
	 (added (cl-set-difference local-files files :test #'string=))
	 (collection (if (> (length added) 1)
			 (append (list "ALL") added)
		       added))
	 (target (completing-read "Add: " collection))
	 (added (if (string= target "ALL") added (list target)))
	 (add-data (org-gtasks-build-add-data added)))
    (org-gtasks-add-tasklists account add-data)))

;; remove
(defun org-gtasks-build-remove-data (tasklists removed)
  (let (data)
    (dolist (file removed)
      (when-let* ((tasklist (cl-find-if (lambda (tl) (string= (tasklist-file tl) file))
					tasklists))
		  (id (tasklist-id tasklist))
		  (url (format "%s/users/@me/lists/%s" org-gtasks-default-url id)))
	(push (list tasklist url "DELETE") data)))
    data))

(defun org-gtasks-remove-tasklists (account remove-data &optional data)
  (if remove-data
      (pcase-let ((`(,tasklist ,url ,type) (pop remove-data)))
	(setf (org-gtasks-tasklists account)
	      (remove tasklist (org-gtasks-tasklists account)))
	(org-gtasks-request-push account url
				 (apply-partially #'org-gtasks-remove-tasklists
						  account remove-data)
				 type nil))
    (message "Remove done")))

(defun org-gtasks-remove (account)
  (let* ((local-files (org-gtasks-local-files account))
	 (tasklists (org-gtasks-tasklists account))
	 (files (mapcar 'tasklist-file tasklists))
	 (removed (cl-set-difference files local-files :test #'string=))
	 (collection (if (> (length removed) 1)
			 (append (list "ALL") removed)
		       removed))
	 (target (completing-read "Remove: " collection))
	 (removed (if (string= target "ALL") removed (list target)))
	 (remove-data (org-gtasks-build-remove-data tasklists removed)))
    (org-gtasks-remove-tasklists account remove-data)))

;;;###autoload
(defun org-gtasks ()
  (interactive)
  (when-let* ((collection (mapcar 'org-gtasks-name org-gtasks-accounts))
	      (name (completing-read "Select Account: " collection))
	      (account (org-gtasks-find-account-by-name name))
	      (action (completing-read (format "Action (%s): " name)
				       (mapcar 'car org-gtasks-actions)))
	      (func (assoc-default action org-gtasks-actions)))
    (org-gtasks-check-token account)
    (funcall func account)))

(defun org-gtasks-account-eq (a1 a2)
  (when (and (org-gtasks-p a1) (org-gtasks-p a2))
    (or (string= (org-gtasks-name a1) (org-gtasks-name a2))
	(string= (org-gtasks-directory a1) (org-gtasks-directory a2)))))

(defun org-gtasks-register-account (&rest plist)
  (let* ((name (plist-get plist :name))
	 (directory (file-name-as-directory
		     (expand-file-name (plist-get plist :directory))))
	 (client-id (plist-get plist :client-id))
	 (client-secret (plist-get plist :client-secret))
	 (account (make-org-gtasks :name name
				   :directory directory
				   :client-id client-id
				   :client-secret client-secret)))
    (unless (file-directory-p directory)
      (make-directory directory))
    (add-to-list 'org-gtasks-accounts account t 'org-gtasks-account-eq)))

(provide 'org-gtasks)

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 8
;; End:
