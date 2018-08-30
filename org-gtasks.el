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

(defcustom org-gtasks-dir
  (concat user-emacs-directory "org-gtasks/")
  "File in which to save token."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-token-file
  (expand-file-name ".org-gtasks-token" org-gtasks-dir)
  "File in which to save token."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-client-id nil
  "Client ID for OAuth."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-client-secret nil
  "Google tasks secret key for OAuth."
  :group 'org-gtasks
  :type 'string)

(defcustom org-gtasks-file nil
  "Org file where we store Google tasks"
  :group 'org-gtasks
  :type 'string)

(defvar org-gtasks-token-plist nil
  "token plist.")

(defconst org-gtasks-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gtasks-token-url "https://www.googleapis.com/oauth2/v3/token"
  "Google OAuth2 server URL.")

(defconst org-gtasks-resource-url "https://www.googleapis.com/auth/tasks"
  "URL used to request access to tasks resources.")

(defconst org-gtasks-key-url (concat "?key=" org-gtasks-client-secret))

(defconst org-gtasks-events-url "https://www.googleapis.com/tasks/v1/lists/@default/tasks")

(defun org-gtasks--json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))

(defun org-gtasks--save-sexp (data file)
  (if (file-directory-p org-gtasks-dir)
      (if (file-exists-p file)
          (if  (plist-get (read (buffer-string)) :token )
              (with-temp-file file
                (pp (plist-put (read (buffer-string)) :token data) (current-buffer)))
            (with-temp-file file
              (pp `(:token ,data :elem nil) (current-buffer))))
        (progn
          (find-file-noselect file)
          (with-temp-file file
            (pp `(:token ,data :elem nil) (current-buffer)))))
    (progn
      (make-directory org-gtasks-dir)
      (org-gtasks--save-sexp data file))))

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
   :parser 'org-gtasks--json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (when data
                 (setq org-gtasks-token-plist data)
                 (org-gtasks--save-sexp data org-gtasks-token-file))))
   :error
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Got error: %S" error-thrown)))))

(defun org-gtasks--get-refresh-token ()
  (if org-gtasks-token-plist
      (plist-get org-gtasks-token-plist :refresh_token)
    (progn
      (if (file-exists-p org-gtasks-token-file)
          (progn
            (with-temp-buffer (insert-file-contents org-gtasks-token-file)
                              (plist-get (plist-get (read (buffer-string)) :token) :refresh_token)))
        (message "%s doesn't exist" org-gtasks-token-file)))))

(defun org-gtasks-refresh-token ()
  "Refresh OAuth access at TOKEN-URL."
  (deferred:$
    (request-deferred
     org-gtasks-token-url
     :type "POST"
     :data `(("client_id" . ,org-gtasks-client-id)
             ("client_secret" . ,org-gtasks-client-secret)
             ("refresh_token" . ,(org-gtasks--get-refresh-token))
             ("grant_type" . "refresh_token"))
     :parser 'org-gtasks--json-read
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Got error: %S" error-thrown))))
    (deferred:nextc it
      (lambda (response)
        (let ((temp (request-response-data response)))
          (plist-put org-gtasks-token-plist
                     :access_token
                     (plist-get temp :access_token))
          (org-gtasks--save-sexp org-gtasks-token-plist org-gtasks-token-file)
          org-gtasks-token-plist)))
    (deferred:nextc it
      (lambda (token)
        (org-gtasks-sync)))))

(defun org-gtasks--ensure-token ()
  (cond
   (org-gtasks-token-plist t)
   ((and (file-exists-p org-gtasks-token-file)
         (ignore-errors
           (setq org-gtasks-token-plist
                 (with-temp-buffer
                   (insert-file-contents org-gtasks-token-file)
                   (plist-get (read (current-buffer)) :token))))) t)
   (t (org-gtasks-request-token))))

(defun org-gtasks--get-access-token ()
  (if org-gtasks-token-plist
      (plist-get org-gtasks-token-plist :access_token)
    (progn
      (if (file-exists-p org-gtasks-token-file)
          (progn
            (with-temp-buffer (insert-file-contents org-gtasks-token-file)
                              (plist-get (plist-get (read (buffer-string)) :token) :access_token)))
        (message "No token file found")))))

(defun org-gtasks--cons-list (plst)
  (let* ((title  (plist-get plst :title))
	 (notes  (plist-get plst :notes))
	 (status (if (string= "completed" (plist-get plst :status))
		     "DONE"
		   "TODO")))
    (concat (format "* %s %s\n" status title)
	    (when notes) notes (when notes "\n"))))

(defun org-gtasks-sync ()
  (interactive)
  (org-gtasks--ensure-token)
  (deferred:$
    (request-deferred
     org-gtasks-events-url
     :type "GET"
     :params `(("access_token" . ,(org-gtasks--get-access-token))
               ("key" . ,org-gtasks-client-secret)
               ("singleEvents" . "True")
	       ("orderBy" . "startTime")
               ("grant_type" . "authorization_code"))
     :parser 'org-gtasks--json-read
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
              (lambda() (org-gtasks-refresh-token))))
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
	    (with-current-buffer (find-file-noselect org-gtasks-file)
              (erase-buffer)
              (let ((items (plist-get (request-response-data response) :items )))
                (insert
                 (mapconcat 'identity
			    (mapcar (lambda (lst)
                                      (org-gtasks--cons-list lst))
				    items) "")))
              (org-set-startup-visibility)
              (save-buffer)))))))))

(provide 'org-gtasks)
