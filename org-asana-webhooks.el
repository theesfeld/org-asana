;;; org-asana-webhooks.el --- Webhook support for org-asana -*- lexical-binding: t; -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides webhook support for real-time updates.
;; Requires a public HTTPS endpoint to receive webhooks.

;;; Code:

(require 'org-asana)
(require 'simple-httpd)  ; For local webhook server
(require 'gnutls)

;;; Custom Variables

(defcustom org-asana-webhook-endpoint nil
  "Public HTTPS endpoint URL for receiving webhooks.
Must be accessible from the internet."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-webhook-server-port 8080
  "Port for local webhook server (development only)."
  :type 'integer
  :group 'org-asana)

(defcustom org-asana-webhook-filters
  '(((resource_type . "task")
     (action . "changed")
     (fields . ("completed" "name" "due_on" "assignee"))))
  "Webhook filter definitions."
  :type 'alist
  :group 'org-asana)

;;; Variables

(defvar org-asana--webhook-secrets (make-hash-table :test 'equal)
  "Hash table mapping webhook GIDs to their secrets.")

(defvar org-asana--registered-webhooks nil
  "List of registered webhook GIDs.")

(defvar org-asana--webhook-server nil
  "Local webhook server process.")

;;; API Functions

(defun org-asana--create-webhook (resource-gid target-url filters)
  "Create webhook for RESOURCE-GID with TARGET-URL and FILTERS."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (org-asana--get-token)))
           ("Content-Type" . "application/x-www-form-urlencoded")))
        (params (org-asana--build-webhook-params resource-gid target-url filters)))
    (with-current-buffer (url-retrieve-synchronously
                         (concat org-asana-api-base-url "/webhooks") t t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-array-type 'list))
        (json-read)))))

(defun org-asana--build-webhook-params (resource-gid target-url filters)
  "Build URL-encoded params for webhook creation."
  (let ((params (format "resource=%s&target=%s"
                       (url-hexify-string resource-gid)
                       (url-hexify-string target-url))))
    (cl-loop for i from 0
             for filter in filters
             do (setq params
                     (concat params
                             (format "&filters[%d][resource_type]=%s" i
                                    (alist-get 'resource_type filter))
                             (format "&filters[%d][action]=%s" i
                                    (alist-get 'action filter))
                             (when (alist-get 'fields filter)
                               (mapconcat
                                (lambda (field)
                                  (format "&filters[%d][fields][]=%s" i field))
                                (alist-get 'fields filter) "")))))
    params))

(defun org-asana--list-webhooks (&optional resource-gid)
  "List webhooks, optionally filtered by RESOURCE-GID."
  (let ((endpoint (if resource-gid
                     (format "/webhooks?resource=%s" resource-gid)
                   "/webhooks")))
    (alist-get 'data (org-asana--make-request "GET" endpoint))))

(defun org-asana--delete-webhook (webhook-gid)
  "Delete webhook with WEBHOOK-GID."
  (org-asana--make-request "DELETE" (format "/webhooks/%s" webhook-gid)))

;;; Handshake Handler

(defun org-asana--handle-webhook-handshake (request)
  "Handle webhook handshake REQUEST."
  (let ((secret (gethash "X-Hook-Secret" (plist-get request :headers))))
    (when secret
      ;; Store secret for future validation
      (puthash (plist-get request :webhook-gid) secret org-asana--webhook-secrets)
      ;; Return secret in response header
      `(:status 200
        :headers (("X-Hook-Secret" . ,secret)
                 ("Content-Type" . "text/plain"))
        :body "OK"))))

;;; Signature Validation

(defun org-asana--validate-webhook-signature (webhook-gid headers body)
  "Validate webhook signature for WEBHOOK-GID with HEADERS and BODY."
  (let ((signature (gethash "X-Hook-Signature" headers))
        (secret (gethash webhook-gid org-asana--webhook-secrets)))
    (when (and signature secret)
      (let ((expected-sig (org-asana--compute-hmac-sha256 secret body)))
        (string= signature expected-sig)))))

(defun org-asana--compute-hmac-sha256 (secret message)
  "Compute HMAC-SHA256 of MESSAGE with SECRET."
  (with-temp-buffer
    (insert message)
    (secure-hash 'sha256 (current-buffer) nil nil secret)))

;;; Event Processing

(defun org-asana--process-webhook-event (event)
  "Process a single webhook EVENT."
  (let ((action (alist-get 'action event))
        (resource (alist-get 'resource event))
        (resource-type (alist-get 'resource_type resource))
        (resource-gid (alist-get 'gid resource)))
    (cond
     ((equal resource-type "task")
      (org-asana--handle-task-event action resource-gid))
     ((equal resource-type "project")
      (org-asana--handle-project-event action resource-gid))
     (t (message "Unhandled webhook event type: %s" resource-type)))))

(defun org-asana--handle-task-event (action task-gid)
  "Handle task event with ACTION for TASK-GID."
  (cond
   ((equal action "changed")
    (org-asana--refresh-task task-gid))
   ((equal action "deleted")
    (org-asana--remove-task-from-org task-gid))
   ((equal action "added")
    (org-asana--add-new-task task-gid))
   (t (message "Unhandled task action: %s" action))))

(defun org-asana--handle-project-event (action project-gid)
  "Handle project event with ACTION for PROJECT-GID."
  (message "Project %s event: %s" project-gid action)
  ;; Trigger full refresh for project changes
  (org-asana-sync))

;;; Task Update Functions

(defun org-asana--refresh-task (task-gid)
  "Refresh single TASK-GID from API."
  (let ((task (org-asana--fetch-single-task task-gid)))
    (when task
      (org-asana--update-task-in-org task))))

(defun org-asana--fetch-single-task (task-gid)
  "Fetch single task by TASK-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/tasks/%s" task-gid))))

(defun org-asana--update-task-in-org (task)
  "Update TASK in org file."
  (with-current-buffer (find-file-noselect org-asana-org-file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format ":ASANA-TASK-GID: %s" (alist-get 'gid task)) nil t)
        (org-back-to-heading t)
        ;; Update TODO state
        (let ((completed (alist-get 'completed task)))
          (org-todo (if completed "DONE" "TODO")))
        ;; Update title
        (org-edit-headline (alist-get 'name task))
        ;; Update properties
        (org-asana--update-task-properties task)))))

(defun org-asana--remove-task-from-org (task-gid)
  "Remove TASK-GID from org file."
  (with-current-buffer (find-file-noselect org-asana-org-file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format ":ASANA-TASK-GID: %s" task-gid) nil t)
        (org-back-to-heading t)
        (org-cut-subtree)
        (message "Task %s removed from org file" task-gid)))))

(defun org-asana--add-new-task (task-gid)
  "Add new TASK-GID to org file."
  (message "New task detected: %s. Running full sync..." task-gid)
  (org-asana-sync))

;;; Server Functions

(defun org-asana--start-webhook-server ()
  "Start local webhook server for development."
  (interactive)
  (unless org-asana-webhook-endpoint
    (error "Set org-asana-webhook-endpoint first"))
  (setq httpd-port org-asana-webhook-server-port)
  (httpd-start)
  (defservlet* org-asana-webhook text/plain (path headers query)
    (org-asana--handle-webhook-request
     (list :path path :headers headers :body (buffer-string))))
  (message "Webhook server started on port %d" org-asana-webhook-server-port))

(defun org-asana--stop-webhook-server ()
  "Stop local webhook server."
  (interactive)
  (httpd-stop)
  (message "Webhook server stopped"))

(defun org-asana--handle-webhook-request (request)
  "Handle incoming webhook REQUEST."
  (let ((headers (plist-get request :headers))
        (body (plist-get request :body)))
    ;; Check for handshake
    (if (gethash "X-Hook-Secret" headers)
        (org-asana--handle-webhook-handshake request)
      ;; Regular webhook event
      (if (org-asana--validate-webhook-signature
           (plist-get request :webhook-gid) headers body)
          (progn
            (let ((events (alist-get 'events (json-read-from-string body))))
              (dolist (event events)
                (org-asana--process-webhook-event event)))
            '(:status 200 :body "OK"))
        '(:status 401 :body "Invalid signature")))))

;;; Interactive Functions

(defun org-asana-register-webhook (resource-gid)
  "Register webhook for RESOURCE-GID."
  (interactive "sResource GID (project/team): ")
  (unless org-asana-webhook-endpoint
    (error "Configure org-asana-webhook-endpoint first"))
  (let ((response (org-asana--create-webhook
                  resource-gid
                  org-asana-webhook-endpoint
                  org-asana-webhook-filters)))
    (if (alist-get 'errors response)
        (error "Failed to create webhook: %s"
               (alist-get 'message (car (alist-get 'errors response))))
      (let ((webhook (alist-get 'data response)))
        (push (alist-get 'gid webhook) org-asana--registered-webhooks)
        (message "Webhook registered: %s" (alist-get 'gid webhook))))))

(defun org-asana-list-webhooks ()
  "List all registered webhooks."
  (interactive)
  (let ((webhooks (org-asana--list-webhooks))
        (buffer (get-buffer-create "*Asana Webhooks*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Asana Webhooks\n")
      (insert "==============\n\n")
      (dolist (webhook webhooks)
        (insert (format "GID: %s\n" (alist-get 'gid webhook)))
        (insert (format "Resource: %s\n" (alist-get 'gid (alist-get 'resource webhook))))
        (insert (format "Target: %s\n" (alist-get 'target webhook)))
        (insert (format "Active: %s\n\n" (alist-get 'active webhook))))
      (goto-char (point-min))
      (view-mode))
    (display-buffer buffer)))

(defun org-asana-delete-webhook (webhook-gid)
  "Delete webhook with WEBHOOK-GID."
  (interactive "sWebhook GID: ")
  (org-asana--delete-webhook webhook-gid)
  (setq org-asana--registered-webhooks
        (delete webhook-gid org-asana--registered-webhooks))
  (remhash webhook-gid org-asana--webhook-secrets)
  (message "Webhook %s deleted" webhook-gid))

(defun org-asana-delete-all-webhooks ()
  "Delete all registered webhooks."
  (interactive)
  (when (yes-or-no-p "Delete all webhooks? ")
    (dolist (webhook-gid org-asana--registered-webhooks)
      (org-asana--delete-webhook webhook-gid))
    (setq org-asana--registered-webhooks nil)
    (clrhash org-asana--webhook-secrets)
    (message "All webhooks deleted")))

(provide 'org-asana-webhooks)
;;; org-asana-webhooks.el ends here