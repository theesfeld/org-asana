;;; org-asana.el --- Two-way sync between Org-mode and Asana  -*- lexical-binding: t; -*-

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

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (json "1.5") (url "0"))
;; Keywords: tools, asana, org-mode, productivity
;; URL: https://github.com/username/org-asana

;;; Commentary:

;; This package provides two-way synchronization between Emacs Org-mode
;; and Asana tasks.  It follows a functional programming approach with
;; clear separation between data fetching, transformation, and rendering.

;; Main features:
;; - Fetch tasks from Asana and convert to Org headings
;; - Sync changes back to Asana
;; - Rate limiting protection
;; - Visual task faces for priorities and deadlines
;; - Progress indicators
;; - Org-agenda integration

;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'subr-x)

;;; Custom Variables

(defgroup org-asana nil
  "Options for org-asana."
  :tag "Org Asana"
  :group 'org)

(defcustom org-asana-token nil
  "Personal Access Token for Asana API."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-org-file "~/org/asana.org"
  "Path to the Org file for Asana tasks."
  :type 'file
  :group 'org-asana)

(defcustom org-asana-fetch-metadata t
  "Whether to fetch comments and attachments for tasks."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-show-progress-indicators t
  "Whether to show [done/total] progress indicators on headings."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-rate-limit-delay 0.4
  "Delay in seconds between API requests to respect rate limits."
  :type 'number
  :group 'org-asana)

(defcustom org-asana-debug nil
  "Enable debug messages during sync operations."
  :type 'boolean
  :group 'org-asana)

;;; Constants

(defconst org-asana-api-base-url "https://app.asana.com/api/1.0"
  "Base URL for Asana API requests.")

(defconst org-asana-rate-limit-max 150
  "Maximum API requests per minute.")

(defvar org-asana--rate-limit-remaining org-asana-rate-limit-max
  "Remaining API calls in current rate limit window.")

(defvar org-asana--rate-limit-reset nil
  "Time when rate limit resets.")

;;; API Request Functions

(defun org-asana--check-rate-limit ()
  "Check and enforce API rate limits."
  (when (and org-asana--rate-limit-reset
             (< org-asana--rate-limit-remaining 10)
             (> (float-time org-asana--rate-limit-reset) (float-time)))
    (let ((wait-time (- (float-time org-asana--rate-limit-reset) (float-time))))
      (message "Rate limit low. Waiting %.1f seconds..." wait-time)
      (sleep-for wait-time))))

(defun org-asana--make-request (method endpoint &optional data)
  "Make an API request to METHOD ENDPOINT with optional DATA."
  (unless org-asana-token
    (error "No Asana token configured. Set `org-asana-token'"))
  (org-asana--check-rate-limit)
  (let* ((url (concat org-asana-api-base-url endpoint))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " org-asana-token))
            ("Content-Type" . "application/json")))
         (url-request-data (when data (json-encode data))))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((response (json-read)))
        (kill-buffer)
        (sleep-for org-asana-rate-limit-delay)
        response))))

(defun org-asana--fetch-paginated (endpoint)
  "Fetch all pages of data from ENDPOINT."
  (let ((all-data '())
        (next-page nil))
    (catch 'done
      (while t
        (let* ((url (or next-page endpoint))
               (response (org-asana--make-request "GET" url)))
          (setq all-data (append all-data (alist-get 'data response)))
          (setq next-page (alist-get 'next_page response))
          (unless next-page (throw 'done all-data)))))))

(defun org-asana--fetch-workspace-info ()
  "Fetch current user and workspace information."
  (let ((response (org-asana--make-request "GET" "/users/me")))
    (let* ((user-data (alist-get 'data response))
           (workspaces (alist-get 'workspaces user-data)))
      (list (alist-get 'gid user-data)
            (alist-get 'gid (car workspaces))))))

(defun org-asana--fetch-all-tasks ()
  "Fetch all incomplete tasks assigned to current user."
  (let* ((workspace-info (org-asana--fetch-workspace-info))
         (user-gid (car workspace-info))
         (workspace-gid (cadr workspace-info))
         (opt-fields (concat "gid,name,notes,completed,due_on,due_at,"
                            "created_at,modified_at,created_by.name,"
                            "memberships.project.gid,memberships.project.name,"
                            "memberships.section.gid,memberships.section.name"))
         (endpoint (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s"
                          workspace-gid user-gid opt-fields)))
    (org-asana--fetch-paginated endpoint)))

(defun org-asana--fetch-task-metadata (task-id)
  "Fetch stories and attachments for TASK-ID."
  (when org-asana-fetch-metadata
    (let ((stories (org-asana--make-request 
                   "GET" 
                   (format "/tasks/%s/stories" task-id)))
          (attachments (org-asana--make-request
                       "GET"
                       (format "/tasks/%s/attachments" task-id))))
      (cons (alist-get 'data stories)
            (alist-get 'data attachments)))))

(provide 'org-asana)
;;; org-asana.el ends here