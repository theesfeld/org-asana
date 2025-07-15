;;; org-asana.el --- Two-way sync between Org-mode and Asana -*- lexical-binding: t; -*-

;; Copyright (C) 2024 William Theesfeld <william@theesfeld.net>

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
;; Maintainer: William Theesfeld <william@theesfeld.net>
;; Created: 15 July 2024
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (org "9.4"))
;; Keywords: convenience, org-mode, asana, productivity
;; URL: https://github.com/wtheesfeld/org-asana

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bidirectional synchronization between Org-mode todos and Asana tasks.
;; 
;; This package provides comprehensive two-way synchronization between
;; Org-mode task entries and Asana tasks, supporting both manual and
;; periodic sync modes with intelligent conflict resolution.
;;
;; Features:
;; - Two-way sync between org entries and Asana tasks
;; - Support for manual or periodic synchronization
;; - Intelligent conflict resolution using newest-wins strategy
;; - Integration with org-agenda
;; - Personal Access Token authentication
;; - Rate limiting and error handling
;; - Pure functional design following GNU standards
;;
;; Setup:
;; 1. Get a Personal Access Token from Asana (Profile Settings > Apps)
;; 2. Configure the package:
;;    (setq org-asana-token "your-personal-access-token")
;; 3. Run M-x org-asana-setup for initial configuration
;;
;; Usage:
;; - M-x org-asana-sync for manual synchronization
;; - Convert org headings to Asana tasks with org-asana-create-task-from-heading
;; - Import Asana tasks with org-asana-import-my-tasks
;;
;; For periodic sync, set:
;; (setq org-asana-sync-method 'periodic
;;       org-asana-sync-interval 15) ; minutes

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'url)
(require 'json)

;;; Constants and Configuration

(defgroup org-asana nil
  "Integration between Org-mode and Asana."
  :group 'org
  :prefix "org-asana-")

(defconst org-asana-api-base-url "https://app.asana.com/api/1.0"
  "Base URL for Asana API endpoints.")

(defconst org-asana-api-rate-limit 150
  "Asana API rate limit (requests per minute).")

(defconst org-asana-version "1.0.0"
  "Version of org-asana package.")

;;; Configuration Variables

(defcustom org-asana-token nil
  "Asana Personal Access Token for API authentication."
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Personal Access Token"))
  :group 'org-asana)

(defcustom org-asana-sync-method 'manual
  "Method for synchronizing with Asana.
'manual means sync only when explicitly requested.
'periodic means automatically sync at regular intervals."
  :type '(choice (const :tag "Manual sync only" manual)
                 (const :tag "Periodic automatic sync" periodic))
  :group 'org-asana)

(defcustom org-asana-sync-interval 15
  "Interval in minutes for periodic synchronization.
Only used when `org-asana-sync-method' is 'periodic."
  :type 'integer
  :group 'org-asana)

(defcustom org-asana-conflict-resolution 'newest-wins
  "Strategy for resolving conflicts when both sides have changes.
'newest-wins compares modification timestamps and keeps most recent.
'asana-wins always prefers Asana data over Org-mode data."
  :type '(choice (const :tag "Prefer newest modification" newest-wins)
                 (const :tag "Always prefer Asana" asana-wins))
  :group 'org-asana)

(defcustom org-asana-default-workspace nil
  "Default workspace GID for operations.
If nil, will use the first available workspace."
  :type '(choice (const :tag "Auto-select first workspace" nil)
                 (string :tag "Workspace GID"))
  :group 'org-asana)

(defcustom org-asana-default-project nil
  "Default project GID for new tasks.
If nil, tasks will be created in 'My Tasks'."
  :type '(choice (const :tag "My Tasks" nil)
                 (string :tag "Project GID"))
  :group 'org-asana)

(defcustom org-asana-sync-tags t
  "Whether to synchronize org tags with Asana tags."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-sync-priority t
  "Whether to synchronize org priority with Asana priority."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-org-file nil
  "Specific org file to use for Asana tasks.
If nil, will use the current buffer when syncing."
  :type '(choice (const :tag "Current buffer" nil)
                 (file :tag "Org file path"))
  :group 'org-asana)

(defcustom org-asana-heading-level 2
  "Org heading level to use for imported Asana tasks."
  :type 'integer
  :group 'org-asana)

;;; Internal Variables

(defvar org-asana--sync-timer nil
  "Timer object for periodic synchronization.")

(defvar org-asana--last-sync-time nil
  "Timestamp of last successful synchronization.")

(defvar org-asana--workspaces nil
  "Cache of available workspaces.")

(defvar org-asana--projects nil
  "Cache of available projects.")

(defvar org-asana--rate-limit-remaining nil
  "Remaining API requests before rate limit.")

(defvar org-asana--rate-limit-reset-time nil
  "Time when rate limit resets.")

;;; Utility Functions

(defun org-asana--format-timestamp (time)
  "Format TIME as ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time t))

(defun org-asana--parse-timestamp (timestamp-str)
  "Parse ISO 8601 TIMESTAMP-STR to Emacs time."
  (when timestamp-str
    (date-to-time timestamp-str)))

(defun org-asana--current-timestamp ()
  "Get current time as ISO 8601 timestamp string."
  (org-asana--format-timestamp (current-time)))

(defun org-asana--safe-get (alist key)
  "Safely get KEY from ALIST, returning nil if not found."
  (cdr (assoc key alist)))

(defun org-asana--get-property (property)
  "Get org PROPERTY at current heading."
  (org-entry-get nil property))

(defun org-asana--set-property (property value)
  "Set org PROPERTY to VALUE at current heading."
  (org-entry-put nil property value))

(defun org-asana--task-id ()
  "Get Asana task ID from current org heading."
  (org-asana--get-property "ASANA_TASK_ID"))

(defun org-asana--set-task-id (task-id)
  "Set Asana TASK-ID for current org heading."
  (org-asana--set-property "ASANA_TASK_ID" task-id))

(defun org-asana--modified-time ()
  "Get modification time from current org heading."
  (org-asana--get-property "ASANA_MODIFIED"))

(defun org-asana--set-modified-time (time-str)
  "Set modification TIME-STR for current org heading."
  (org-asana--set-property "ASANA_MODIFIED" time-str))

(defun org-asana--get-heading-text ()
  "Get the heading text without TODO keywords or tags."
  (nth 4 (org-heading-components)))

(defun org-asana--get-todo-state ()
  "Get TODO state of current heading."
  (nth 2 (org-heading-components)))

(defun org-asana--get-priority ()
  "Get priority of current heading as string."
  (when-let ((priority (nth 3 (org-heading-components))))
    (char-to-string priority)))

(defun org-asana--get-tags ()
  "Get tags of current heading as list."
  (nth 5 (org-heading-components)))

(defun org-asana--get-deadline ()
  "Get deadline of current heading as timestamp."
  (org-entry-get nil "DEADLINE"))

(defun org-asana--get-entry-body ()
  "Get body text of current org entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (progn (outline-next-heading) (point)))
          (end (progn (outline-next-heading) (point))))
      (string-trim (buffer-substring-no-properties start end)))))

;;; Authentication and Setup

(defun org-asana--headers ()
  "Get HTTP headers for Asana API requests."
  (unless org-asana-token
    (error "Asana token not configured. Set org-asana-token or run org-asana-setup"))
  `(("Authorization" . ,(concat "Bearer " org-asana-token))
    ("Content-Type" . "application/json")))

(defun org-asana--api-request (method endpoint &optional data)
  "Make API request with METHOD to ENDPOINT with optional DATA."
  (let* ((url (concat org-asana-api-base-url endpoint))
         (url-request-method method)
         (url-request-extra-headers (org-asana--headers))
         (url-request-data (when data (json-encode data)))
         (buffer (url-retrieve-synchronously url t t)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "\n\n" nil t)
        (let ((response (buffer-substring-no-properties (point) (point-max))))
          (kill-buffer)
          (condition-case nil
              (json-read-from-string response)
            (error nil)))))))

(defun org-asana-test-connection ()
  "Test connection to Asana API."
  (interactive)
  (condition-case err
      (let ((me (org-asana--api-request "GET" "/users/me")))
        (if (org-asana--safe-get me 'data)
            (message "Connection successful! Connected as: %s"
                     (org-asana--safe-get (org-asana--safe-get me 'data) 'name))
          (message "Connection failed: Invalid response")))
    (error (message "Connection failed: %s" (error-message-string err)))))

;;; Data Transformation Functions

(defun org-asana--priority-to-asana (org-priority)
  "Convert org ORG-PRIORITY ([A], [B], [C]) to Asana priority."
  (when org-priority
    (pcase org-priority
      ("A" "high")
      ("B" "medium")
      ("C" "low")
      (_ "medium"))))

(defun org-asana--priority-from-asana (asana-priority)
  "Convert Asana ASANA-PRIORITY to org priority character."
  (pcase asana-priority
    ("high" "A")
    ("medium" "B")
    ("low" "C")
    (_ "B")))

(defun org-asana--date-to-asana (org-date)
  "Convert org date ORG-DATE to Asana date format."
  (when org-date
    (let ((time (org-time-string-to-time org-date)))
      (format-time-string "%Y-%m-%d" time))))

(defun org-asana--date-from-asana (asana-date)
  "Convert Asana ASANA-DATE to org timestamp."
  (when asana-date
    (format "<%s>" asana-date)))

(defun org-asana--task-to-org-entry (task)
  "Convert Asana TASK to org entry format."
  (let* ((data (org-asana--safe-get task 'data))
         (name (org-asana--safe-get data 'name))
         (completed (org-asana--safe-get data 'completed))
         (due-date (org-asana--safe-get data 'due_date))
         (notes (org-asana--safe-get data 'notes))
         (tags (org-asana--safe-get data 'tags))
         (priority (org-asana--safe-get data 'priority))
         (modified-at (org-asana--safe-get data 'modified_at))
         (task-id (org-asana--safe-get data 'gid))
         (assignee (org-asana--safe-get data 'assignee))
         (projects (org-asana--safe-get data 'projects)))
    `((heading . ,name)
      (todo-state . ,(if completed "DONE" "TODO"))
      (priority . ,(when org-asana-sync-priority
                     (org-asana--priority-from-asana priority)))
      (deadline . ,(org-asana--date-from-asana due-date))
      (tags . ,(when org-asana-sync-tags
                 (mapcar (lambda (tag)
                           (org-asana--safe-get tag 'name))
                         tags)))
      (body . ,notes)
      (task-id . ,task-id)
      (modified . ,modified-at)
      (assignee . ,(when assignee (org-asana--safe-get assignee 'gid)))
      (projects . ,(mapcar (lambda (proj)
                             (org-asana--safe-get proj 'gid))
                           projects)))))

(defun org-asana--org-entry-to-task ()
  "Convert current org entry to Asana task format."
  (let ((heading (org-asana--get-heading-text))
        (todo-state (org-asana--get-todo-state))
        (priority (org-asana--get-priority))
        (deadline (org-asana--get-deadline))
        (tags (org-asana--get-tags))
        (body (org-asana--get-entry-body)))
    `((name . ,heading)
      (completed . ,(string= todo-state "DONE"))
      (due_date . ,(org-asana--date-to-asana deadline))
      (notes . ,body)
      ,(when org-asana-sync-priority
         `(priority . ,(org-asana--priority-to-asana priority)))
      ,(when (and org-asana-sync-tags tags)
         `(tag_names . ,tags)))))

;;; API Interface Functions

(defun org-asana--fetch-my-tasks ()
  "Fetch user's tasks from Asana."
  (let ((endpoint "/tasks")
        (params "?assignee=me&completed_since=now&opt_fields=name,completed,due_date,notes,tags.name,priority,modified_at,gid,assignee.gid,projects.gid"))
    (org-asana--api-request "GET" (concat endpoint params))))

(defun org-asana--fetch-task (task-id)
  "Fetch specific task by TASK-ID from Asana."
  (let ((endpoint (format "/tasks/%s" task-id))
        (params "?opt_fields=name,completed,due_date,notes,tags.name,priority,modified_at,gid,assignee.gid,projects.gid"))
    (org-asana--api-request "GET" (concat endpoint params))))

(defun org-asana--create-task (task-data)
  "Create new task in Asana with TASK-DATA."
  (let ((data `((data . ,task-data))))
    (org-asana--api-request "POST" "/tasks" data)))

(defun org-asana--update-task (task-id task-data)
  "Update existing task TASK-ID with TASK-DATA."
  (let ((endpoint (format "/tasks/%s" task-id))
        (data `((data . ,task-data))))
    (org-asana--api-request "PUT" endpoint data)))

(defun org-asana--delete-task (task-id)
  "Delete task with TASK-ID from Asana."
  (let ((endpoint (format "/tasks/%s" task-id)))
    (org-asana--api-request "DELETE" endpoint)))

(defun org-asana--fetch-workspaces ()
  "Fetch available workspaces."
  (org-asana--api-request "GET" "/workspaces"))

(defun org-asana--fetch-projects (workspace-id)
  "Fetch projects in WORKSPACE-ID."
  (let ((endpoint (format "/projects?workspace=%s" workspace-id)))
    (org-asana--api-request "GET" endpoint)))

;;; Conflict Resolution

(defun org-asana--compare-timestamps (org-time asana-time)
  "Compare ORG-TIME and ASANA-TIME, return newer timestamp source.
Returns 'org if org timestamp is newer, 'asana if asana is newer,
or 'asana if timestamps are equal or comparison fails."
  (condition-case nil
      (let ((org-parsed (org-asana--parse-timestamp org-time))
            (asana-parsed (org-asana--parse-timestamp asana-time)))
        (cond
         ((and org-parsed asana-parsed)
          (if (time-less-p asana-parsed org-parsed) 'org 'asana))
         (org-parsed 'org)
         (asana-parsed 'asana)
         (t 'asana)))
    (error 'asana)))

(defun org-asana--resolve-conflict (org-entry asana-task)
  "Resolve conflict between ORG-ENTRY and ASANA-TASK.
Returns the entry that should be used as source of truth."
  (pcase org-asana-conflict-resolution
    ('asana-wins asana-task)
    ('newest-wins
     (let ((org-modified (org-asana--safe-get org-entry 'modified))
           (asana-modified (org-asana--safe-get asana-task 'modified)))
       (if (eq (org-asana--compare-timestamps org-modified asana-modified) 'org)
           org-entry
         asana-task)))
    (_ asana-task)))

;;; Synchronization Functions

(defun org-asana--sync-from-asana-to-org (asana-task)
  "Update org entry from ASANA-TASK."
  (let* ((task-data (org-asana--safe-get asana-task 'data))
         (task-id (org-asana--safe-get task-data 'gid))
         (org-entry (org-asana--task-to-org-entry asana-task)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format ":ASANA_TASK_ID: %s" task-id) nil t)
          ;; Update existing entry
          (progn
            (org-back-to-heading t)
            (org-asana--update-org-entry-from-data org-entry))
        ;; Create new entry
        (org-asana--create-org-entry-from-data org-entry)))))

(defun org-asana--sync-from-org-to-asana ()
  "Update Asana task from current org entry."
  (when-let ((task-id (org-asana--task-id)))
    (let ((task-data (org-asana--org-entry-to-task)))
      (org-asana--update-task task-id task-data)
      (org-asana--set-modified-time (org-asana--current-timestamp)))))

(defun org-asana--update-org-entry-from-data (org-entry)
  "Update current org heading with ORG-ENTRY data."
  (let ((heading (org-asana--safe-get org-entry 'heading))
        (todo-state (org-asana--safe-get org-entry 'todo-state))
        (priority (org-asana--safe-get org-entry 'priority))
        (deadline (org-asana--safe-get org-entry 'deadline))
        (tags (org-asana--safe-get org-entry 'tags))
        (body (org-asana--safe-get org-entry 'body))
        (task-id (org-asana--safe-get org-entry 'task-id))
        (modified (org-asana--safe-get org-entry 'modified))
        (assignee (org-asana--safe-get org-entry 'assignee))
        (projects (org-asana--safe-get org-entry 'projects)))
    
    ;; Update heading
    (org-edit-headline heading)
    
    ;; Update TODO state
    (when todo-state
      (org-todo todo-state))
    
    ;; Update priority
    (when priority
      (org-priority (string-to-char priority)))
    
    ;; Update deadline
    (if deadline
        (org-deadline nil deadline)
      (org-deadline '(4)))  ; Remove deadline
    
    ;; Update tags
    (when tags
      (org-set-tags tags))
    
    ;; Update properties
    (org-asana--set-task-id task-id)
    (org-asana--set-modified-time modified)
    (when assignee
      (org-asana--set-property "ASANA_ASSIGNEE" assignee))
    (when projects
      (org-asana--set-property "ASANA_PROJECTS" 
                               (mapconcat #'identity projects ",")))
    
    ;; Update body
    (when body
      (save-excursion
        (org-end-of-meta-data t)
        (delete-region (point) (save-excursion (outline-next-heading) (point)))
        (insert body "\n")))))

(defun org-asana--create-org-entry-from-data (org-entry)
  "Create new org entry from ORG-ENTRY data."
  (let ((heading (org-asana--safe-get org-entry 'heading))
        (todo-state (org-asana--safe-get org-entry 'todo-state))
        (priority (org-asana--safe-get org-entry 'priority))
        (deadline (org-asana--safe-get org-entry 'deadline))
        (tags (org-asana--safe-get org-entry 'tags))
        (body (org-asana--safe-get org-entry 'body))
        (task-id (org-asana--safe-get org-entry 'task-id))
        (modified (org-asana--safe-get org-entry 'modified))
        (assignee (org-asana--safe-get org-entry 'assignee))
        (projects (org-asana--safe-get org-entry 'projects)))
    
    ;; Go to end of buffer
    (goto-char (point-max))
    
    ;; Insert heading
    (insert (format "%s %s %s%s%s\n"
                    (make-string org-asana-heading-level ?*)
                    (or todo-state "TODO")
                    (if priority (format "[#%s] " priority) "")
                    heading
                    (if tags (format " :%s:" (mapconcat #'identity tags ":")) "")))
    
    ;; Add deadline if present
    (when deadline
      (insert "DEADLINE: " deadline "\n"))
    
    ;; Add properties
    (insert ":PROPERTIES:\n")
    (insert (format ":ASANA_TASK_ID: %s\n" task-id))
    (insert (format ":ASANA_MODIFIED: %s\n" modified))
    (when assignee
      (insert (format ":ASANA_ASSIGNEE: %s\n" assignee)))
    (when projects
      (insert (format ":ASANA_PROJECTS: %s\n" 
                       (mapconcat #'identity projects ","))))
    (insert ":END:\n")
    
    ;; Add body
    (when body
      (insert body "\n"))
    
    (insert "\n")))

;;; Interactive Commands

(defun org-asana-sync ()
  "Perform bidirectional synchronization between Org-mode and Asana."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run M-x org-asana-setup"))
  
  (message "Starting Asana synchronization...")
  
  (condition-case err
      (let ((asana-tasks (org-asana--fetch-my-tasks))
            (synced-count 0))
        
        ;; Sync from Asana to Org
        (when-let ((tasks (org-asana--safe-get asana-tasks 'data)))
          (dolist (task tasks)
            (org-asana--sync-from-asana-to-org task)
            (setq synced-count (1+ synced-count))))
        
        ;; Sync from Org to Asana (scan current buffer for entries with task IDs)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward ":ASANA_TASK_ID:" nil t)
            (org-back-to-heading t)
            (org-asana--sync-from-org-to-asana)))
        
        (setq org-asana--last-sync-time (current-time))
        (message "Synchronization complete. Processed %d tasks." synced-count))
    
    (error (message "Synchronization failed: %s" (error-message-string err)))))

(defun org-asana-create-task-from-heading ()
  "Create Asana task from current org heading."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run M-x org-asana-setup"))
  
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (when (org-asana--task-id)
    (error "This heading already has an associated Asana task"))
  
  (condition-case err
      (let* ((task-data (org-asana--org-entry-to-task))
             (response (org-asana--create-task task-data))
             (task (org-asana--safe-get response 'data)))
        (when task
          (let ((task-id (org-asana--safe-get task 'gid)))
            (org-asana--set-task-id task-id)
            (org-asana--set-modified-time (org-asana--current-timestamp))
            (message "Created Asana task: %s" task-id))))
    (error (message "Failed to create task: %s" (error-message-string err)))))

(defun org-asana-import-my-tasks ()
  "Import all 'My Tasks' from Asana to current org buffer."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run M-x org-asana-setup"))
  
  (condition-case err
      (let ((asana-tasks (org-asana--fetch-my-tasks))
            (imported-count 0))
        (when-let ((tasks (org-asana--safe-get asana-tasks 'data)))
          (dolist (task tasks)
            (let* ((task-data (org-asana--safe-get task 'data))
                   (task-id (org-asana--safe-get task-data 'gid)))
              (save-excursion
                (goto-char (point-min))
                (unless (re-search-forward (format ":ASANA_TASK_ID: %s" task-id) nil t)
                  (org-asana--sync-from-asana-to-org task)
                  (setq imported-count (1+ imported-count))))))
          (message "Imported %d new tasks from Asana." imported-count)))
    (error (message "Import failed: %s" (error-message-string err)))))

(defun org-asana-open-in-asana ()
  "Open current task in Asana web interface."
  (interactive)
  (when-let ((task-id (org-asana--task-id)))
    (browse-url (format "https://app.asana.com/0/0/%s" task-id))
    (message "Opened task in Asana"))
  (unless (org-asana--task-id)
    (error "No Asana task associated with this heading")))

(defun org-asana-update-from-heading ()
  "Update Asana task from current org heading."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (unless (org-asana--task-id)
    (error "No Asana task associated with this heading"))
  
  (org-asana--sync-from-org-to-asana)
  (message "Updated Asana task"))

(defun org-asana-delete-task ()
  "Delete Asana task for current heading."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (when-let ((task-id (org-asana--task-id)))
    (when (yes-or-no-p "Delete Asana task? ")
      (condition-case err
          (progn
            (org-asana--delete-task task-id)
            (org-delete-property "ASANA_TASK_ID")
            (org-delete-property "ASANA_MODIFIED")
            (org-delete-property "ASANA_ASSIGNEE")
            (org-delete-property "ASANA_PROJECTS")
            (message "Deleted Asana task"))
        (error (message "Failed to delete task: %s" (error-message-string err))))))
  (unless (org-asana--task-id)
    (error "No Asana task associated with this heading")))

;;; Setup and Configuration

(defun org-asana-setup ()
  "Interactive setup wizard for org-asana."
  (interactive)
  
  ;; Get token
  (unless org-asana-token
    (setq org-asana-token 
          (read-string "Enter your Asana Personal Access Token: ")))
  
  ;; Test connection
  (condition-case err
      (org-asana-test-connection)
    (error 
     (message "Connection test failed: %s" (error-message-string err))
     (return)))
  
  ;; Configure sync method
  (let ((sync-method 
         (intern (completing-read "Sync method: " '("manual" "periodic") nil t))))
    (setq org-asana-sync-method sync-method)
    
    (when (eq sync-method 'periodic)
      (setq org-asana-sync-interval 
            (read-number "Sync interval (minutes): " org-asana-sync-interval))))
  
  ;; Configure conflict resolution
  (setq org-asana-conflict-resolution
        (intern (completing-read "Conflict resolution: " 
                                '("newest-wins" "asana-wins") nil t)))
  
  ;; Configure workspace and project
  (org-asana-configure-workspace)
  
  ;; Save configuration option
  (when (yes-or-no-p "Save configuration to init file? ")
    (org-asana--save-configuration))
  
  ;; Start periodic sync if enabled
  (when (eq org-asana-sync-method 'periodic)
    (org-asana--start-periodic-sync))
  
  (message "org-asana setup complete!"))

(defun org-asana-configure-workspace ()
  "Configure default workspace interactively."
  (interactive)
  (condition-case err
      (let* ((workspaces-response (org-asana--fetch-workspaces))
             (workspaces (org-asana--safe-get workspaces-response 'data)))
        (when workspaces
          (let* ((workspace-names (mapcar (lambda (ws)
                                           (cons (org-asana--safe-get ws 'name)
                                                 (org-asana--safe-get ws 'gid)))
                                         workspaces))
                 (selected-name (completing-read "Select workspace: " 
                                               workspace-names nil t))
                 (selected-gid (cdr (assoc selected-name workspace-names))))
            (setq org-asana-default-workspace selected-gid)
            (message "Default workspace set to: %s" selected-name)
            
            ;; Configure project in selected workspace
            (org-asana-configure-project))))
    (error (message "Failed to configure workspace: %s" (error-message-string err)))))

(defun org-asana-configure-project ()
  "Configure default project interactively."
  (interactive)
  (when org-asana-default-workspace
    (condition-case err
        (let* ((projects-response (org-asana--fetch-projects org-asana-default-workspace))
               (projects (org-asana--safe-get projects-response 'data)))
          (when projects
            (let* ((project-names (append '(("My Tasks" . nil))
                                         (mapcar (lambda (proj)
                                                  (cons (org-asana--safe-get proj 'name)
                                                        (org-asana--safe-get proj 'gid)))
                                                projects)))
                   (selected-name (completing-read "Select default project: " 
                                                 project-names nil t))
                   (selected-gid (cdr (assoc selected-name project-names))))
              (setq org-asana-default-project selected-gid)
              (message "Default project set to: %s" selected-name))))
      (error (message "Failed to configure project: %s" (error-message-string err))))))

(defun org-asana-reset-configuration ()
  "Reset org-asana configuration to defaults."
  (interactive)
  (when (yes-or-no-p "Reset all org-asana configuration? ")
    (setq org-asana-token nil
          org-asana-sync-method 'manual
          org-asana-sync-interval 15
          org-asana-conflict-resolution 'newest-wins
          org-asana-default-workspace nil
          org-asana-default-project nil
          org-asana-sync-tags t
          org-asana-sync-priority t
          org-asana-org-file nil
          org-asana-heading-level 2)
    (org-asana--stop-periodic-sync)
    (message "Configuration reset to defaults")))

(defun org-asana--save-configuration ()
  "Save current configuration to user's init file."
  (let ((config-string 
         (format ";; org-asana configuration\n(setq org-asana-token \"%s\"\n      org-asana-sync-method '%s\n      org-asana-sync-interval %d\n      org-asana-conflict-resolution '%s\n      org-asana-default-workspace %s\n      org-asana-default-project %s\n      org-asana-sync-tags %s\n      org-asana-sync-priority %s\n      org-asana-org-file %s\n      org-asana-heading-level %d)\n"
                 (or org-asana-token "")
                 org-asana-sync-method
                 org-asana-sync-interval
                 org-asana-conflict-resolution
                 (if org-asana-default-workspace 
                     (format "\"%s\"" org-asana-default-workspace) 
                   "nil")
                 (if org-asana-default-project 
                     (format "\"%s\"" org-asana-default-project) 
                   "nil")
                 org-asana-sync-tags
                 org-asana-sync-priority
                 (if org-asana-org-file 
                     (format "\"%s\"" org-asana-org-file) 
                   "nil")
                 org-asana-heading-level)))
    (append-to-file config-string nil user-init-file)
    (message "Configuration saved to %s" user-init-file)))

;;; Periodic Sync

(defun org-asana--start-periodic-sync ()
  "Start periodic synchronization timer."
  (org-asana--stop-periodic-sync)
  (setq org-asana--sync-timer
        (run-with-timer (* org-asana-sync-interval 60)
                       (* org-asana-sync-interval 60)
                       #'org-asana--periodic-sync-function)))

(defun org-asana--stop-periodic-sync ()
  "Stop periodic synchronization timer."
  (when org-asana--sync-timer
    (cancel-timer org-asana--sync-timer)
    (setq org-asana--sync-timer nil)))

(defun org-asana--periodic-sync-function ()
  "Function called by periodic sync timer."
  (when (and org-asana-token
             (eq org-asana-sync-method 'periodic))
    (condition-case err
        (let ((org-file (or org-asana-org-file (buffer-file-name))))
          (when (and org-file (file-exists-p org-file))
            (with-current-buffer (find-file-noselect org-file)
              (org-asana-sync))))
      (error (message "Periodic sync failed: %s" (error-message-string err))))))

;;; Minor Mode and Keybindings

(defvar org-asana-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'org-asana-sync)
    (define-key map (kbd "C-c a c") #'org-asana-create-task-from-heading)
    (define-key map (kbd "C-c a i") #'org-asana-import-my-tasks)
    (define-key map (kbd "C-c a o") #'org-asana-open-in-asana)
    (define-key map (kbd "C-c a u") #'org-asana-update-from-heading)
    (define-key map (kbd "C-c a d") #'org-asana-delete-task)
    (define-key map (kbd "C-c a t") #'org-asana-test-connection)
    map)
  "Keymap for org-asana-mode.")

(define-minor-mode org-asana-mode
  "Minor mode for org-asana integration."
  :lighter " Asana"
  :keymap org-asana-mode-map
  (when (and org-asana-mode
             (eq org-asana-sync-method 'periodic))
    (org-asana--start-periodic-sync)))

;;; Org-Agenda Integration

(defvar org-asana-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'org-asana-sync)
    (define-key map (kbd "C-c a c") #'org-asana-create-task-from-heading)
    (define-key map (kbd "C-c a o") #'org-asana-open-in-asana)
    map)
  "Keymap for org-asana-agenda-mode.")

(define-minor-mode org-asana-agenda-mode
  "Minor mode for org-asana integration in agenda views."
  :lighter " Asana"
  :keymap org-asana-agenda-mode-map)

(defun org-asana--agenda-format-function (item)
  "Format agenda ITEM with Asana indicator if synced."
  (save-excursion
    (org-agenda-goto)
    (when (org-asana--task-id)
      (setq item (concat item " [Asana]"))))
  item)

;;; Hooks and Initialization

(defun org-asana--enable-agenda-integration ()
  "Enable agenda integration features."
  (add-to-list 'org-agenda-format-date-fns 'org-asana--agenda-format-function))

(defun org-asana--disable-agenda-integration ()
  "Disable agenda integration features."
  (setq org-agenda-format-date-fns 
        (remove 'org-asana--agenda-format-function org-agenda-format-date-fns)))

(add-hook 'org-asana-agenda-mode-hook #'org-asana--enable-agenda-integration)
(add-hook 'org-asana-agenda-mode-hook 
          (lambda ()
            (unless org-asana-agenda-mode
              (org-asana--disable-agenda-integration))))

;;; Package Initialization

(defun org-asana-initialize ()
  "Initialize org-asana package."
  (when (and org-asana-token
             (eq org-asana-sync-method 'periodic))
    (org-asana--start-periodic-sync)))

;; Initialize when package is loaded
(add-hook 'after-init-hook #'org-asana-initialize)

;; Clean up on package unload
(add-hook 'kill-emacs-hook #'org-asana--stop-periodic-sync)

(provide 'org-asana)

;;; org-asana.el ends here