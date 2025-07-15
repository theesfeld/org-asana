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
  "Strategy for resolving conflicts between Org and Asana data.
'newest-wins uses modification timestamps to determine winner.
'asana-wins always prefers Asana data over Org data."
  :type '(choice (const :tag "Newest modification wins" newest-wins)
                 (const :tag "Asana always wins" asana-wins))
  :group 'org-asana)

(defcustom org-asana-default-workspace nil
  "Default Asana workspace GID to use for operations.
When nil, the first available workspace will be used."
  :type '(choice (const :tag "Auto-select workspace" nil)
                 (string :tag "Workspace GID"))
  :group 'org-asana)

(defcustom org-asana-default-project nil
  "Default Asana project GID for new tasks.
When nil, tasks will be created in 'My Tasks' only."
  :type '(choice (const :tag "My Tasks only" nil)
                 (string :tag "Project GID"))
  :group 'org-asana)

(defcustom org-asana-sync-tags t
  "Whether to synchronize Org tags with Asana tags."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-sync-priority t
  "Whether to synchronize Org priority with Asana priority."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-org-file nil
  "Org file to use for Asana task synchronization.
When nil, tasks will be added to the current buffer."
  :type '(choice (const :tag "Current buffer" nil)
                 (file :tag "Specific org file"))
  :group 'org-asana)

(defcustom org-asana-heading-level 2
  "Org heading level to use for Asana tasks."
  :type 'integer
  :group 'org-asana)

;;; Internal Variables

(defvar org-asana--sync-timer nil
  "Timer object for periodic synchronization.")

(defvar org-asana--rate-limit-tracker nil
  "Tracker for API rate limiting.")

(defvar org-asana--cached-workspaces nil
  "Cached list of available workspaces.")

(defvar org-asana--cached-projects nil
  "Cached list of available projects.")

;;; Utility Functions

(defun org-asana--get-token ()
  "Get the configured Asana token."
  (or org-asana-token
      (error "Asana token not configured. Set `org-asana-token' or run `org-asana-setup'")))

(defun org-asana--http-headers ()
  "Return HTTP headers for Asana API requests."
  `(("Authorization" . ,(format "Bearer %s" (org-asana--get-token)))
    ("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defun org-asana--api-url (endpoint)
  "Construct full API URL for ENDPOINT."
  (concat org-asana-api-base-url endpoint))

(defun org-asana--parse-json-response (buffer)
  "Parse JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun org-asana--handle-http-error (status)
  "Handle HTTP error STATUS."
  (cond
   ((= status 401) (error "Asana authentication failed. Check your token"))
   ((= status 403) (error "Asana access forbidden. Check token permissions"))
   ((= status 404) (error "Asana resource not found"))
   ((= status 429) (error "Asana rate limit exceeded. Please wait"))
   ((>= status 500) (error "Asana server error (%d)" status))
   (t (error "Asana API error (%d)" status))))

;;; Authentication and Setup Functions

(defun org-asana-authenticate ()
  "Authenticate with Asana API using configured token."
  (unless org-asana-token
    (error "No Asana token configured. Run `org-asana-setup' first"))
  (org-asana-test-connection))

(defun org-asana-test-connection ()
  "Test connection to Asana API."
  (interactive)
  (let* ((url (org-asana--api-url "/users/me"))
         (url-request-method "GET")
         (url-request-extra-headers (org-asana--http-headers))
         (buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (let ((status (string-to-number (match-string 1))))
                (if (= status 200)
                    (progn
                      (message "Asana connection successful")
                      t)
                  (org-asana--handle-http-error status)))
            (error "Invalid HTTP response from Asana")))
      (kill-buffer buffer))))

;;;###autoload
(defun org-asana-setup ()
  "Setup org-asana with initial configuration wizard."
  (interactive)
  (unless org-asana-token
    (setq org-asana-token
          (read-string "Enter your Asana Personal Access Token: ")))
  
  (if (org-asana-test-connection)
      (progn
        (when (y-or-n-p "Configure sync method? ")
          (let ((method (completing-read 
                        "Sync method: " 
                        '("manual" "periodic") nil t)))
            (setq org-asana-sync-method (intern method))
            (when (eq org-asana-sync-method 'periodic)
              (let ((interval (read-number "Sync interval (minutes): " 15)))
                (setq org-asana-sync-interval interval)))))
        
        (when (y-or-n-p "Set default org file for Asana tasks? ")
          (let ((file (read-file-name "Org file: " nil nil nil nil
                                     (lambda (name) (string-suffix-p ".org" name)))))
            (setq org-asana-org-file file)))
        
        (when (y-or-n-p "Configure sync options? ")
          (setq org-asana-sync-tags 
                (y-or-n-p "Sync org tags with Asana tags? "))
          (setq org-asana-sync-priority 
                (y-or-n-p "Sync org priority with Asana priority? "))
          (let ((conflict-method 
                 (completing-read 
                  "Conflict resolution strategy: "
                  '("newest-wins" "asana-wins") nil t)))
            (setq org-asana-conflict-resolution (intern conflict-method))))
        
        (when (y-or-n-p "Save configuration to init file? ")
          (org-asana--save-configuration))
        
        (message "org-asana setup complete!")
        (when (eq org-asana-sync-method 'periodic)
          (org-asana-start-periodic-sync)))
    (setq org-asana-token nil)
    (error "Setup failed. Please check your token")))

(defun org-asana--save-configuration ()
  "Save current org-asana configuration to user's init file."
  (let ((config-string 
         (format "
;; org-asana configuration
(setq org-asana-token \"%s\"
      org-asana-sync-method '%s
      org-asana-sync-interval %d
      org-asana-conflict-resolution '%s
      org-asana-sync-tags %s
      org-asana-sync-priority %s%s)
"
                 org-asana-token
                 org-asana-sync-method
                 org-asana-sync-interval
                 org-asana-conflict-resolution
                 org-asana-sync-tags
                 org-asana-sync-priority
                 (if org-asana-org-file
                     (format "\n      org-asana-org-file \"%s\"" org-asana-org-file)
                   ""))))
    
    (with-temp-buffer
      (insert config-string)
      (append-to-file (point-min) (point-max) user-init-file))
    (message "Configuration saved to %s" user-init-file)))

(defun org-asana-configure-workspace ()
  "Configure default Asana workspace."
  (interactive)
  (let* ((workspaces (org-asana-fetch-workspaces))
         (workspace-names (mapcar (lambda (ws) 
                                   (cons (alist-get 'name ws) 
                                         (alist-get 'gid ws)))
                                 workspaces))
         (selected (completing-read "Select workspace: " workspace-names nil t)))
    (setq org-asana-default-workspace (cdr (assoc selected workspace-names)))
    (message "Default workspace set to: %s" selected)))

(defun org-asana-reset-configuration ()
  "Reset org-asana configuration to defaults."
  (interactive)
  (when (y-or-n-p "Reset all org-asana configuration? ")
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
    (org-asana-stop-periodic-sync)
    (message "org-asana configuration reset to defaults")))

;;; Pure Data Transformation Functions

(defun org-asana--format-org-heading (level text)
  "Format org heading at LEVEL with TEXT."
  (concat (make-string level ?*) " " text))

(defun org-asana--parse-asana-timestamp (timestamp-str)
  "Parse Asana timestamp string to Emacs time."
  (when timestamp-str
    (date-to-time timestamp-str)))

(defun org-asana--format-org-timestamp (time)
  "Format Emacs TIME to org timestamp."
  (when time
    (format-time-string "<%Y-%m-%d %a>" time)))

(defun org-asana--format-org-timestamp-with-time (time)
  "Format Emacs TIME to org timestamp with time."
  (when time
    (format-time-string "<%Y-%m-%d %a %H:%M>" time)))

(defun org-asana--get-org-todo-state (completed)
  "Get org TODO state based on COMPLETED boolean."
  (if completed "DONE" "TODO"))

(defun org-asana--get-asana-completed (todo-state)
  "Get Asana completed boolean from org TODO-STATE."
  (string= todo-state "DONE"))

(defun org-asana--format-org-priority (asana-priority)
  "Convert ASANA-PRIORITY to org priority."
  (cond
   ((string= asana-priority "high") "[#A]")
   ((string= asana-priority "medium") "[#B]")
   ((string= asana-priority "low") "[#C]")
   (t "")))

(defun org-asana--get-asana-priority (org-priority)
  "Convert org priority character to Asana priority."
  (cond
   ((string= org-priority "A") "high")
   ((string= org-priority "B") "medium")
   ((string= org-priority "C") "low")
   (t nil)))

(defun org-asana--format-org-tags (asana-tags)
  "Convert ASANA-TAGS list to org tags string."
  (when asana-tags
    (concat ":" (mapconcat (lambda (tag) (alist-get 'name tag)) asana-tags ":") ":")))

(defun org-asana--parse-org-tags (tags-string)
  "Parse org TAGS-STRING to list of tag names."
  (when (and tags-string (not (string-empty-p tags-string)))
    (split-string (substring tags-string 1 -1) ":")))

(defun org-asana-task-to-org-entry (task)
  "Convert Asana TASK to org entry string."
  (let* ((name (alist-get 'name task))
         (notes (alist-get 'notes task))
         (completed (alist-get 'completed task))
         (due-on (alist-get 'due_on task))
         (tags (alist-get 'tags task))
         (gid (alist-get 'gid task))
         (modified-at (alist-get 'modified_at task))
         (assignee (alist-get 'assignee task))
         (projects (alist-get 'projects task))
         (todo-state (org-asana--get-org-todo-state completed))
         (priority-str (org-asana--format-org-priority 
                       (alist-get 'priority task)))
         (tags-str (when org-asana-sync-tags
                    (org-asana--format-org-tags tags)))
         (deadline-str (when due-on
                        (org-asana--format-org-timestamp
                         (org-asana--parse-asana-timestamp due-on)))))
    
    (concat
     (org-asana--format-org-heading org-asana-heading-level
                                   (concat todo-state " " priority-str name))
     (when tags-str (concat " " tags-str))
     "\n"
     (when deadline-str
       (concat "DEADLINE: " deadline-str "\n"))
     ":PROPERTIES:\n"
     ":ASANA_TASK_ID: " gid "\n"
     (when modified-at
       (concat ":ASANA_MODIFIED: " modified-at "\n"))
     (when assignee
       (concat ":ASANA_ASSIGNEE: " (alist-get 'gid assignee) "\n"))
     (when projects
       (concat ":ASANA_PROJECTS: " 
               (mapconcat (lambda (p) (alist-get 'gid p)) projects ",") "\n"))
     ":END:\n"
     (when (and notes (not (string-empty-p notes)))
       (concat notes "\n")))))

(defun org-asana-org-entry-to-task (entry-data)
  "Convert org ENTRY-DATA to Asana task data structure."
  (let* ((heading (plist-get entry-data :heading))
         (todo-state (plist-get entry-data :todo-state))
         (priority (plist-get entry-data :priority))
         (tags (plist-get entry-data :tags))
         (deadline (plist-get entry-data :deadline))
         (body (plist-get entry-data :body))
         (properties (plist-get entry-data :properties))
         (task-id (plist-get properties :ASANA_TASK_ID))
         (completed (org-asana--get-asana-completed todo-state))
         (asana-priority (when org-asana-sync-priority
                          (org-asana--get-asana-priority priority)))
         (due-date (when deadline
                    (format-time-string "%Y-%m-%d" deadline))))
    
    `((name . ,heading)
      (notes . ,(or body ""))
      (completed . ,completed)
      ,@(when due-date `((due_on . ,due-date)))
      ,@(when asana-priority `((priority . ,asana-priority)))
      ,@(when task-id `((gid . ,task-id))))))

(defun org-asana--extract-org-entry-data ()
  "Extract org entry data at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (heading (org-element-property :raw-value element))
           (todo-state (org-element-property :todo-keyword element))
           (priority (org-element-property :priority element))
           (tags (org-element-property :tags element))
           (deadline (org-element-property :deadline element))
           (properties (org-entry-properties))
           (body (org-get-entry)))
      
      `(:heading ,heading
        :todo-state ,todo-state
        :priority ,(when priority (char-to-string priority))
        :tags ,tags
        :deadline ,(when deadline (org-element-property :raw-value deadline))
        :body ,body
        :properties ,(mapcar (lambda (prop)
                              (cons (intern (concat ":" (car prop)))
                                    (cdr prop)))
                            properties)))))

;;; HTTP/API Interface Functions

(defun org-asana--make-request (method endpoint &optional data)
  "Make HTTP request to Asana API.
METHOD is the HTTP method, ENDPOINT is the API path, DATA is optional JSON data."
  (let* ((url (org-asana--api-url endpoint))
         (url-request-method method)
         (url-request-extra-headers (org-asana--http-headers))
         (url-request-data (when data (json-encode data)))
         (buffer (url-retrieve-synchronously url)))
    
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (let ((status (string-to-number (match-string 1))))
                (if (and (>= status 200) (< status 300))
                    (org-asana--parse-json-response buffer)
                  (org-asana--handle-http-error status)))
            (error "Invalid HTTP response from Asana")))
      (kill-buffer buffer))))

(defun org-asana-fetch-user-info ()
  "Fetch current user information from Asana."
  (let ((response (org-asana--make-request "GET" "/users/me")))
    (alist-get 'data response)))

(defun org-asana-fetch-workspaces ()
  "Fetch available workspaces from Asana."
  (let ((response (org-asana--make-request "GET" "/workspaces")))
    (setq org-asana--cached-workspaces (alist-get 'data response))
    org-asana--cached-workspaces))

(defun org-asana--get-workspace-gid ()
  "Get the workspace GID to use for operations."
  (or org-asana-default-workspace
      (let ((workspaces (or org-asana--cached-workspaces
                           (org-asana-fetch-workspaces))))
        (alist-get 'gid (car workspaces)))))

(defun org-asana-fetch-my-tasks ()
  "Fetch user's incomplete tasks from Asana."
  (let* ((user-info (org-asana-fetch-user-info))
         (user-gid (alist-get 'gid user-info))
         (workspace-gid (org-asana--get-workspace-gid))
         (endpoint (format "/users/%s/user_task_list?workspace=%s" 
                          user-gid workspace-gid))
         (task-list-response (org-asana--make-request "GET" endpoint))
         (task-list-gid (alist-get 'gid (alist-get 'data task-list-response)))
         ;; Fetch only incomplete tasks with limit of 100
         (tasks-endpoint (format "/user_task_lists/%s/tasks?completed_since=now&limit=100&opt_fields=gid,name,notes,completed,due_on,due_at,modified_at,priority,tags.name,assignee.gid,projects.gid" 
                                task-list-gid))
         (tasks-response (org-asana--make-request "GET" tasks-endpoint))
         (all-tasks (alist-get 'data tasks-response)))
    ;; Filter for incomplete tasks only
    (seq-filter (lambda (task) 
                  (not (alist-get 'completed task)))
                all-tasks)))

(defun org-asana-fetch-project-tasks (project-gid)
  "Fetch incomplete tasks from PROJECT-GID."
  (let* ((endpoint (format "/projects/%s/tasks?completed_since=now&limit=100&opt_fields=gid,name,notes,completed,due_on,due_at,modified_at,priority,tags.name,assignee.gid,projects.gid" 
                          project-gid))
         (response (org-asana--make-request "GET" endpoint))
         (all-tasks (alist-get 'data response)))
    ;; Filter for incomplete tasks only
    (seq-filter (lambda (task) 
                  (not (alist-get 'completed task)))
                all-tasks)))

(defun org-asana-fetch-all-my-tasks ()
  "Fetch all incomplete tasks assigned to me from all accessible workspaces and projects."
  (let* ((user-info (org-asana-fetch-user-info))
         (user-gid (alist-get 'gid user-info))
         (workspace-gid (org-asana--get-workspace-gid))
         ;; Search for tasks assigned to me
         (search-endpoint (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=gid,name,notes,completed,due_on,due_at,modified_at,priority,tags.name,assignee.gid,projects.gid"
                                 workspace-gid user-gid))
         (search-response (org-asana--make-request "GET" search-endpoint))
         (tasks (alist-get 'data search-response)))
    tasks))

(defcustom org-asana-sync-only-with-due-dates nil
  "If non-nil, only sync tasks that have due dates."
  :type 'boolean
  :group 'org-asana)

(defun org-asana-fetch-tasks-with-due-dates ()
  "Fetch all incomplete tasks with due dates assigned to the current user."
  (let* ((all-tasks (org-asana-fetch-all-my-tasks))
         (tasks-with-dates (seq-filter (lambda (task)
                                        (or (alist-get 'due_on task)
                                            (alist-get 'due_at task)))
                                      all-tasks)))
    tasks-with-dates))

(defun org-asana-fetch-task (task-gid)
  "Fetch detailed task information for TASK-GID."
  (let* ((endpoint (format "/tasks/%s?opt_fields=gid,name,notes,completed,due_on,modified_at,priority,tags.name,assignee.gid,projects.gid" 
                          task-gid))
         (response (org-asana--make-request "GET" endpoint)))
    (alist-get 'data response)))

(defun org-asana-create-task (task-data)
  "Create a new task in Asana with TASK-DATA."
  (let* ((workspace-gid (org-asana--get-workspace-gid))
         (data `((data . ,(append task-data 
                                 `((workspace . ,workspace-gid))))))
         (response (org-asana--make-request "POST" "/tasks" data)))
    (alist-get 'data response)))

(defun org-asana-update-task (task-gid task-data)
  "Update task TASK-GID with TASK-DATA."
  (let* ((endpoint (format "/tasks/%s" task-gid))
         (data `((data . ,task-data)))
         (response (org-asana--make-request "PUT" endpoint data)))
    (alist-get 'data response)))

(defun org-asana-delete-task (task-gid)
  "Delete task with TASK-GID."
  (let ((endpoint (format "/tasks/%s" task-gid)))
    (org-asana--make-request "DELETE" endpoint)))

(defun org-asana-add-task-to-project (task-gid project-gid)
  "Add TASK-GID to PROJECT-GID."
  (let* ((endpoint (format "/tasks/%s/addProject" task-gid))
         (data `((data . ((project . ,project-gid))))))
    (org-asana--make-request "POST" endpoint data)))

(defun org-asana-remove-task-from-project (task-gid project-gid)
  "Remove TASK-GID from PROJECT-GID."
  (let* ((endpoint (format "/tasks/%s/removeProject" task-gid))
         (data `((data . ((project . ,project-gid))))))
    (org-asana--make-request "POST" endpoint data)))

;;; Conflict Resolution Functions

(defun org-asana--compare-timestamps (org-time asana-time-str)
  "Compare ORG-TIME with ASANA-TIME-STR, return t if org is newer."
  (when (and org-time asana-time-str)
    (let ((asana-time (org-asana--parse-asana-timestamp asana-time-str)))
      (time-less-p asana-time org-time))))

(defun org-asana--get-entry-modified-time ()
  "Get modification time for current org entry."
  (let ((timestamp (org-entry-get nil "ASANA_MODIFIED")))
    (when timestamp
      (org-asana--parse-asana-timestamp timestamp))))

(defun org-asana--resolve-conflict (org-entry-data asana-task)
  "Resolve conflict between ORG-ENTRY-DATA and ASANA-TASK.
Returns :org or :asana depending on resolution strategy."
  (cond
   ((eq org-asana-conflict-resolution 'asana-wins) :asana)
   ((eq org-asana-conflict-resolution 'newest-wins)
    (let* ((properties (plist-get org-entry-data :properties))
           (org-modified (plist-get properties :ASANA_MODIFIED))
           (asana-modified (alist-get 'modified_at asana-task)))
      (if (and org-modified asana-modified)
          (if (org-asana--compare-timestamps 
               (org-asana--parse-asana-timestamp org-modified)
               asana-modified)
              :org
            :asana)
        :asana)))
   (t :asana)))

(defun org-asana--needs-sync-p (org-entry-data asana-task)
  "Check if ORG-ENTRY-DATA and ASANA-TASK need synchronization."
  (let* ((org-name (plist-get org-entry-data :heading))
         (asana-name (alist-get 'name asana-task))
         (org-completed (org-asana--get-asana-completed 
                        (plist-get org-entry-data :todo-state)))
         (asana-completed (alist-get 'completed asana-task))
         (properties (plist-get org-entry-data :properties))
         (org-modified (plist-get properties :ASANA_MODIFIED))
         (asana-modified (alist-get 'modified_at asana-task)))
    
    (or (not (string= org-name asana-name))
        (not (eq org-completed asana-completed))
        (and org-modified asana-modified
             (not (string= org-modified asana-modified))))))

(defun org-asana--merge-task-data (org-entry-data asana-task)
  "Merge ORG-ENTRY-DATA and ASANA-TASK based on conflict resolution."
  (let ((winner (org-asana--resolve-conflict org-entry-data asana-task)))
    (if (eq winner :asana)
        asana-task
      (org-asana-org-entry-to-task org-entry-data))))

;;; Synchronization Logic

(defun org-asana--find-org-entry-by-task-id (task-id)
  "Find org entry with TASK-ID in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward 
           (format "^[ \t]*:ASANA_TASK_ID:[ \t]+%s" task-id) nil t)
      (org-back-to-heading t)
      (point))))

(defun org-asana--get-all-org-asana-entries ()
  "Get all org entries with Asana task IDs in current buffer."
  (let ((entries '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ASANA_TASK_ID:[ \t]+\\(.+\\)" nil t)
        (let ((task-id (match-string 1)))
          (org-back-to-heading t)
          (let ((entry-data (org-asana--extract-org-entry-data)))
            (push (cons task-id entry-data) entries))))
      entries)))

(defun org-asana--sync-task-to-org (task)
  "Sync single TASK from Asana to org."
  (let* ((task-id (alist-get 'gid task))
         (existing-pos (org-asana--find-org-entry-by-task-id task-id)))
    
    (if existing-pos
        ;; Task exists locally - update it (including completed status)
        (progn
          (goto-char existing-pos)
          (let ((org-entry-data (org-asana--extract-org-entry-data)))
            (when (org-asana--needs-sync-p org-entry-data task)
              (let ((winner (org-asana--resolve-conflict org-entry-data task)))
                (when (eq winner :asana)
                  (org-cut-subtree)
                  (insert (org-asana-task-to-org-entry task)))))))
      
      ;; New task - only add if incomplete (we don't want to import completed tasks)
      (unless (alist-get 'completed task)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (org-asana-task-to-org-entry task))))))

(defun org-asana--sync-org-to-asana (entry-data task-id)
  "Sync org ENTRY-DATA to Asana for TASK-ID."
  (let ((task-data (org-asana-org-entry-to-task entry-data)))
    (if task-id
        (org-asana-update-task task-id task-data)
      (let ((new-task (org-asana-create-task task-data)))
        (save-excursion
          (org-back-to-heading t)
          (org-set-property "ASANA_TASK_ID" (alist-get 'gid new-task))
          (org-set-property "ASANA_MODIFIED" (alist-get 'modified_at new-task)))))))

(defun org-asana-sync-from-asana ()
  "Sync tasks from Asana to org."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run `org-asana-setup'"))
  
  (let ((buffer (if org-asana-org-file
                   (find-file-noselect org-asana-org-file)
                 (current-buffer))))
    
    (with-current-buffer buffer
      ;; First, get all local entries with Asana IDs to check for updates
      (let* ((local-entries (org-asana--get-all-org-asana-entries))
             (local-task-ids (mapcar #'car local-entries))
             (updated-count 0)
             (new-count 0))
        
        ;; Fetch and update existing tasks (including completed ones)
        (dolist (task-id local-task-ids)
          (condition-case err
              (let ((task (org-asana-fetch-task task-id)))
                (when task
                  (save-excursion
                    (org-asana--sync-task-to-org task)
                    (setq updated-count (1+ updated-count)))))
            (error (message "Failed to fetch task %s: %s" task-id (error-message-string err)))))
        
        ;; Fetch new incomplete tasks
        (let ((new-tasks (condition-case err
                            (if org-asana-sync-only-with-due-dates
                                (org-asana-fetch-tasks-with-due-dates)
                              (org-asana-fetch-all-my-tasks))
                          (error 
                           (message "Search API failed, falling back to My Tasks list: %s" 
                                   (error-message-string err))
                           (org-asana-fetch-my-tasks)))))
          
          (message "Fetched %d incomplete tasks from Asana" (length new-tasks))
          
          ;; Add only truly new tasks
          (save-excursion
            (dolist (task new-tasks)
              (unless (member (alist-get 'gid task) local-task-ids)
                (org-asana--sync-task-to-org task)
                (setq new-count (1+ new-count))))))
        
        (save-buffer)
        (message "Sync complete: %d existing tasks updated, %d new tasks added" 
                updated-count new-count)))))

(defun org-asana-sync-to-asana ()
  "Sync org entries to Asana."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run `org-asana-setup'"))
  
  (let ((entries (org-asana--get-all-org-asana-entries))
        (synced-count 0))
    
    (dolist (entry entries)
      (let* ((task-id (car entry))
             (entry-data (cdr entry))
             (properties (plist-get entry-data :properties))
             (existing-task-id (plist-get properties :ASANA_TASK_ID)))
        
        (condition-case err
            (progn
              (org-asana--sync-org-to-asana entry-data existing-task-id)
              (setq synced-count (1+ synced-count)))
          (error (message "Failed to sync task %s: %s" task-id (error-message-string err))))))
    
    (save-buffer)
    (message "Synced %d org entries to Asana" synced-count)))

(defun org-asana-sync-bidirectional ()
  "Perform bidirectional synchronization between org and Asana."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run `org-asana-setup'"))
  
  (message "Starting bidirectional sync...")
  (org-asana-sync-from-asana)
  (org-asana-sync-to-asana)
  (message "Bidirectional sync complete"))

;;;###autoload
(defun org-asana-sync ()
  "Main synchronization command."
  (interactive)
  (org-asana-sync-bidirectional))

(defun org-asana-start-periodic-sync ()
  "Start periodic synchronization timer."
  (interactive)
  (when org-asana--sync-timer
    (cancel-timer org-asana--sync-timer))
  
  (when (eq org-asana-sync-method 'periodic)
    (setq org-asana--sync-timer
          (run-at-time t (* org-asana-sync-interval 60)
                       #'org-asana-sync-bidirectional))
    (message "Started periodic sync every %d minutes" org-asana-sync-interval)))

(defun org-asana-stop-periodic-sync ()
  "Stop periodic synchronization timer."
  (interactive)
  (when org-asana--sync-timer
    (cancel-timer org-asana--sync-timer)
    (setq org-asana--sync-timer nil)
    (message "Stopped periodic sync")))

;;; Org-Agenda Integration

(defun org-asana--agenda-add-asana-info ()
  "Add Asana task information to agenda lines."
  (when (and (boundp 'org-agenda-mode) org-agenda-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-hd-marker)
          (let* ((marker (get-text-property (point) 'org-hd-marker))
                 (task-id (with-current-buffer (marker-buffer marker)
                           (save-excursion
                             (goto-char marker)
                             (org-entry-get nil "ASANA_TASK_ID")))))
            (when task-id
              (let ((line-end (line-end-position)))
                (goto-char line-end)
                (insert (propertize " [Asana]" 'face 'org-agenda-filter-tags))))))
        (forward-line 1)))))

(defun org-asana-agenda-sync ()
  "Sync Asana tasks from agenda view."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (progn
        (org-asana-sync)
        (org-agenda-redo-all))
    (error "Not in org-agenda-mode")))

(defun org-asana-agenda-create-task ()
  "Create Asana task from current agenda item."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let ((marker (org-get-at-bol 'org-hd-marker)))
        (if marker
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (org-asana-create-task-from-heading)))
          (error "No task at point")))
    (error "Not in org-agenda-mode")))

(defun org-asana-agenda-open-in-asana ()
  "Open current agenda item in Asana web interface."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let ((marker (org-get-at-bol 'org-hd-marker)))
        (if marker
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (let ((task-id (org-entry-get nil "ASANA_TASK_ID")))
                  (if task-id
                      (browse-url (format "https://app.asana.com/0/0/%s" task-id))
                    (error "No Asana task ID found")))))
          (error "No task at point")))
    (error "Not in org-agenda-mode")))

(define-minor-mode org-asana-agenda-mode
  "Minor mode for Asana integration in org-agenda."
  :lighter " Asana"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a s") #'org-asana-agenda-sync)
            (define-key map (kbd "C-c a c") #'org-asana-agenda-create-task)
            (define-key map (kbd "C-c a o") #'org-asana-agenda-open-in-asana)
            map)
  (if org-asana-agenda-mode
      (add-hook 'org-agenda-finalize-hook #'org-asana--agenda-add-asana-info nil t)
    (remove-hook 'org-agenda-finalize-hook #'org-asana--agenda-add-asana-info t)))

;;; Interactive Commands

;;;###autoload
(defun org-asana-create-task-from-heading ()
  "Create Asana task from current org heading."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (let* ((entry-data (org-asana--extract-org-entry-data))
         (existing-task-id (plist-get (plist-get entry-data :properties) :ASANA_TASK_ID)))
    
    (if existing-task-id
        (error "This heading already has an Asana task (ID: %s)" existing-task-id)
      (let* ((task-data (org-asana-org-entry-to-task entry-data))
             (new-task (org-asana-create-task task-data)))
        (org-set-property "ASANA_TASK_ID" (alist-get 'gid new-task))
        (org-set-property "ASANA_MODIFIED" (alist-get 'modified_at new-task))
        (message "Created Asana task: %s" (alist-get 'name new-task))))))

;;;###autoload
(defun org-asana-import-my-tasks ()
  "Import all tasks from Asana 'My Tasks'."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run `org-asana-setup'"))
  
  (let ((tasks (org-asana-fetch-my-tasks))
        (buffer (if org-asana-org-file
                   (find-file org-asana-org-file)
                 (current-buffer))))
    
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n* Asana Tasks (imported %s)\n" 
                     (format-time-string "%Y-%m-%d %H:%M")))
      
      (dolist (task tasks)
        (insert (org-asana-task-to-org-entry task)))
      
      (save-buffer)
      (message "Imported %d tasks from Asana" (length tasks)))))

;;;###autoload
(defun org-asana-delete-task ()
  "Delete Asana task for current org heading."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (let ((task-id (org-entry-get nil "ASANA_TASK_ID")))
    (unless task-id
      (error "No Asana task ID found for this heading"))
    
    (when (yes-or-no-p (format "Delete Asana task %s? " task-id))
      (org-asana-delete-task task-id)
      (org-delete-property "ASANA_TASK_ID")
      (org-delete-property "ASANA_MODIFIED")
      (org-delete-property "ASANA_ASSIGNEE")
      (org-delete-property "ASANA_PROJECTS")
      (message "Deleted Asana task %s" task-id))))

;;;###autoload
(defun org-asana-open-in-asana ()
  "Open current org heading's Asana task in web browser."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (let ((task-id (org-entry-get nil "ASANA_TASK_ID")))
    (unless task-id
      (error "No Asana task ID found for this heading"))
    
    (browse-url (format "https://app.asana.com/0/0/%s" task-id))))

;;;###autoload
(defun org-asana-update-from-heading ()
  "Update Asana task from current org heading."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at an org heading"))
  
  (let ((task-id (org-entry-get nil "ASANA_TASK_ID")))
    (unless task-id
      (error "No Asana task ID found for this heading"))
    
    (let* ((entry-data (org-asana--extract-org-entry-data))
           (task-data (org-asana-org-entry-to-task entry-data))
           (updated-task (org-asana-update-task task-id task-data)))
      (org-set-property "ASANA_MODIFIED" (alist-get 'modified_at updated-task))
      (message "Updated Asana task: %s" (alist-get 'name updated-task)))))

;;; Minor Mode

(defvar org-asana-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'org-asana-sync)
    (define-key map (kbd "C-c a c") #'org-asana-create-task-from-heading)
    (define-key map (kbd "C-c a i") #'org-asana-import-my-tasks)
    (define-key map (kbd "C-c a d") #'org-asana-delete-task)
    (define-key map (kbd "C-c a o") #'org-asana-open-in-asana)
    (define-key map (kbd "C-c a u") #'org-asana-update-from-heading)
    (define-key map (kbd "C-c a t") #'org-asana-test-connection)
    map)
  "Keymap for org-asana-mode.")

;;;###autoload
(define-minor-mode org-asana-mode
  "Minor mode for Asana integration in org-mode."
  :lighter " Asana"
  :keymap org-asana-mode-map
  (if org-asana-mode
      (progn
        (when (and org-asana-token (eq org-asana-sync-method 'periodic))
          (org-asana-start-periodic-sync))
        (message "org-asana-mode enabled"))
    (org-asana-stop-periodic-sync)
    (message "org-asana-mode disabled")))

;;;###autoload
(defun org-asana-debug-fetch ()
  "Debug function to test task fetching from Asana."
  (interactive)
  (unless org-asana-token
    (error "Asana token not configured. Run `org-asana-setup'"))
  
  (condition-case err
      (let* ((user-info (org-asana-fetch-user-info))
             (user-gid (alist-get 'gid user-info))
             (workspaces (org-asana-fetch-workspaces))
             (workspace-gid (org-asana--get-workspace-gid)))
        
        (message "=== ASANA DEBUG INFO ===")
        (message "User: %s (GID: %s)" (alist-get 'name user-info) user-gid)
        (message "Workspaces found: %d" (length workspaces))
        (dolist (ws workspaces)
          (message "  - %s (GID: %s)" (alist-get 'name ws) (alist-get 'gid ws)))
        (message "Using workspace GID: %s" workspace-gid)
        
        ;; Try to fetch task list
        (let* ((endpoint (format "/users/%s/user_task_list?workspace=%s" 
                                user-gid workspace-gid))
               (task-list-response (org-asana--make-request "GET" endpoint))
               (task-list-data (alist-get 'data task-list-response))
               (task-list-gid (alist-get 'gid task-list-data)))
          
          (message "Task list GID: %s" task-list-gid)
          (message "Task list name: %s" (alist-get 'name task-list-data))
          
          ;; Try to fetch tasks
          (let* ((tasks-endpoint (format "/user_task_lists/%s/tasks?limit=10" 
                                        task-list-gid))
                 (tasks-response (org-asana--make-request "GET" tasks-endpoint))
                 (tasks (alist-get 'data tasks-response)))
            
            (message "Tasks found: %d" (length tasks))
            (when (> (length tasks) 0)
              (message "First few tasks:")
              (let ((count 0))
                (dolist (task tasks)
                  (when (< count 5)
                    (message "  - %s (completed: %s)" 
                            (alist-get 'name task)
                            (if (alist-get 'completed task) "yes" "no"))
                    (setq count (1+ count))))))))
        
        (message "=== END DEBUG INFO ==="))
    
    (error
     (message "Error during debug: %s" (error-message-string err)))))

(provide 'org-asana)
;;; org-asana.el ends here