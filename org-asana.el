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
;; Package-Requires: ((emacs "30.1"))
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
(require 'cl-lib)

;;; Module Loading

(defcustom org-asana-modules 
  '(org-asana-cache
    org-asana-custom-fields
    org-asana-dependencies
    org-asana-subtasks
    org-asana-webhooks
    org-asana-attachments
    org-asana-users
    org-asana-types
    org-asana-browser)
  "List of org-asana modules to load.
Each module provides additional functionality:
- org-asana-cache: Caching layer to reduce API calls
- org-asana-custom-fields: Support for Asana Premium custom fields
- org-asana-dependencies: Task dependency management
- org-asana-subtasks: Recursive subtask synchronization
- org-asana-webhooks: Real-time updates via webhooks
- org-asana-attachments: File attachment upload/download
- org-asana-users: User and assignee management
- org-asana-types: Task type indicators (milestone, approval)
- org-asana-browser: Interactive task browser"
  :type '(set (const :tag "Caching layer" org-asana-cache)
              (const :tag "Custom fields" org-asana-custom-fields)
              (const :tag "Dependencies" org-asana-dependencies)
              (const :tag "Subtasks" org-asana-subtasks)
              (const :tag "Webhooks" org-asana-webhooks)
              (const :tag "Attachments" org-asana-attachments)
              (const :tag "User management" org-asana-users)
              (const :tag "Task types" org-asana-types)
              (const :tag "Task browser" org-asana-browser))
  :group 'org-asana)

(defun org-asana--load-modules ()
  "Load configured org-asana modules."
  (dolist (module org-asana-modules)
    (condition-case err
        (require module nil t)
      (error (message "Failed to load %s: %s" module (error-message-string err))))))

;;; Error Conditions

(define-error 'org-asana-error "Org-Asana error")
(define-error 'org-asana-auth-error "Authentication failed" 'org-asana-error)
(define-error 'org-asana-rate-limit-error "Rate limit exceeded" 'org-asana-error)
(define-error 'org-asana-sync-error "Sync operation failed" 'org-asana-error)
(define-error 'org-asana-api-error "API request failed" 'org-asana-error)

;;; Custom Faces

(defface org-asana-priority-high
  '((t :foreground "red" :weight bold))
  "Face for high priority tasks."
  :group 'org-asana)

(defface org-asana-priority-medium
  '((t :foreground "orange"))
  "Face for medium priority tasks."
  :group 'org-asana)

(defface org-asana-deadline-warning
  '((t :foreground "dark orange" :weight bold))
  "Face for tasks with upcoming deadlines."
  :group 'org-asana)

(defface org-asana-deadline-overdue
  '((t :foreground "red" :weight bold :underline t))
  "Face for overdue tasks."
  :group 'org-asana)

;;; Custom Variables

(defgroup org-asana nil
  "Options for org-asana."
  :tag "Org Asana"
  :group 'org)

(defcustom org-asana-token nil
  "Personal Access Token for Asana API.
If nil, will attempt to retrieve from authinfo."
  :type '(choice (const :tag "Use authinfo" nil)
                 (string :tag "Token"))
  :group 'org-asana)

(defcustom org-asana-authinfo-machine "app.asana.com"
  "Machine name to use when looking up token in authinfo."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-org-file "~/org/asana.org"
  "Path to the Org file for Asana tasks."
  :type 'file
  :group 'org-asana)

(defcustom org-asana-fetch-metadata t
  "Whether to fetch comments and attachments for tasks.
Uses the Asana Batch API to efficiently fetch metadata in parallel."
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

(defcustom org-asana-enable-save-hook nil
  "Whether to enable automatic sync on save.
When enabled, saving the org-asana file will trigger a full sync."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-enable-todo-hook nil
  "Whether to sync when TODO state changes.
When enabled, changing TODO states will trigger a full sync."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-enable-agenda-integration t
  "Whether to add org-asana file to agenda files."
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

(defvar org-asana--initialized nil
  "Whether org-asana has been initialized.")

;;; Data Storage - Hash Tables

(defvar org-asana--projects-table nil
  "Hash table storing all projects by GID.")

(defvar org-asana--sections-table nil
  "Hash table storing all sections by GID.")

(defvar org-asana--tasks-table nil
  "Hash table storing all tasks by GID.")

(defun org-asana--create-projects-table ()
  "Create and return a new projects hash table."
  (make-hash-table :test 'equal :size 50))

(defun org-asana--create-sections-table ()
  "Create and return a new sections hash table."
  (make-hash-table :test 'equal :size 200))

(defun org-asana--create-tasks-table ()
  "Create and return a new tasks hash table."
  (make-hash-table :test 'equal :size 1000))

(defun org-asana--clear-all-tables ()
  "Clear all data storage tables."
  (setq org-asana--projects-table (org-asana--create-projects-table))
  (setq org-asana--sections-table (org-asana--create-sections-table))
  (setq org-asana--tasks-table (org-asana--create-tasks-table)))

;;; Project Storage Functions

(defun org-asana--store-project (project)
  "Store PROJECT in projects table."
  (puthash (alist-get 'gid project) project org-asana--projects-table))

(defun org-asana--get-project (gid)
  "Get project by GID."
  (gethash gid org-asana--projects-table))

(defun org-asana--get-all-projects ()
  "Get all projects as a list."
  (hash-table-values org-asana--projects-table))

(defun org-asana--project-name (gid)
  "Get name of project with GID."
  (alist-get 'name (org-asana--get-project gid)))

(defun org-asana--project-exists-p (gid)
  "Return t if project with GID exists."
  (gethash gid org-asana--projects-table nil))

;;; Section Storage Functions

(defun org-asana--store-section (section)
  "Store SECTION in sections table."
  (puthash (alist-get 'gid section) section org-asana--sections-table))

(defun org-asana--get-section (gid)
  "Get section by GID."
  (gethash gid org-asana--sections-table))

(defun org-asana--get-all-sections ()
  "Get all sections as a list."
  (hash-table-values org-asana--sections-table))

(defun org-asana--section-name (gid)
  "Get name of section with GID."
  (alist-get 'name (org-asana--get-section gid)))

(defun org-asana--section-project-gid (gid)
  "Get project GID for section with GID."
  (alist-get 'project (org-asana--get-section gid)))

;;; Task Storage Functions

(defun org-asana--validate-task-data (task)
  "Validate and sanitize TASK data."
  (when task
    (let ((gid (alist-get 'gid task)))
      (unless (and gid (stringp gid))
        (error "Invalid task: missing or non-string GID"))
      ;; Ensure all expected fields are strings if present
      (dolist (field '(name notes permalink_url))
        (let ((value (alist-get field task)))
          (when (and value (not (stringp value)))
            (error "Invalid task field %s: expected string, got %s" 
                   field (type-of value)))))
      t)))

(defun org-asana--store-task (task)
  "Store TASK in tasks table."
  (when (org-asana--validate-task-data task)
    (puthash (alist-get 'gid task) task org-asana--tasks-table)))

(defun org-asana--get-task (gid)
  "Get task by GID."
  (gethash gid org-asana--tasks-table))

(defun org-asana--get-all-tasks ()
  "Get all tasks as a list."
  (hash-table-values org-asana--tasks-table))

(defun org-asana--task-name (gid)
  "Get name of task with GID."
  (alist-get 'name (org-asana--get-task gid)))

(defun org-asana--task-completed-p (gid)
  "Return t if task with GID is completed."
  (alist-get 'completed (org-asana--get-task gid)))

(defun org-asana--task-project-gids (gid)
  "Get list of project GIDs for task with GID."
  (let* ((task (org-asana--get-task gid))
         (memberships (alist-get 'memberships task)))
    (mapcar (lambda (m)
              (alist-get 'gid (alist-get 'project m)))
            (if (vectorp memberships)
                (append memberships nil)
              memberships))))

(defun org-asana--task-section-gids (gid)
  "Get list of section GIDs for task with GID."
  (let* ((task (org-asana--get-task gid))
         (memberships (alist-get 'memberships task)))
    (mapcar (lambda (m)
              (alist-get 'gid (alist-get 'section m)))
            (if (vectorp memberships)
                (append memberships nil)
              memberships))))

;;; Cross-Reference Functions

(defun org-asana--project-sections (project-gid)
  "Get all sections for PROJECT-GID."
  (seq-filter (lambda (section)
                (equal project-gid
                       (alist-get 'gid (alist-get 'project section))))
              (org-asana--get-all-sections)))

(defun org-asana--section-tasks (section-gid)
  "Get all tasks in SECTION-GID."
  (seq-filter (lambda (task)
                (member section-gid (org-asana--task-section-gids
                                   (alist-get 'gid task))))
              (org-asana--get-all-tasks)))

(defun org-asana--project-tasks (project-gid)
  "Get all tasks in PROJECT-GID."
  (seq-filter (lambda (task)
                (member project-gid (org-asana--task-project-gids
                                   (alist-get 'gid task))))
              (org-asana--get-all-tasks)))

;;; Sorting Functions

(defun org-asana--sort-by-name (items)
  "Sort ITEMS by name field."
  (sort items (lambda (a b)
                (string< (or (alist-get 'name a) "")
                        (or (alist-get 'name b) "")))))

(defun org-asana--sort-by-due-date (tasks)
  "Sort TASKS by due date."
  (sort tasks (lambda (a b)
                (let ((date-a (alist-get 'due_on a))
                      (date-b (alist-get 'due_on b)))
                  (cond
                   ((and date-a date-b) (string< date-a date-b))
                   (date-a t)
                   (t nil))))))

(defun org-asana--sort-by-created-date (items)
  "Sort ITEMS by created date."
  (sort items (lambda (a b)
                (string< (or (alist-get 'created_at a) "")
                        (or (alist-get 'created_at b) "")))))

(defun org-asana--sort-by-modified-date (items)
  "Sort ITEMS by modified date."
  (sort items (lambda (a b)
                (string> (or (alist-get 'modified_at a) "")
                        (or (alist-get 'modified_at b) "")))))

;;; Data Loading Functions

(defun org-asana--populate-from-tasks (tasks)
  "Populate all hash tables from TASKS list."
  (org-asana--clear-all-tables)
  (dolist (task tasks)
    (org-asana--process-task-memberships task)
    (org-asana--store-task task)))

(defun org-asana--process-task-memberships (task)
  "Process and store projects/sections from TASK memberships."
  (let ((memberships (alist-get 'memberships task)))
    (dolist (membership (if (vectorp memberships)
                           (append memberships nil)
                         memberships))
      (org-asana--process-membership-project membership)
      (org-asana--process-membership-section membership))))

(defun org-asana--process-membership-project (membership)
  "Process and store project from MEMBERSHIP."
  (let ((project (alist-get 'project membership)))
    (when (and project (alist-get 'gid project))
      (unless (org-asana--project-exists-p (alist-get 'gid project))
        (org-asana--store-project project)))))

(defun org-asana--process-membership-section (membership)
  "Process and store section from MEMBERSHIP."
  (let ((section (alist-get 'section membership))
        (project (alist-get 'project membership)))
    (when (and section (alist-get 'gid section))
      (unless (org-asana--get-section (alist-get 'gid section))
        (let ((section-with-project (cons (cons 'project project) section)))
          (org-asana--store-section section-with-project))))))

;;; Tree Building Functions Using Hash Tables

(defun org-asana--build-org-tree-from-tables (metadata-map)
  "Build org tree using data from hash tables with METADATA-MAP."
  (let ((projects (org-asana--sort-by-name (org-asana--get-all-projects))))
    (mapcar (lambda (project)
              (org-asana--create-project-node-from-tables
               project 1 (or metadata-map (make-hash-table :test 'equal))))
            projects)))

(defun org-asana--create-project-node-from-tables (project level metadata-map)
  "Create project node from PROJECT at LEVEL with METADATA-MAP."
  (let* ((project-gid (alist-get 'gid project))
         (sections (org-asana--sort-by-name
                   (org-asana--project-sections project-gid))))
    (list :level level
          :title (alist-get 'name project)
          :properties `(("ASANA-PROJECT-GID" . ,project-gid))
          :children (mapcar (lambda (section)
                             (org-asana--create-section-node-from-tables
                              section (1+ level) metadata-map))
                           sections))))

(defun org-asana--create-section-node-from-tables (section level metadata-map)
  "Create section node from SECTION at LEVEL with METADATA-MAP."
  (let* ((section-gid (alist-get 'gid section))
         (tasks (org-asana--sort-by-created-date
                (org-asana--section-tasks section-gid))))
    (list :level level
          :title (alist-get 'name section)
          :properties `(("ASANA-SECTION-GID" . ,section-gid))
          :children (mapcar (lambda (task)
                             (org-asana--create-task-node-from-tables
                              task (1+ level) metadata-map))
                           tasks))))

(defun org-asana--create-task-node-from-tables (task level metadata-map)
  "Create task node from TASK at LEVEL with METADATA-MAP."
  (let* ((task-gid (alist-get 'gid task))
         (metadata (gethash task-gid metadata-map))
         (properties (org-asana--task-to-properties task metadata))
         (body-text (org-asana--format-task-body properties metadata))
         ;; Use type-aware title formatting if available
         (task-title (if (and (boundp 'org-asana-modules)
                             (memq 'org-asana-types org-asana-modules)
                             (fboundp 'org-asana--format-task-title-with-type))
                        (org-asana--format-task-title-with-type task)
                      (org-asana--format-task-title task)))
         (completed (alist-get 'completed task))
         (todo-keyword (if completed "DONE" "TODO")))
    (list :level level
          :title task-title
          :todo-keyword todo-keyword
          :properties (org-asana--plist-to-alist properties)
          :body body-text)))

(defun org-asana--format-task-title (task)
  "Format TASK title with section prefix."
  (let* ((task-gid (alist-get 'gid task))
         (task-name (alist-get 'name task))
         (section-gids (org-asana--task-section-gids task-gid))
         (section-name (when section-gids
                        (org-asana--section-name (car section-gids)))))
    (if section-name
        (format "[%s] %s" section-name task-name)
      task-name)))


;;; Org File Loading Functions

(defvar org-asana--org-projects-table nil
  "Hash table storing projects from org file.")

(defvar org-asana--org-sections-table nil
  "Hash table storing sections from org file.")

(defvar org-asana--org-tasks-table nil
  "Hash table storing tasks from org file.")

(defun org-asana--create-org-data-tables ()
  "Create hash tables for org file data."
  (setq org-asana--org-projects-table (make-hash-table :test 'equal :size 50))
  (setq org-asana--org-sections-table (make-hash-table :test 'equal :size 200))
  (setq org-asana--org-tasks-table (make-hash-table :test 'equal :size 1000)))

(defun org-asana--load-org-file-to-tables ()
  "Load existing org file data into hash tables."
  (org-asana--create-org-data-tables)
  (when (file-exists-p org-asana-org-file)
    (with-current-buffer (find-file-noselect org-asana-org-file)
      (org-map-entries #'org-asana--process-org-entry))))

(defun org-asana--process-org-entry ()
  "Process current org entry and store in appropriate table."
  (let ((props (org-entry-properties)))
    (cond
     ((assoc "ASANA-PROJECT-GID" props)
      (org-asana--store-org-project props))
     ((assoc "ASANA-SECTION-GID" props)
      (org-asana--store-org-section props))
     ((assoc "ASANA-TASK-GID" props)
      (org-asana--store-org-task props)))))

(defun org-asana--store-org-project (props)
  "Store project from PROPS in org projects table."
  (let* ((gid (cdr (assoc "ASANA-PROJECT-GID" props)))
         (title (org-get-heading t t t t))
         (project `((gid . ,gid) (name . ,title))))
    (puthash gid project org-asana--org-projects-table)))

(defun org-asana--store-org-section (props)
  "Store section from PROPS in org sections table."
  (let* ((gid (cdr (assoc "ASANA-SECTION-GID" props)))
         (title (org-get-heading t t t t))
         (section `((gid . ,gid) (name . ,title))))
    (puthash gid section org-asana--org-sections-table)))

(defun org-asana--store-org-task (props)
  "Store task from PROPS in org tasks table."
  (let* ((gid (cdr (assoc "ASANA-TASK-GID" props)))
         (title (org-get-heading t t t t))
         (todo-state (org-get-todo-state))
         (completed (equal todo-state "DONE"))
         (modified-at (cdr (assoc "ASANA-MODIFIED-AT" props)))
         (task `((gid . ,gid)
                 (name . ,title)
                 (completed . ,completed)
                 (modified_at . ,modified-at))))
    (puthash gid task org-asana--org-tasks-table)))

;;; Comment Parsing Functions

(defun org-asana--extract-comment-gid (comment-line)
  "Extract comment GID from COMMENT-LINE."
  (when (string-match "\\[\\[asana-comment:\\([^]]+\\)\\]\\[" comment-line)
    (match-string 1 comment-line)))

(defun org-asana--extract-comment-text (comment-line)
  "Extract comment text from COMMENT-LINE."
  (cond
   ;; Asana comment format: [[asana-comment:GID][DATE]] AUTHOR: TEXT
   ((string-match "\\]: \\(.+\\)$" comment-line)
    (match-string 1 comment-line))
   ;; New comment format: [DATE]: TEXT
   ((string-match "^\\[[-0-9 A-Za-z:]+\\]: \\(.+\\)$" comment-line)
    (match-string 1 comment-line))))

(defun org-asana--parse-comments-drawer ()
  "Parse comments from current task's COMMENTS drawer."
  (save-excursion
    (when (org-asana--find-comments-drawer)
      (let ((comments '())
            (drawer-end (save-excursion
                         (re-search-forward "^:END:$" nil t)
                         (line-beginning-position))))
        (while (and (< (point) drawer-end)
                   (re-search-forward "^\\(.+\\)$" drawer-end t))
          (let ((line (match-string 1)))
            (cond
             ;; Existing Asana comment
             ((string-match "asana-comment:" line)
              (push (cons (org-asana--extract-comment-gid line)
                         (org-asana--extract-comment-text line))
                    comments))
             ;; New local comment
             ((string-match "^\\[[-0-9 A-Za-z:]+\\]:" line)
              (push (cons nil ; No GID for new comments
                         (org-asana--extract-comment-text line))
                    comments)))))
        (nreverse comments)))))

(defun org-asana--find-comments-drawer ()
  "Find COMMENTS drawer in current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (re-search-forward "^:COMMENTS:$" end t))))

(defun org-asana--get-task-comments (task-gid)
  "Get all comments for TASK-GID from org buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format ":ASANA-TASK-GID: %s" task-gid) nil t)
      (when (org-asana--find-comments-drawer)
        (org-asana--parse-comments-drawer)))))

;;; Comparison Functions

(defun org-asana--task-modified-p (task-gid)
  "Check if task with TASK-GID has been modified."
  (let ((org-task (gethash task-gid org-asana--org-tasks-table))
        (api-task (gethash task-gid org-asana--tasks-table)))
    (and org-task api-task
         (string> (alist-get 'modified_at api-task)
                  (alist-get 'modified_at org-task)))))

(defun org-asana--task-completed-locally-p (task-gid)
  "Check if task with TASK-GID was completed locally."
  (let ((org-task (gethash task-gid org-asana--org-tasks-table)))
    (and org-task (alist-get 'completed org-task))))

(defun org-asana--find-new-tasks ()
  "Find tasks that exist in API but not in org file."
  (let ((new-tasks '()))
    (maphash (lambda (gid task)
               (unless (gethash gid org-asana--org-tasks-table)
                 (push task new-tasks)))
             org-asana--tasks-table)
    new-tasks))

(defun org-asana--find-deleted-tasks ()
  "Find tasks that exist in org file but not in API."
  (let ((deleted-tasks '()))
    (maphash (lambda (gid _task)
               (unless (gethash gid org-asana--tasks-table)
                 (push gid deleted-tasks)))
             org-asana--org-tasks-table)
    deleted-tasks))

;;; Bidirectional Sync Functions

(defun org-asana--sync-bidirectional ()
  "Perform bidirectional sync between org and Asana."
  (message "Starting bidirectional sync...")
  (let* ((tasks-raw (org-asana--fetch-all-tasks))
         (tasks (org-asana--ensure-list tasks-raw)))
    (org-asana--populate-from-tasks tasks)
    (org-asana--load-org-file-to-tables)
    (org-asana--process-sync-changes)
    (org-asana--rebuild-org-file)))

(defun org-asana--process-sync-changes ()
  "Process all sync changes between org and API."
  (org-asana--handle-new-tasks)
  (org-asana--handle-deleted-tasks)
  (org-asana--handle-modified-tasks)
  (org-asana--handle-completed-tasks)
  (org-asana--handle-task-comments)
  ;; Handle dependencies if module is loaded
  (when (and (boundp 'org-asana-modules)
             (memq 'org-asana-dependencies org-asana-modules)
             (fboundp 'org-asana--sync-all-dependencies))
    (org-asana--sync-all-dependencies))
  ;; Handle attachments if module is loaded
  (when (and (boundp 'org-asana-modules)
             (memq 'org-asana-attachments org-asana-modules)
             org-asana-auto-download-attachments
             (fboundp 'org-asana--download-all-attachments-in-buffer))
    (org-asana--download-all-attachments-in-buffer)))

(defun org-asana--handle-new-tasks ()
  "Handle tasks that are new from API."
  (let ((new-tasks (org-asana--find-new-tasks)))
    (when new-tasks
      (message "Found %d new tasks from Asana" (length new-tasks)))))

(defun org-asana--handle-deleted-tasks ()
  "Handle tasks that were deleted in API."
  (let ((deleted-tasks (org-asana--find-deleted-tasks)))
    (when deleted-tasks
      (message "Found %d tasks to remove" (length deleted-tasks)))))

(defun org-asana--handle-modified-tasks ()
  "Handle tasks that were modified in API."
  (let ((modified-count 0))
    (maphash (lambda (gid _task)
               (when (org-asana--task-modified-p gid)
                 (cl-incf modified-count)))
             org-asana--tasks-table)
    (when (> modified-count 0)
      (message "Found %d modified tasks" modified-count))))

(defun org-asana--handle-completed-tasks ()
  "Handle tasks with changed completion status that need sync to Asana."
  (let ((completed-locally '())
        (incomplete-locally '()))
    (maphash (lambda (gid org-task)
               (let ((api-task (gethash gid org-asana--tasks-table)))
                 (when api-task
                   (let ((org-completed (alist-get 'completed org-task))
                         (api-completed (alist-get 'completed api-task)))
                     (cond
                      ;; Task completed locally but not in Asana
                      ((and org-completed (not api-completed))
                       (push gid completed-locally))
                      ;; Task incomplete locally but completed in Asana
                      ((and (not org-completed) api-completed)
                       (push gid incomplete-locally)))))))
             org-asana--org-tasks-table)
    (when completed-locally
      (message "Found %d tasks to mark complete in Asana"
               (length completed-locally))
      (dolist (gid completed-locally)
        (org-asana--mark-task-complete gid)))
    (when incomplete-locally
      (message "Found %d tasks to mark incomplete in Asana"
               (length incomplete-locally))
      (dolist (gid incomplete-locally)
        (org-asana--mark-task-incomplete gid)))))

(defun org-asana--handle-task-comments ()
  "Handle new comments that need to be synced to Asana."
  (when org-asana-fetch-metadata
    (let ((new-comments-count 0))
      (maphash (lambda (gid _org-task)
                 (let ((new-comments (org-asana--find-new-comments gid)))
                   (when new-comments
                     (dolist (comment new-comments)
                       (org-asana--create-comment gid comment)
                       (cl-incf new-comments-count)))))
               org-asana--org-tasks-table)
      (when (> new-comments-count 0)
        (message "Created %d new comments in Asana" new-comments-count)))))

(defun org-asana--find-new-comments (task-gid)
  "Find new comments for TASK-GID that don't have asana-comment links."
  (let ((all-comments (org-asana--get-task-comments task-gid))
        (new-comments '()))
    (dolist (comment all-comments)
      (unless (car comment) ; No GID means it's a new comment
        (push (cdr comment) new-comments)))
    (nreverse new-comments)))

(defun org-asana--rebuild-org-file ()
  "Rebuild org file from updated hash tables."
  (let* ((metadata-map (when org-asana-fetch-metadata
                         (org-asana--fetch-all-metadata (org-asana--get-all-tasks))))
         (org-tree (org-asana--build-org-tree-from-tables metadata-map))
         (buffer (org-asana--prepare-buffer)))
    (org-asana--render-org-tree org-tree buffer)
    (with-current-buffer buffer
      (org-asana--update-progress-indicators)
      (when (and (boundp 'org-asana-apply-faces) org-asana-apply-faces)
        (org-asana--apply-task-faces))
      ;; Apply type faces if module is loaded
      (when (and (boundp 'org-asana-modules)
                 (memq 'org-asana-types org-asana-modules)
                 (fboundp 'org-asana--apply-type-faces-buffer))
        (org-asana--apply-type-faces-buffer))
      (save-buffer))))

;;; API Update Functions

(defun org-asana--mark-task-complete (task-gid)
  "Mark task with TASK-GID as complete in Asana."
  (org-asana--make-request
   "PUT"
   (format "/tasks/%s" task-gid)
   '((data . ((completed . t))))))

(defun org-asana--mark-task-incomplete (task-gid)
  "Mark task with TASK-GID as incomplete in Asana."
  (org-asana--make-request
   "PUT"
   (format "/tasks/%s" task-gid)
   '((data . ((completed . nil))))))

(defun org-asana--create-comment (task-gid comment-text)
  "Create a comment on TASK-GID with COMMENT-TEXT."
  (org-asana--make-request
   "POST"
   (format "/tasks/%s/stories" task-gid)
   `((data . ((text . ,comment-text))))))

;;; Display and Query Functions

(defun org-asana--display-all-projects ()
  "Display all projects in a temporary buffer."
  (interactive)
  (let ((projects (org-asana--sort-by-name (org-asana--get-all-projects)))
        (buffer (get-buffer-create "*Asana Projects*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Asana Projects\n")
      (insert "==============\n\n")
      (dolist (project projects)
        (org-asana--insert-project-summary project))
      (goto-char (point-min))
      (view-mode))
    (display-buffer buffer)))

(defun org-asana--insert-project-summary (project)
  "Insert summary of PROJECT at point."
  (let* ((gid (alist-get 'gid project))
         (name (alist-get 'name project))
         (task-count (length (org-asana--project-tasks gid)))
         (section-count (length (org-asana--project-sections gid))))
    (insert (format "* %s\n" name))
    (insert (format "  GID: %s\n" gid))
    (insert (format "  Sections: %d\n" section-count))
    (insert (format "  Tasks: %d\n\n" task-count))))

(defun org-asana--display-tasks-by-due-date ()
  "Display all tasks sorted by due date."
  (interactive)
  (let ((tasks (org-asana--sort-by-due-date (org-asana--get-all-tasks)))
        (buffer (get-buffer-create "*Asana Tasks by Due Date*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Asana Tasks by Due Date\n\n")
      (dolist (task tasks)
        (org-asana--insert-task-with-context task))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun org-asana--extract-task-context (task)
  "Extract context information from TASK."
  (let ((gid (alist-get 'gid task)))
    (list :gid gid
          :name (alist-get 'name task)
          :due-date (alist-get 'due_on task)
          :project-gids (org-asana--task-project-gids gid)
          :section-gids (org-asana--task-section-gids gid))))

(defun org-asana--format-context-names (gids name-fn)
  "Format GIDS using NAME-FN to get names."
  (mapconcat name-fn gids ", "))

(defun org-asana--insert-task-heading (name)
  "Insert task heading with NAME."
  (insert (format "* %s\n" name)))

(defun org-asana--insert-task-deadline (due-date)
  "Insert task DEADLINE if DUE-DATE exists."
  (when due-date
    (insert (format "  DEADLINE: <%s>\n" due-date))))

(defun org-asana--insert-context-line (label gids name-fn)
  "Insert context line with LABEL and GIDS using NAME-FN."
  (when gids
    (insert (format "  %s: %s\n" label
                    (org-asana--format-context-names gids name-fn)))))

(defun org-asana--insert-task-with-context (task)
  "Insert TASK with project/section context."
  (let ((context (org-asana--extract-task-context task)))
    (org-asana--insert-task-heading (plist-get context :name))
    (org-asana--insert-task-deadline (plist-get context :due-date))
    (org-asana--insert-context-line "Projects"
                                   (plist-get context :project-gids)
                                   #'org-asana--project-name)
    (org-asana--insert-context-line "Sections"
                                   (plist-get context :section-gids)
                                   #'org-asana--section-name)
    (insert "\n")))

(defun org-asana--find-tasks-by-keyword (keyword)
  "Find all tasks containing KEYWORD in name or notes."
  (interactive "sSearch keyword: ")
  (let ((matching-tasks '()))
    (maphash (lambda (_gid task)
               (when (or (string-match-p keyword (or (alist-get 'name task) ""))
                        (string-match-p keyword (or (alist-get 'notes task) "")))
                 (push task matching-tasks)))
             org-asana--tasks-table)
    (org-asana--display-search-results keyword matching-tasks)))

(defun org-asana--display-search-results (keyword tasks)
  "Display search results for KEYWORD with TASKS."
  (let ((buffer (get-buffer-create "*Asana Search Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Search Results for '%s'\n\n" keyword))
      (insert (format "Found %d matching tasks\n\n" (length tasks)))
      (dolist (task (org-asana--sort-by-name tasks))
        (org-asana--insert-task-with-context task))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun org-asana--count-table-entries ()
  "Count entries in all hash tables."
  (list :tasks (hash-table-count org-asana--tasks-table)
        :projects (hash-table-count org-asana--projects-table)
        :sections (hash-table-count org-asana--sections-table)))

(defun org-asana--count-task-dates ()
  "Count tasks with due dates and overdue tasks."
  (let ((with-dates 0)
        (overdue 0)
        (today (format-time-string "%Y-%m-%d")))
    (maphash (lambda (_gid task)
               (let ((due-date (alist-get 'due_on task)))
                 (when due-date
                   (cl-incf with-dates)
                   (when (string< due-date today)
                     (cl-incf overdue)))))
             org-asana--tasks-table)
    (list :with-dates with-dates :overdue overdue)))

(defun org-asana--format-statistics (counts dates)
  "Format statistics message from COUNTS and DATES."
  (format "Asana Statistics: %d projects, %d sections, %d tasks (%d with due dates, %d overdue)"
          (plist-get counts :projects)
          (plist-get counts :sections)
          (plist-get counts :tasks)
          (plist-get dates :with-dates)
          (plist-get dates :overdue)))

(defun org-asana--statistics ()
  "Display Asana workspace statistics."
  (interactive)
  (let ((counts (org-asana--count-table-entries))
        (dates (org-asana--count-task-dates)))
    (message "%s" (org-asana--format-statistics counts dates))))

;;; API Request Functions

(defun org-asana--check-rate-limit ()
  "Check and enforce API rate limits."
  (when (and org-asana--rate-limit-reset
             (< org-asana--rate-limit-remaining 10)
             (> (float-time org-asana--rate-limit-reset) (float-time)))
    (let ((wait-time (- (float-time org-asana--rate-limit-reset) (float-time))))
      (message "Rate limit low. Waiting %.1f seconds..." wait-time)
      (sleep-for wait-time))))

(defun org-asana--build-request-headers ()
  "Build request headers with authentication."
  `(("Authorization" . ,(concat "Bearer " (org-asana--get-token)))
    ("Content-Type" . "application/json")))

(defun org-asana--build-request-url (endpoint)
  "Build full request URL from ENDPOINT."
  (concat org-asana-api-base-url endpoint))

(defun org-asana--parse-api-response (buffer)
  "Parse API response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$")
    (let ((json-array-type 'list))
      (json-read))))

(defun org-asana--execute-http-request (url method headers data)
  "Execute HTTP request to URL with METHOD, HEADERS and DATA."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (when data (json-encode data))))
    (url-retrieve-synchronously url t t)))

(defun org-asana--make-request (method endpoint &optional data)
  "Make an API request to METHOD ENDPOINT with optional DATA."
  ;; Use cached request if cache module is loaded
  (if (and (boundp 'org-asana-modules) 
           (memq 'org-asana-cache org-asana-modules)
           (fboundp 'org-asana--cached-request))
      (org-asana--cached-request method endpoint data)
    ;; Otherwise use standard request
    (org-asana--make-request-uncached method endpoint data)))

(defun org-asana--make-request-uncached (method endpoint &optional data)
  "Make an uncached API request to METHOD ENDPOINT with optional DATA."
  (org-asana--validate-token)
  (org-asana--check-rate-limit)
  (let* ((url (org-asana--build-request-url endpoint))
         (headers (org-asana--build-request-headers))
         (buffer nil))
    (condition-case err
        (progn
          (setq buffer (org-asana--execute-http-request url method headers data))
          (if (buffer-live-p buffer)
              (unwind-protect
                  (prog1 (org-asana--parse-api-response buffer)
                    (sleep-for org-asana-rate-limit-delay))
                (kill-buffer buffer))
            (signal 'org-asana-api-error '("Failed to retrieve response"))))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal 'org-asana-api-error (list (error-message-string err)))))))

(defun org-asana--fetch-paginated (endpoint)
  "Fetch all pages of data from ENDPOINT."
  (let ((all-data '())
        (next-path nil)
        (first-request t))
    (catch 'done
      (while t
        (let* ((url (if first-request
                       endpoint
                       (alist-get 'path next-path)))
               (response (org-asana--make-request "GET" url)))
          (setq first-request nil)
          (setq all-data (append all-data (alist-get 'data response)))
          (setq next-path (alist-get 'next_page response))
          (unless next-path (throw 'done all-data)))))))

(defun org-asana--fetch-workspace-info ()
  "Fetch current user and workspace information."
  (let ((response (org-asana--make-request "GET" "/users/me")))
    (let* ((user-data (alist-get 'data response))
           (workspaces (alist-get 'workspaces user-data))
           (workspace-list (if (vectorp workspaces)
                              (append workspaces nil)
                            workspaces)))
      (list (alist-get 'gid user-data)
            (alist-get 'gid (car workspace-list))))))

(defun org-asana--build-task-opt-fields ()
  "Build opt_fields parameter for task API requests."
  (let ((base-fields (concat "gid,name,notes,html_notes,completed,completed_at,"
                            "due_on,due_at,start_on,start_at,"
                            "created_at,modified_at,created_by.name,"
                            "assignee.name,assignee.gid,assignee_status,"
                            "followers.name,num_likes,liked,"
                            "parent.gid,parent.name,num_subtasks,"
                            "tags.gid,tags.name,"
                            "memberships.project.gid,memberships.project.name,"
                            "memberships.section.gid,memberships.section.name,"
                            "permalink_url,resource_subtype")))
    (when (and (boundp 'org-asana-modules) (memq 'org-asana-types org-asana-modules))
      (setq base-fields (concat base-fields ",approval_status,is_rendered_as_separator")))
    (when (and (boundp 'org-asana-modules) (memq 'org-asana-custom-fields org-asana-modules))
      (setq base-fields (concat base-fields ",custom_fields,custom_fields.gid,custom_fields.name,custom_fields.type,custom_fields.enum_options,custom_fields.enum_value,custom_fields.multi_enum_values,custom_fields.text_value,custom_fields.number_value,custom_fields.date_value,custom_fields.people_value")))
    (when (and (boundp 'org-asana-modules) (memq 'org-asana-dependencies org-asana-modules))
      (setq base-fields (concat base-fields ",dependencies,dependents")))
    (when (and (boundp 'org-asana-modules) (memq 'org-asana-attachments org-asana-modules))
      (setq base-fields (concat base-fields ",num_attachments")))
    base-fields))

(defun org-asana--build-task-search-endpoint (workspace-gid user-gid)
  "Build task search endpoint for WORKSPACE-GID and USER-GID."
  (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s"
          workspace-gid user-gid (org-asana--build-task-opt-fields)))

(defun org-asana--fetch-all-tasks ()
  "Fetch all incomplete tasks assigned to current user."
  (let* ((workspace-info (org-asana--fetch-workspace-info))
         (user-gid (car workspace-info))
         (workspace-gid (cadr workspace-info))
         (endpoint (org-asana--build-task-search-endpoint workspace-gid user-gid))
         (tasks (org-asana--fetch-paginated endpoint)))
    ;; Fetch subtasks if module is loaded
    (when (and (boundp 'org-asana-modules)
               (memq 'org-asana-subtasks org-asana-modules)
               (fboundp 'org-asana--enhance-tasks-with-subtasks))
      (setq tasks (org-asana--enhance-tasks-with-subtasks tasks)))
    tasks))

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

(defun org-asana--make-batch-request (actions)
  "Make a batch request with ACTIONS list."
  (org-asana--make-request
   "POST"
   "/batch"
   `((actions . ,actions))))

(defun org-asana--fetch-metadata-batch (task-ids)
  "Fetch metadata for multiple TASK-IDS using batch API."
  (let ((actions (cl-loop for task-id in task-ids
                         append (list `((relative_path . ,(format "/tasks/%s/stories" task-id))
                                       (method . "GET"))
                                     `((relative_path . ,(format "/tasks/%s/attachments" task-id))
                                       (method . "GET"))))))
    (org-asana--make-batch-request actions)))

;;; Data Transformation Functions


(defun org-asana--format-timestamp (timestamp)
  "Format Asana TIMESTAMP to Org date format."
  (when timestamp
    (format-time-string "<%Y-%m-%d %a %H:%M>"
                       (date-to-time timestamp))))

(defun org-asana--format-date (date-string)
  "Format Asana DATE-STRING to Org date format."
  (when date-string
    (format-time-string "<%Y-%m-%d %a>"
                       (date-to-time date-string))))

(defun org-asana--sanitize-text (text)
  "Sanitize TEXT for safe inclusion in Org files."
  (save-match-data
    (when text
      (let ((sanitized text))
        (setq sanitized (replace-regexp-in-string "\\*" "\\\\*" sanitized))
        (setq sanitized (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" sanitized))
        sanitized))))

(defun org-asana--remove-org-properties (text)
  "Remove org properties blocks from TEXT."
  (save-match-data
    (replace-regexp-in-string ":PROPERTIES:[^:]*:END:[\n\r]*" "" text)))

(defun org-asana--remove-org-keywords (text)
  "Remove TODO/DONE keywords from TEXT."
  (save-match-data
    (replace-regexp-in-string "^\\(?:TODO\\|DONE\\|ODO\\) " "" text)))

(defun org-asana--clean-org-links (text)
  "Clean and extract text from org links in TEXT."
  (save-match-data
    (let ((cleaned text))
      (setq cleaned (replace-regexp-in-string
                     "\\[\\[https?://[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" cleaned))
      (setq cleaned (replace-regexp-in-string
                     "/[0-9]+/[0-9]+/[^]]*\\]\\[\\([^]]+\\)\\]\\]" "\\1" cleaned))
      cleaned)))

(defun org-asana--remove-org-metadata (text)
  "Remove deadlines, headings, and property lines from TEXT."
  (save-match-data
    (let ((cleaned text))
      (setq cleaned (replace-regexp-in-string "^DEADLINE: <[^>]+>[\n\r]*" "" cleaned))
      (setq cleaned (replace-regexp-in-string "^\\*+ " "" cleaned))
      (setq cleaned (replace-regexp-in-string "^:[A-Z_]+: .*[\n\r]*" "" cleaned))
      cleaned)))

(defun org-asana--normalize-whitespace (text)
  "Normalize whitespace in TEXT."
  (save-match-data
    (let ((cleaned (replace-regexp-in-string "[\n\r]+" "\n" text)))
      (setq cleaned (string-trim cleaned))
      (if (string-empty-p cleaned) nil cleaned))))

(defun org-asana--clean-notes (notes)
  "Clean NOTES field from org-mode corruption."
  (save-match-data
    (when notes
      (thread-first notes
        (org-asana--remove-org-properties)
        (org-asana--remove-org-keywords)
        (org-asana--clean-org-links)
        (org-asana--remove-org-metadata)
        (org-asana--normalize-whitespace)))))

(defun org-asana--ensure-list (obj)
  "Ensure OBJ is a list, converting from vector if needed."
  (if (vectorp obj) (append obj nil) obj))

(defun org-asana--extract-basic-properties (task)
  "Extract basic properties from TASK."
  (list :gid (alist-get 'gid task)
        :name (org-asana--sanitize-text (alist-get 'name task))
        :notes (org-asana--clean-notes (alist-get 'notes task))
        :permalink (alist-get 'permalink_url task)
        :parent (alist-get 'name (alist-get 'parent task))
        :num-subtasks (alist-get 'num_subtasks task)))

(defun org-asana--extract-date-properties (task)
  "Extract and format date properties from TASK."
  (list :due-on (org-asana--format-date (alist-get 'due_on task))
        :start-on (org-asana--format-date (alist-get 'start_on task))
        :created-at (org-asana--format-timestamp (alist-get 'created_at task))
        :modified-at (org-asana--format-timestamp (alist-get 'modified_at task))))

(defun org-asana--extract-people-properties (task)
  "Extract people-related properties from TASK."
  (let ((followers (alist-get 'followers task)))
    (list :created-by (alist-get 'name (alist-get 'created_by task))
          :assignee (alist-get 'name (alist-get 'assignee task))
          :assignee-status (alist-get 'assignee_status task)
          :followers (mapcar (lambda (f) (alist-get 'name f))
                           (org-asana--ensure-list followers))
          :num-likes (alist-get 'num_likes task))))

(defun org-asana--extract-tag-properties (task)
  "Extract tag properties from TASK."
  (let ((tags (alist-get 'tags task)))
    (list :tags (mapcar (lambda (tag) (alist-get 'name tag))
                       (org-asana--ensure-list tags)))))

(defun org-asana--extract-metadata-properties (metadata)
  "Extract metadata properties from METADATA."
  (list :stories (car metadata)
        :attachments (cdr metadata)))

(defun org-asana--task-to-properties (task metadata)
  "Convert TASK and METADATA to property list."
  (let ((properties (append (org-asana--extract-basic-properties task)
                           (org-asana--extract-date-properties task)
                           (org-asana--extract-people-properties task)
                           (org-asana--extract-tag-properties task)
                           (org-asana--extract-metadata-properties metadata))))
    ;; Add custom fields if module is loaded
    (when (and (boundp 'org-asana-modules)
               (memq 'org-asana-custom-fields org-asana-modules)
               (fboundp 'org-asana--extract-custom-field-properties))
      (setq properties (append properties (org-asana--extract-custom-field-properties task))))
    ;; Add type properties if module is loaded
    (when (and (boundp 'org-asana-modules)
               (memq 'org-asana-types org-asana-modules)
               (fboundp 'org-asana--add-type-properties))
      (setq properties (append properties (org-asana--add-type-properties task))))
    ;; Add assignee properties if module is loaded
    (when (and (boundp 'org-asana-modules)
               (memq 'org-asana-users org-asana-modules)
               (fboundp 'org-asana--add-assignee-to-properties))
      (setq properties (append properties (org-asana--add-assignee-to-properties task))))
    ;; Add dependency count if module is loaded
    (when (and (boundp 'org-asana-modules)
               (memq 'org-asana-dependencies org-asana-modules))
      (let ((dep-count (+ (length (alist-get 'dependencies task))
                         (length (alist-get 'dependents task)))))
        (when (> dep-count 0)
          (push `(:asana-dependency-count . ,(number-to-string dep-count)) properties))))
    properties))

;;; Org Tree Building Functions


;;; Rendering Functions

(defun org-asana--render-org-tree (org-tree buffer)
  "Render ORG-TREE to BUFFER."
  (with-current-buffer buffer
    (erase-buffer)
    (insert "#+TITLE: Asana Tasks\n")
    (insert "#+STARTUP: overview\n\n")
    (dolist (node org-tree)
      (org-asana--render-node node))
    (goto-char (point-min))))

(defun org-asana--extract-node-data (node)
  "Extract all data from NODE into a plist."
  (list :level (plist-get node :level)
        :title (plist-get node :title)
        :todo-keyword (plist-get node :todo-keyword)
        :properties (plist-get node :properties)
        :deadline (plist-get node :deadline)
        :scheduled (plist-get node :scheduled)
        :body (plist-get node :body)
        :children (plist-get node :children)))

(defun org-asana--render-node-content (data)
  "Render node content from DATA plist."
  (org-asana--insert-heading
   (plist-get data :level)
   (plist-get data :title)
   (plist-get data :todo-keyword))
  (when (plist-get data :deadline)
    (org-asana--insert-deadline (plist-get data :deadline)))
  (when (plist-get data :scheduled)
    (org-asana--insert-scheduled (plist-get data :scheduled)))
  (when (plist-get data :properties)
    (org-asana--insert-properties (plist-get data :properties)))
  (when (plist-get data :body)
    (org-asana--insert-body (plist-get data :body))))

(defun org-asana--render-children (children)
  "Render all CHILDREN nodes."
  (dolist (child children)
    (org-asana--render-node child)))

(defun org-asana--render-node (node)
  "Render NODE to current buffer."
  (let ((data (org-asana--extract-node-data node)))
    (org-asana--render-node-content data)
    (org-asana--render-children (plist-get data :children))))

(defun org-asana--insert-heading (level title &optional todo-keyword)
  "Insert heading at LEVEL with TITLE and optional TODO-KEYWORD."
  (insert (make-string level ?*) " ")
  (when (and todo-keyword (>= level 3))
    (insert todo-keyword " "))
  (insert title)
  (insert "\n"))

(defun org-asana--insert-deadline (deadline)
  "Insert DEADLINE on current heading."
  (when deadline
    (insert "DEADLINE: " deadline "\n")))

(defun org-asana--insert-scheduled (scheduled)
  "Insert SCHEDULED date on current heading."
  (when scheduled
    (insert "SCHEDULED: " scheduled "\n")))

(defun org-asana--insert-properties (properties)
  "Insert PROPERTIES drawer."
  (insert ":PROPERTIES:\n")
  (dolist (prop properties)
    (when (and (cdr prop)
               ;; Skip stories - they'll be handled separately
               (not (equal (car prop) "stories")))
      (let ((value (cdr prop)))
        (insert ":" (car prop) ": " 
                (cond
                 ;; If value is a string, insert it directly
                 ((stringp value) value)
                 ;; If value is a list of strings, join them
                 ((and (listp value)
                       (cl-every #'stringp value))
                  (mapconcat #'identity value ", "))
                 ;; For any other type, convert to string
                 (t (format "%s" value)))
                "\n"))))
  (insert ":END:\n"))

(defun org-asana--insert-body (body)
  "Insert BODY text."
  (when (and body (not (string-empty-p body)))
    (insert "\n" body "\n")))

(defun org-asana--count-subtask-progress ()
  "Count DONE and total tasks under current heading."
  (let ((done 0)
        (total 0))
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\{4,\\} \\(TODO\\|DONE\\)" nil t)
          (setq total (1+ total))
          (when (string= (match-string 1) "DONE")
            (setq done (1+ done))))))
    (cons done total)))

(defun org-asana--update-single-progress-indicator ()
  "Update progress indicator at point."
  (let ((stars (match-string 1))
        (title (match-string 2))
        (progress (org-asana--count-subtask-progress)))
    (replace-match
     (format "%s %s [%d/%d]" stars title (car progress) (cdr progress))
     t t)))

(defun org-asana--progress-indicator-regex ()
  "Return regex for matching progress indicators."
  "^\\(\\*\\{2,3\\}\\) \\(.+?\\) \\[\\(/\\|[0-9]+/[0-9]+\\)\\]")

(defun org-asana--update-progress-indicators ()
  "Update progress indicators for all headings."
  (when org-asana-show-progress-indicators
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward (org-asana--progress-indicator-regex) nil t)
          (org-asana--update-single-progress-indicator))))))

;;; Face Application Functions

(defun org-asana--apply-task-faces ()
  "Apply faces to tasks based on priority and deadlines."
  (save-excursion
    (org-map-entries
     #'org-asana--apply-face-to-current-task
     nil 'file)))

(defun org-asana--apply-face-to-current-task ()
  "Apply appropriate face to current task."
  (let* ((deadline (org-get-deadline-time (point)))
         (priority (org-entry-get (point) "PRIORITY"))
         (tags (org-entry-get (point) "ASANA-TAGS"))
         (face (org-asana--determine-task-face deadline priority tags)))
    (when face
      (org-asana--apply-face-to-heading face))))

(defun org-asana--determine-task-face (deadline priority tags)
  "Determine face based on DEADLINE, PRIORITY and TAGS."
  (ignore tags)  ; Reserved for future use
  (cond
   ((and deadline (time-less-p deadline (current-time)))
    'org-asana-deadline-overdue)
   ((and deadline (time-less-p deadline (time-add (current-time) (* 3 86400))))
    'org-asana-deadline-warning)
   ((and priority (string= priority "A"))
    'org-asana-priority-high)
   ((and priority (string= priority "B"))
    'org-asana-priority-medium)
   (t nil)))

(defun org-asana--apply-face-to-heading (face)
  "Apply FACE to current heading."
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (add-text-properties beg end `(face ,face))))

(defcustom org-asana-apply-faces t
  "Whether to apply faces to tasks."
  :type 'boolean
  :group 'org-asana)

;;; Sync Orchestration

(defun org-asana--sync-from-asana ()
  "Main sync function - fetch, transform, and render."
  (message "Starting Asana sync...")
  (let* ((tasks-raw (org-asana--fetch-all-tasks))
         (tasks (org-asana--ensure-list tasks-raw))
         (task-count (length tasks)))
    (message "Fetched %d tasks, populating data structures..." task-count)
    (org-asana--populate-from-tasks tasks)
    (let* ((metadata-map (when org-asana-fetch-metadata
                          (org-asana--fetch-all-metadata tasks)))
           (org-tree (org-asana--build-org-tree-from-tables metadata-map))
           (buffer (org-asana--prepare-buffer)))
      (org-asana--render-org-tree org-tree buffer)
      (with-current-buffer buffer
        (org-asana--update-progress-indicators)
        (when org-asana-apply-faces
          (org-asana--apply-task-faces))
        (save-buffer))
      (message "Sync complete. %d tasks synchronized." task-count))))

(defun org-asana--prepare-buffer ()
  "Prepare or create the org buffer for Asana tasks."
  (let ((buffer (find-file-noselect org-asana-org-file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun org-asana--extract-task-gids (tasks)
  "Extract GIDs from TASKS list."
  (mapcar (lambda (task) (alist-get 'gid task))
          (org-asana--ensure-list tasks)))

(defun org-asana--calculate-batch-count (total batch-size)
  "Calculate number of batches for TOTAL items with BATCH-SIZE."
  (ceiling (/ (float total) batch-size)))

(defun org-asana--process-metadata-batch-results (batch-gids batch-results metadata)
  "Process BATCH-RESULTS for BATCH-GIDS and store in METADATA."
  (let ((result-index 0))
    (dolist (task-gid batch-gids)
      (let ((stories-result (nth result-index batch-results))
            (attachments-result (nth (1+ result-index) batch-results)))
        (when (and stories-result attachments-result)
          (puthash task-gid
                   (cons (alist-get 'data (alist-get 'body stories-result))
                         (alist-get 'data (alist-get 'body attachments-result)))
                   metadata))
        (setq result-index (+ result-index 2))))))

(defun org-asana--fetch-metadata-for-batch (batch-gids batch-num total-batches metadata)
  "Fetch metadata for BATCH-GIDS (batch BATCH-NUM of TOTAL-BATCHES) into METADATA."
  (message "Processing batch %d/%d..." batch-num total-batches)
  (let ((batch-results (alist-get 'data (org-asana--fetch-metadata-batch batch-gids))))
    (org-asana--process-metadata-batch-results batch-gids batch-results metadata)))

(defun org-asana--fetch-all-metadata (tasks)
  "Fetch metadata for all TASKS and return as hash table."
  (let ((metadata (make-hash-table :test 'equal))
        (task-gids (org-asana--extract-task-gids tasks))
        (batch-size 5))
    (if task-gids
        (let ((total-batches (org-asana--calculate-batch-count 
                             (length task-gids) batch-size))
              (batch-num 0))
          (message "Fetching metadata for %d tasks using batch API..." 
                   (length task-gids))
          (while task-gids
            (let ((batch-gids (seq-take task-gids batch-size)))
              (cl-incf batch-num)
              (org-asana--fetch-metadata-for-batch 
               batch-gids batch-num total-batches metadata)
              (setq task-gids (seq-drop task-gids batch-size)))))
      (message "No tasks to fetch metadata for."))
    metadata))

;;; Utility Functions

(defun org-asana--plist-to-alist (plist)
  "Convert PLIST to alist format."
  (let ((result '()))
    (while plist
      (push (cons (substring (symbol-name (car plist)) 1)
                  (cadr plist))
            result)
      (setq plist (cddr plist)))
    (nreverse result)))

(defun org-asana--format-notes-section (notes)
  "Format NOTES section for task body."
  (when (and notes (not (string-empty-p notes)))
    notes))

(defun org-asana--format-single-attachment (attachment)
  "Format a single ATTACHMENT."
  (let ((name (alist-get 'name attachment))
        (url (alist-get 'view_url attachment)))
    (format "- [[%s][%s]]" url name)))

(defun org-asana--format-attachments-section (attachments)
  "Format ATTACHMENTS section for task body."
  (when (and attachments (> (length attachments) 0))
    (concat "\n***** Attachments\n"
            (mapconcat #'org-asana--format-single-attachment
                      attachments "\n"))))

(defun org-asana--format-single-comment (story)
  "Format a single STORY comment."
  (when (equal (alist-get 'type story) "comment")
    (let ((text (alist-get 'text story))
          (author (alist-get 'name (alist-get 'created_by story)))
          (date (org-asana--format-timestamp (alist-get 'created_at story)))
          (gid (alist-get 'gid story)))
      (format "[[asana-comment:%s][%s]] %s: %s" gid date author text))))

(defun org-asana--format-comments-section (stories)
  "Format STORIES as comments section."
  (when (and stories org-asana-fetch-metadata (> (length stories) 0))
    (let ((comments (delq nil (mapcar #'org-asana--format-single-comment stories))))
      (when comments
        (concat "\n:COMMENTS:\n"
                (mapconcat #'identity comments "\n")
                "\n:END:")))))

(defun org-asana--format-task-body (props metadata)
  "Format task body from PROPS and METADATA."
  (let ((notes (org-asana--format-notes-section (plist-get props :notes)))
        (attachments (org-asana--format-attachments-section (cdr metadata)))
        (comments (org-asana--format-comments-section (car metadata))))
    (mapconcat #'identity
               (delq nil (list notes attachments comments))
               "\n")))

(defun org-asana--get-token-from-authinfo ()
  "Retrieve Asana token from authinfo."
  (require 'auth-source)
  (let ((auth (car (auth-source-search :host org-asana-authinfo-machine
                                       :require '(:secret)
                                       :max 1))))
    (when auth
      (let ((secret (plist-get auth :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun org-asana--get-token ()
  "Get Asana token from variable or authinfo."
  (or org-asana-token
      (org-asana--get-token-from-authinfo)
      (signal 'org-asana-auth-error 
              '("No Asana token found. Set `org-asana-token' or add to authinfo"))))

(defun org-asana--validate-token ()
  "Validate the Asana token is configured."
  (org-asana--get-token))

(defun org-asana--ensure-file-exists ()
  "Ensure the org file exists or create it."
  (unless (file-exists-p org-asana-org-file)
    (make-directory (file-name-directory org-asana-org-file) t)
    (write-region "" nil org-asana-org-file)))

;;; Interactive Commands

;;;###autoload
(defun org-asana-add-comment ()
  "Add a comment to the current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((comment-text (read-string "Comment: ")))
      (when (and comment-text (not (string-empty-p comment-text)))
        (org-asana--add-comment-to-drawer comment-text)
        (message "Comment added. Run sync to send to Asana.")))))

(defun org-asana--add-comment-to-drawer (comment-text)
  "Add COMMENT-TEXT to current task's COMMENTS drawer."
  (save-excursion
    (org-back-to-heading t)
    (let ((has-drawer (org-asana--find-comments-drawer)))
      (if has-drawer
        (org-asana--append-to-comments-drawer comment-text)
        (org-asana--create-comments-drawer comment-text)))))

(defun org-asana--append-to-comments-drawer (comment-text)
  "Append COMMENT-TEXT to existing COMMENTS drawer."
  (re-search-forward "^:END:$" nil t)
  (beginning-of-line)
  (insert (format "%s: %s\n" 
                 (format-time-string "[%Y-%m-%d %a %H:%M]")
                 comment-text)))

(defun org-asana--create-comments-drawer (comment-text)
  "Create COMMENTS drawer with COMMENT-TEXT."
  (org-end-of-meta-data t)
  (insert ":COMMENTS:\n")
  (insert (format "%s: %s\n"
                 (format-time-string "[%Y-%m-%d %a %H:%M]")
                 comment-text))
  (insert ":END:\n"))

;;;###autoload
(defun org-asana-sync ()
  "Sync tasks between Org and Asana."
  (interactive)
  (org-asana--ensure-initialized)
  (org-asana--validate-token)
  (org-asana--ensure-file-exists)
  (condition-case err
      (org-asana--sync-bidirectional)
    (org-asana-auth-error
     (message "Authentication failed: %s" (error-message-string err)))
    (org-asana-rate-limit-error
     (message "Rate limit exceeded: %s" (error-message-string err)))
    (org-asana-api-error
     (message "API error: %s" (error-message-string err)))
    (org-asana-sync-error
     (message "Sync error: %s" (error-message-string err)))
    (error
     (message "Unexpected error: %s" (error-message-string err)))))

;;;###autoload
(defun org-asana-test-connection ()
  "Test the Asana API connection."
  (interactive)
  (org-asana--ensure-initialized)
  (org-asana--validate-token)
  (condition-case err
      (let ((user-info (org-asana--fetch-workspace-info)))
        (message "Connection successful! User GID: %s, Workspace GID: %s"
                (car user-info) (cadr user-info)))
    (org-asana-auth-error
     (message "Authentication failed: %s" (error-message-string err)))
    (org-asana-api-error
     (message "API error: %s" (error-message-string err)))
    (error
     (message "Connection failed: %s" (error-message-string err)))))

;;;###autoload
(defun org-asana-initialize ()
  "Initialize org-asana with interactive setup."
  (interactive)
  (org-asana--ensure-initialized)
  (let ((token (read-string "Enter your Asana Personal Access Token: ")))
    (customize-save-variable 'org-asana-token token)
    (let ((file (read-file-name "Org file for Asana tasks: "
                               "~/org/" nil nil "asana.org")))
      (customize-save-variable 'org-asana-org-file file))
    (org-asana--ensure-file-exists)
    (message "org-asana initialized. Run `org-asana-sync' to fetch tasks.")))

;;; Hooks and Buffer Management

(defun org-asana--save-hook ()
  "Hook function to sync on buffer save."
  (when (and org-asana-enable-save-hook
             (string= (buffer-file-name) (expand-file-name org-asana-org-file))
             org-asana--initialized)
    (org-asana--sync-bidirectional)))

(defun org-asana--todo-state-hook (change)
  "Hook function to sync when TODO state CHANGE occurs."
  (when (and org-asana-enable-todo-hook
             (string= (buffer-file-name) (expand-file-name org-asana-org-file))
             org-asana--initialized
             (eq (plist-get change :type) 'todo-state-change))
    (save-buffer)
    (org-asana--sync-bidirectional)))

(defun org-asana--setup-buffer-hooks ()
  "Setup buffer-local hooks for org-asana file."
  (when (string= (buffer-file-name) (expand-file-name org-asana-org-file))
    (add-hook 'after-save-hook #'org-asana--save-hook nil t)
    (add-hook 'org-trigger-hook #'org-asana--todo-state-hook nil t)))

(defun org-asana--remove-buffer-hooks ()
  "Remove buffer-local hooks from org-asana file."
  (remove-hook 'after-save-hook #'org-asana--save-hook t)
  (remove-hook 'org-trigger-hook #'org-asana--todo-state-hook t))

;;; Org-Agenda Integration

(defun org-asana-enable-agenda-integration ()
  "Add org-asana file to org-agenda-files."
  (interactive)
  (when org-asana-org-file
    (require 'org-agenda)
    (let ((file (expand-file-name org-asana-org-file)))
      (unless (member file org-agenda-files)
        (add-to-list 'org-agenda-files file))
      (message "Added %s to org-agenda-files" file))))

(defun org-asana-disable-agenda-integration ()
  "Remove org-asana file from org-agenda-files."
  (interactive)
  (when org-asana-org-file
    (require 'org-agenda)
    (let ((file (expand-file-name org-asana-org-file)))
      (setq org-agenda-files (delete file org-agenda-files))
      (message "Removed %s from org-agenda-files" file))))

(defun org-asana--setup-agenda-commands ()
  "Setup custom org-agenda commands for Asana tasks."
  (require 'org-agenda)
  (defvar org-agenda-custom-commands) ; Silence compiler
  (add-to-list 'org-agenda-custom-commands
               '("A" . "Asana tasks"))
  (add-to-list 'org-agenda-custom-commands
               '("At" "Asana tasks today"
                 ((agenda "" ((org-agenda-span 'day)
                            (org-agenda-files (list org-asana-org-file)))))))
  (add-to-list 'org-agenda-custom-commands
               '("Aw" "Asana tasks this week"
                 ((agenda "" ((org-agenda-span 'week)
                            (org-agenda-files (list org-asana-org-file)))))))
  (add-to-list 'org-agenda-custom-commands
               '("Ad" "Asana tasks with deadlines"
                 ((tags-todo "DEADLINE<=\"<+7d>\""
                           ((org-agenda-files (list org-asana-org-file))))))))

;;; Initialization

(defun org-asana--ensure-initialized ()
  "Ensure org-asana is properly initialized."
  (unless org-asana--initialized
    (org-asana--load-modules)
    (org-asana--clear-all-tables)
    (org-asana--create-org-data-tables)
    (when org-asana-enable-agenda-integration
      (org-asana-enable-agenda-integration)
      (org-asana--setup-agenda-commands))
    (add-hook 'find-file-hook #'org-asana--setup-buffer-hooks)
    (setq org-asana--initialized t)))

(defun org-asana-reset ()
  "Reset org-asana state and hooks."
  (interactive)
  (org-asana--clear-all-tables)
  (org-asana--create-org-data-tables)
  (remove-hook 'find-file-hook #'org-asana--setup-buffer-hooks)
  (when (get-file-buffer org-asana-org-file)
    (with-current-buffer (get-file-buffer org-asana-org-file)
      (org-asana--remove-buffer-hooks)))
  (setq org-asana--initialized nil)
  (message "org-asana reset complete"))

;;; Module-Specific Commands

(defun org-asana-browse ()
  "Open the Asana task browser."
  (interactive)
  (if (and (boundp 'org-asana-modules)
           (memq 'org-asana-browser org-asana-modules)
           (fboundp 'org-asana-browse-workspace))
      (call-interactively 'org-asana-browse-workspace)
    (error "Task browser module not loaded. Add org-asana-browser to org-asana-modules")))

(defun org-asana-attach ()
  "Attach a file to the current task."
  (interactive)
  (if (and (boundp 'org-asana-modules)
           (memq 'org-asana-attachments org-asana-modules)
           (fboundp 'org-asana-attach-file))
      (call-interactively 'org-asana-attach-file)
    (error "Attachments module not loaded. Add org-asana-attachments to org-asana-modules")))

(defun org-asana-assign ()
  "Assign the current task to a user."
  (interactive)
  (if (and (boundp 'org-asana-modules)
           (memq 'org-asana-users org-asana-modules)
           (fboundp 'org-asana-assign-task))
      (call-interactively 'org-asana-assign-task)
    (error "Users module not loaded. Add org-asana-users to org-asana-modules")))

(defun org-asana-show-dependencies ()
  "Show dependencies for the current task."
  (interactive)
  (if (and (boundp 'org-asana-modules)
           (memq 'org-asana-dependencies org-asana-modules)
           (fboundp 'org-asana-show-task-dependencies))
      (call-interactively 'org-asana-show-task-dependencies)
    (error "Dependencies module not loaded. Add org-asana-dependencies to org-asana-modules")))

(defun org-asana-cache-status ()
  "Show cache statistics."
  (interactive)
  (if (and (boundp 'org-asana-modules)
           (memq 'org-asana-cache org-asana-modules)
           (fboundp 'org-asana-cache-statistics))
      (call-interactively 'org-asana-cache-statistics)
    (error "Cache module not loaded. Add org-asana-cache to org-asana-modules")))

(provide 'org-asana)
;;; org-asana.el ends here
