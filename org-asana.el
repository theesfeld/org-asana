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

(defun org-asana--fetch-all-tasks ()
  "Fetch all incomplete tasks assigned to current user."
  (let* ((workspace-info (org-asana--fetch-workspace-info))
         (user-gid (car workspace-info))
         (workspace-gid (cadr workspace-info))
         (opt-fields (concat "gid,name,notes,html_notes,completed,completed_at,"
                            "due_on,due_at,start_on,start_at,"
                            "created_at,modified_at,created_by.name,"
                            "assignee.name,assignee_status,"
                            "followers.name,num_likes,liked,"
                            "parent.gid,parent.name,num_subtasks,"
                            "tags.gid,tags.name,"
                            "memberships.project.gid,memberships.project.name,"
                            "memberships.section.gid,memberships.section.name,"
                            "permalink_url,resource_subtype"))
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

;;; Data Transformation Functions

(defun org-asana--group-tasks-by-project (tasks)
  "Group TASKS by their project membership."
  (let ((projects (make-hash-table :test 'equal))
        (task-list (if (vectorp tasks) (append tasks nil) tasks)))
    (dolist (task task-list)
      (let* ((memberships (alist-get 'memberships task))
             (membership-list (if (vectorp memberships) 
                                 (append memberships nil) 
                               memberships))
             (membership (car membership-list))
             (project (alist-get 'project membership))
             (project-gid (alist-get 'gid project)))
        (when project-gid
          (let ((project-tasks (gethash project-gid projects)))
            (puthash project-gid (cons task project-tasks) projects)))))
    projects))

(defun org-asana--group-tasks-by-section (project-tasks)
  "Group PROJECT-TASKS by their section."
  (let ((sections (make-hash-table :test 'equal)))
    (dolist (task project-tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
             (section (alist-get 'section membership))
             (section-gid (alist-get 'gid section)))
        (when section-gid
          (let ((section-tasks (gethash section-gid sections)))
            (puthash section-gid (cons task section-tasks) sections)))))
    sections))

(defun org-asana--extract-unique-projects (tasks)
  "Extract unique project data from TASKS."
  (let ((projects (make-hash-table :test 'equal))
        (result '())
        (task-list (if (vectorp tasks) (append tasks nil) tasks)))
    (dolist (task task-list)
      (let* ((memberships (alist-get 'memberships task))
             (membership-list (if (vectorp memberships)
                                 (append memberships nil)
                               memberships))
             (membership (car membership-list))
             (project (alist-get 'project membership))
             (project-gid (alist-get 'gid project)))
        (when (and project-gid (not (gethash project-gid projects)))
          (puthash project-gid t projects)
          (push project result))))
    (nreverse result)))

(defun org-asana--extract-sections-for-project (tasks project-gid)
  "Extract unique sections for PROJECT-GID from TASKS."
  (let ((sections (make-hash-table :test 'equal))
        (result '())
        (task-list (if (vectorp tasks) (append tasks nil) tasks)))
    (dolist (task task-list)
      (let* ((memberships (alist-get 'memberships task))
             (membership-list (if (vectorp memberships)
                                 (append memberships nil)
                               memberships))
             (membership (car membership-list))
             (project (alist-get 'project membership))
             (section (alist-get 'section membership))
             (task-project-gid (alist-get 'gid project))
             (section-gid (alist-get 'gid section)))
        (when (and (equal task-project-gid project-gid)
                   section-gid
                   (not (gethash section-gid sections)))
          (puthash section-gid t sections)
          (push section result))))
    (nreverse result)))

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
  (when text
    (let ((sanitized text))
      (setq sanitized (replace-regexp-in-string "\\*" "\\\\*" sanitized))
      (setq sanitized (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" sanitized))
      sanitized)))

(defun org-asana--clean-notes (notes)
  "Clean NOTES field from org-mode corruption."
  (when notes
    (let ((cleaned notes))
      ;; Remove org properties blocks
      (setq cleaned (replace-regexp-in-string ":PROPERTIES:[^:]*:END:[\n\r]*" "" cleaned))
      ;; Remove TODO keywords
      (setq cleaned (replace-regexp-in-string "^\\(?:TODO\\|DONE\\|ODO\\) " "" cleaned))
      ;; Remove deadline lines
      (setq cleaned (replace-regexp-in-string "^DEADLINE: <[^>]+>[\n\r]*" "" cleaned))
      ;; Extract text from org links
      (setq cleaned (replace-regexp-in-string "\\[\\[https?://[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" cleaned))
      ;; Remove broken links
      (setq cleaned (replace-regexp-in-string "/[0-9]+/[0-9]+/[^]]*\\]\\[\\([^]]+\\)\\]\\]" "\\1" cleaned))
      ;; Remove org headings
      (setq cleaned (replace-regexp-in-string "^\\*+ " "" cleaned))
      ;; Remove property lines
      (setq cleaned (replace-regexp-in-string "^:[A-Z_]+: .*[\n\r]*" "" cleaned))
      ;; Clean up whitespace
      (setq cleaned (replace-regexp-in-string "[\n\r]+" "\n" cleaned))
      (setq cleaned (string-trim cleaned))
      (if (string-empty-p cleaned) nil cleaned))))

(defun org-asana--task-to-properties (task metadata)
  "Convert TASK and METADATA to property list."
  (let ((stories (car metadata))
        (attachments (cdr metadata))
        (followers (alist-get 'followers task)))
    (list :gid (alist-get 'gid task)
          :name (org-asana--sanitize-text (alist-get 'name task))
          :notes (org-asana--clean-notes (alist-get 'notes task))
          :due-on (org-asana--format-date (alist-get 'due_on task))
          :start-on (org-asana--format-date (alist-get 'start_on task))
          :created-at (org-asana--format-timestamp (alist-get 'created_at task))
          :modified-at (org-asana--format-timestamp (alist-get 'modified_at task))
          :created-by (alist-get 'name (alist-get 'created_by task))
          :assignee (alist-get 'name (alist-get 'assignee task))
          :assignee-status (alist-get 'assignee_status task)
          :followers (mapcar (lambda (f) (alist-get 'name f)) 
                            (if (vectorp followers) (append followers nil) followers))
          :num-likes (alist-get 'num_likes task)
          :tags (mapcar (lambda (tag) (alist-get 'name tag))
                       (let ((tags (alist-get 'tags task)))
                         (if (vectorp tags) (append tags nil) tags)))
          :permalink (alist-get 'permalink_url task)
          :parent (alist-get 'name (alist-get 'parent task))
          :num-subtasks (alist-get 'num_subtasks task)
          :stories stories
          :attachments attachments)))

;;; Org Tree Building Functions

(defun org-asana--build-project-tree (tasks)
  "Build hierarchical tree structure from flat TASKS list."
  (let ((tree '()))
    (dolist (project (org-asana--extract-unique-projects tasks))
      (let* ((project-gid (alist-get 'gid project))
             (project-name (alist-get 'name project))
             (project-sections (org-asana--build-sections-tree tasks project-gid)))
        (push (list :type 'project
                    :gid project-gid
                    :name project-name
                    :children project-sections)
              tree)))
    (nreverse tree)))

(defun org-asana--build-sections-tree (tasks project-gid)
  "Build sections tree for PROJECT-GID from TASKS."
  (let ((sections '()))
    (dolist (section (org-asana--extract-sections-for-project tasks project-gid))
      (let* ((section-gid (alist-get 'gid section))
             (section-name (alist-get 'name section))
             (section-tasks (org-asana--sort-tasks-by-created-date 
                           (org-asana--get-tasks-for-section tasks project-gid section-gid))))
        (push (list :type 'section
                    :gid section-gid
                    :name section-name
                    :children section-tasks
                    :oldest-task-date (org-asana--get-oldest-task-date section-tasks))
              sections)))
    ;; Sort sections by oldest task date
    (sort sections
          (lambda (a b)
            (let ((date-a (plist-get a :oldest-task-date))
                  (date-b (plist-get b :oldest-task-date)))
              (string< (or date-a "") (or date-b "")))))))

(defun org-asana--get-oldest-task-date (task-items)
  "Get the oldest created_at date from TASK-ITEMS."
  (let ((oldest-date nil))
    (dolist (item task-items)
      (let* ((task (plist-get item :data))
             (date (alist-get 'created_at task)))
        (when (and date (or (null oldest-date) (string< date oldest-date)))
          (setq oldest-date date))))
    oldest-date))

(defun org-asana--sort-tasks-by-created-date (task-items)
  "Sort TASK-ITEMS by creation date, oldest first."
  (sort task-items
        (lambda (a b)
          (let* ((task-a (plist-get a :data))
                 (task-b (plist-get b :data))
                 (date-a (alist-get 'created_at task-a))
                 (date-b (alist-get 'created_at task-b)))
            (string< (or date-a "") (or date-b ""))))))

(defun org-asana--get-tasks-for-section (tasks project-gid section-gid)
  "Get tasks for PROJECT-GID and SECTION-GID from TASKS."
  (let ((section-tasks '())
        (task-list (if (vectorp tasks) (append tasks nil) tasks)))
    (dolist (task task-list)
      (let* ((memberships (alist-get 'memberships task))
             (membership-list (if (vectorp memberships)
                                 (append memberships nil)
                               memberships))
             (membership (car membership-list))
             (project (alist-get 'project membership))
             (section (alist-get 'section membership))
             (task-project-gid (alist-get 'gid project))
             (task-section-gid (alist-get 'gid section)))
        (when (and (equal task-project-gid project-gid)
                   (equal task-section-gid section-gid))
          (push (list :type 'task
                      :gid (alist-get 'gid task)
                      :name (alist-get 'name task)
                      :data task)
                section-tasks))))
    (nreverse section-tasks)))

(defun org-asana--create-org-node (item level)
  "Create org node from ITEM at LEVEL."
  (let ((type (plist-get item :type)))
    (cond
     ((eq type 'project)
      (list :level level
            :title (plist-get item :name)
            :properties `(("ASANA-PROJECT-GID" . ,(plist-get item :gid)))
            :children (mapcar (lambda (child)
                               (org-asana--create-org-node child (1+ level)))
                             (plist-get item :children))))
     ((eq type 'section)
      (list :level level
            :title (plist-get item :name)
            :properties `(("ASANA-SECTION-GID" . ,(plist-get item :gid)))
            :children (mapcar (lambda (child)
                               (org-asana--create-org-node child (1+ level)))
                             (plist-get item :children))))
     ((eq type 'task)
      (let* ((task-data (plist-get item :data))
             (props (org-asana--task-to-properties task-data nil)))
        (list :level level
              :title (plist-get props :name)
              :todo-keyword "TODO"
              :properties `(("ASANA-TASK-GID" . ,(plist-get props :gid))
                           ("ASANA-CREATED-AT" . ,(plist-get props :created-at))
                           ("ASANA-MODIFIED-AT" . ,(plist-get props :modified-at))
                           ("ASANA-CREATED-BY" . ,(plist-get props :created-by))
                           ("ASANA-ASSIGNEE" . ,(plist-get props :assignee))
                           ("ASANA-PERMALINK" . ,(plist-get props :permalink)))
              :deadline (plist-get props :due-on)
              :scheduled (plist-get props :start-on)
              :body (plist-get props :notes)))))))

(defun org-asana--build-org-tree (tasks)
  "Build complete org tree structure from TASKS."
  (let ((project-tree (org-asana--build-project-tree tasks)))
    (mapcar (lambda (project)
             (org-asana--create-org-node project 1))
           project-tree)))

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

(defun org-asana--render-node (node)
  "Render NODE to current buffer."
  (let ((level (plist-get node :level))
        (title (plist-get node :title))
        (todo-keyword (plist-get node :todo-keyword))
        (properties (plist-get node :properties))
        (deadline (plist-get node :deadline))
        (scheduled (plist-get node :scheduled))
        (body (plist-get node :body))
        (children (plist-get node :children)))
    (org-asana--insert-heading level title todo-keyword)
    (when deadline
      (org-asana--insert-deadline deadline))
    (when scheduled
      (org-asana--insert-scheduled scheduled))
    (when properties
      (org-asana--insert-properties properties))
    (when body
      (org-asana--insert-body body))
    (dolist (child children)
      (org-asana--render-node child))))

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
    (when (cdr prop)
      (insert ":" (car prop) ": " (cdr prop) "\n")))
  (insert ":END:\n"))

(defun org-asana--insert-body (body)
  "Insert BODY text."
  (when (and body (not (string-empty-p body)))
    (insert "\n" body "\n")))

(defun org-asana--update-progress-indicators ()
  "Update progress indicators for all headings."
  (when org-asana-show-progress-indicators
    (save-excursion
      (goto-char (point-min))
      ;; Match level 2 and 3 headings with [/] or [x/y]
      (while (re-search-forward "^\\(\\*\\{2,3\\}\\) \\(.+?\\) \\[\\(/\\|[0-9]+/[0-9]+\\)\\]" nil t)
        (let ((stars (match-string 1))
              (title (match-string 2))
              (done 0)
              (total 0))
          (save-excursion
            (org-narrow-to-subtree)
            (goto-char (point-min))
            ;; Count TODO and DONE tasks under this heading
            (while (re-search-forward "^\\*\\{4,\\} \\(TODO\\|DONE\\)" nil t)
              (setq total (1+ total))
              (when (string= (match-string 1) "DONE")
                (setq done (1+ done))))
            (widen))
          ;; Replace the entire match with updated progress
          (replace-match (format "%s %s [%d/%d]" stars title done total) t t))))))

;;; Sync Orchestration

(defun org-asana--sync-from-asana ()
  "Main sync function - fetch, transform, and render."
  (message "Starting Asana sync...")
  (let* ((tasks-raw (org-asana--fetch-all-tasks))
         (tasks (if (vectorp tasks-raw) (append tasks-raw nil) tasks-raw))
         (task-count (length tasks))
         (metadata-map (when org-asana-fetch-metadata
                        (org-asana--fetch-all-metadata tasks)))
         (org-tree (org-asana--build-org-tree-with-metadata tasks metadata-map))
         (buffer (org-asana--prepare-buffer)))
    (message "Fetched %d tasks, building org structure..." task-count)
    (org-asana--render-org-tree org-tree buffer)
    (with-current-buffer buffer
      (org-asana--update-progress-indicators)
      (save-buffer))
    (message "Sync complete. %d tasks synchronized." task-count)))

(defun org-asana--prepare-buffer ()
  "Prepare or create the org buffer for Asana tasks."
  (let ((buffer (find-file-noselect org-asana-org-file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun org-asana--fetch-all-metadata (tasks)
  "Fetch metadata for all TASKS and return as hash table."
  (let ((metadata (make-hash-table :test 'equal))
        (task-list (if (vectorp tasks) (append tasks nil) tasks)))
    (dolist (task task-list)
      (let ((task-gid (alist-get 'gid task)))
        (when org-asana-debug
          (message "Fetching metadata for task: %s" task-gid))
        (puthash task-gid 
                (org-asana--fetch-task-metadata task-gid)
                metadata)))
    metadata))

(defun org-asana--build-org-tree-with-metadata (tasks metadata-map)
  "Build org tree from TASKS with METADATA-MAP."
  (let ((project-tree (org-asana--build-project-tree tasks)))
    (mapcar (lambda (project)
             (org-asana--create-org-node-with-metadata 
              project 1 (or metadata-map (make-hash-table :test 'equal)) tasks))
           project-tree)))

(defun org-asana--create-org-node-with-metadata (item level metadata-map tasks)
  "Create org node from ITEM at LEVEL with METADATA-MAP and TASKS."
  (let ((type (plist-get item :type)))
    (cond
     ((eq type 'project)
      (list :level level
            :title (plist-get item :name)
            :properties `(("ASANA-PROJECT-GID" . ,(plist-get item :gid)))
            :children (mapcar (lambda (child)
                               (org-asana--create-org-node-with-metadata 
                                child (1+ level) metadata-map tasks))
                             (plist-get item :children))))
     ((eq type 'section)
      (list :level level
            :title (plist-get item :name)
            :properties `(("ASANA-SECTION-GID" . ,(plist-get item :gid)))
            :children (mapcar (lambda (child)
                               (org-asana--create-org-node-with-metadata 
                                child (1+ level) metadata-map tasks))
                             (plist-get item :children))))
     ((eq type 'task)
      (let* ((task-data (plist-get item :data))
             (task-gid (alist-get 'gid task-data))
             (metadata (gethash task-gid metadata-map))
             (props (org-asana--task-to-properties task-data metadata))
             (followers (plist-get props :followers))
             (tags (plist-get props :tags)))
        (list :level level
              :title (plist-get props :name)
              :todo-keyword "TODO"
              :properties `(("ASANA-TASK-GID" . ,(plist-get props :gid))
                           ("ASANA-CREATED-AT" . ,(plist-get props :created-at))
                           ("ASANA-MODIFIED-AT" . ,(plist-get props :modified-at))
                           ("ASANA-CREATED-BY" . ,(plist-get props :created-by))
                           ("ASANA-ASSIGNEE" . ,(plist-get props :assignee))
                           ("ASANA-STATUS" . ,(plist-get props :assignee-status))
                           ("ASANA-FOLLOWERS" . ,(when followers (string-join followers ", ")))
                           ("ASANA-TAGS" . ,(when tags (string-join tags ", ")))
                           ("ASANA-LIKES" . ,(format "%d" (or (plist-get props :num-likes) 0)))
                           ("ASANA-PERMALINK" . ,(plist-get props :permalink)))
              :deadline (plist-get props :due-on)
              :scheduled (plist-get props :start-on)
              :body (org-asana--format-task-body props metadata)))))))

;;; Utility Functions

(defun org-asana--format-task-body (props metadata)
  "Format task body from PROPS and METADATA."
  (let ((notes (plist-get props :notes))
        (stories (car metadata))
        (attachments (cdr metadata))
        (body-parts '()))
    (when (and notes (not (string-empty-p notes)))
      (push notes body-parts))
    (when (and attachments (> (length attachments) 0))
      (push "\n***** Attachments" body-parts)
      (dolist (att attachments)
        (let ((name (alist-get 'name att))
              (url (alist-get 'view_url att)))
          (push (format "- [[%s][%s]]" url name) body-parts))))
    (when (and stories org-asana-fetch-metadata (> (length stories) 0))
      (push "\n***** Comments" body-parts)
      (dolist (story stories)
        (when (equal (alist-get 'type story) "comment")
          (let ((text (alist-get 'text story))
                (author (alist-get 'name (alist-get 'created_by story)))
                (date (org-asana--format-timestamp (alist-get 'created_at story))))
            (push (format "- %s (%s): %s" author date text) body-parts)))))
    (when body-parts
      (string-join (nreverse body-parts) "\n"))))

(defun org-asana--validate-token ()
  "Validate the Asana token is configured."
  (unless org-asana-token
    (error "Asana token not configured. Set `org-asana-token'")))

(defun org-asana--ensure-file-exists ()
  "Ensure the org file exists or create it."
  (unless (file-exists-p org-asana-org-file)
    (make-directory (file-name-directory org-asana-org-file) t)
    (write-region "" nil org-asana-org-file)))

;;; Interactive Commands

;;;###autoload
(defun org-asana-sync ()
  "Sync tasks between Org and Asana."
  (interactive)
  (org-asana--validate-token)
  (org-asana--ensure-file-exists)
  (condition-case err
      (org-asana--sync-from-asana)
    (error
     (message "Sync failed: %s" (error-message-string err)))))

;;;###autoload
(defun org-asana-test-connection ()
  "Test the Asana API connection."
  (interactive)
  (org-asana--validate-token)
  (condition-case err
      (let ((user-info (org-asana--fetch-workspace-info)))
        (message "Connection successful! User GID: %s, Workspace GID: %s" 
                (car user-info) (cadr user-info)))
    (error
     (message "Connection failed: %s" (error-message-string err)))))

;;;###autoload
(defun org-asana-initialize ()
  "Initialize org-asana with interactive setup."
  (interactive)
  (let ((token (read-string "Enter your Asana Personal Access Token: ")))
    (customize-save-variable 'org-asana-token token)
    (let ((file (read-file-name "Org file for Asana tasks: " 
                               "~/org/" nil nil "asana.org")))
      (customize-save-variable 'org-asana-org-file file))
    (org-asana--ensure-file-exists)
    (message "org-asana initialized. Run `org-asana-sync' to fetch tasks.")))

(provide 'org-asana)
;;; org-asana.el ends here