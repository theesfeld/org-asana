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

;;; Data Transformation Functions

(defun org-asana--group-tasks-by-project (tasks)
  "Group TASKS by their project membership."
  (let ((projects (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
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
        (result '()))
    (dolist (task tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
             (project (alist-get 'project membership))
             (project-gid (alist-get 'gid project)))
        (when (and project-gid (not (gethash project-gid projects)))
          (puthash project-gid t projects)
          (push project result))))
    (nreverse result)))

(defun org-asana--extract-sections-for-project (tasks project-gid)
  "Extract unique sections for PROJECT-GID from TASKS."
  (let ((sections (make-hash-table :test 'equal))
        (result '()))
    (dolist (task tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
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
    (replace-regexp-in-string "\\*" "\\\\*" text)))

(defun org-asana--task-to-properties (task metadata)
  "Convert TASK and METADATA to property list."
  (let ((stories (car metadata))
        (attachments (cdr metadata)))
    (list :gid (alist-get 'gid task)
          :name (org-asana--sanitize-text (alist-get 'name task))
          :notes (org-asana--sanitize-text (alist-get 'notes task))
          :due-on (org-asana--format-date (alist-get 'due_on task))
          :created-at (org-asana--format-timestamp (alist-get 'created_at task))
          :modified-at (org-asana--format-timestamp (alist-get 'modified_at task))
          :created-by (alist-get 'name (alist-get 'created_by task))
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
             (section-tasks (org-asana--get-tasks-for-section tasks project-gid section-gid)))
        (push (list :type 'section
                    :gid section-gid
                    :name section-name
                    :children section-tasks)
              sections)))
    (nreverse sections)))

(defun org-asana--get-tasks-for-section (tasks project-gid section-gid)
  "Get tasks for PROJECT-GID and SECTION-GID from TASKS."
  (let ((section-tasks '()))
    (dolist (task tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
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
              :properties `(("ASANA-TASK-GID" . ,(plist-get props :gid))
                           ("ASANA-CREATED-AT" . ,(plist-get props :created-at))
                           ("ASANA-MODIFIED-AT" . ,(plist-get props :modified-at)))
              :deadline (plist-get props :due-on)
              :body (plist-get props :notes)))))))

(defun org-asana--build-org-tree (tasks)
  "Build complete org tree structure from TASKS."
  (let ((project-tree (org-asana--build-project-tree tasks)))
    (mapcar (lambda (project)
             (org-asana--create-org-node project 2))
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
        (properties (plist-get node :properties))
        (deadline (plist-get node :deadline))
        (body (plist-get node :body))
        (children (plist-get node :children)))
    (org-asana--insert-heading level title)
    (when deadline
      (org-asana--insert-deadline deadline))
    (when properties
      (org-asana--insert-properties properties))
    (when body
      (org-asana--insert-body body))
    (dolist (child children)
      (org-asana--render-node child))))

(defun org-asana--insert-heading (level title)
  "Insert heading at LEVEL with TITLE."
  (insert (make-string level ?*) " " title)
  (when (or (= level 2) (= level 3))
    (insert " [/]"))
  (insert "\n"))

(defun org-asana--insert-deadline (deadline)
  "Insert DEADLINE on current heading."
  (when deadline
    (insert "DEADLINE: " deadline "\n")))

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
      (while (re-search-forward "^\\*\\*+ .+\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" nil t)
        (let ((done 0)
              (total 0))
          (save-excursion
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (while (re-search-forward "^\\*\\{4,\\} \\(TODO\\|DONE\\)" nil t)
              (setq total (1+ total))
              (when (string= (match-string 1) "DONE")
                (setq done (1+ done))))
            (widen))
          (replace-match (format "[%d/%d]" done total) nil nil nil 0))))))

(provide 'org-asana)
;;; org-asana.el ends here