;;; org-asana.el --- Simple two-way sync between Org-mode and Asana  -*- lexical-binding: t; -*-

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
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: convenience, org-mode, asana, productivity
;; URL: https://github.com/wtheesfeld/org-asana

;;; Commentary:

;; Simple bidirectional synchronization between Org-mode and Asana.
;;
;; Configuration:
;; (use-package org-asana
;;   :config
;;   (setq org-asana-token "your-personal-access-token"
;;         org-asana-org-file "~/org/asana.org"))
;;
;; Usage:
;; - M-x org-asana-test-connection - Test API connection
;; - M-x org-asana-sync - Sync tasks between Org and Asana

;;; Code:

(require 'org)
(require 'url)
(require 'json)

;;; Custom Faces

(defface org-asana-overdue
  '((t :foreground "red" :weight bold))
  "Face for overdue Asana tasks."
  :group 'org-asana)

(defface org-asana-priority-high
  '((t :foreground "orange" :weight bold))
  "Face for high priority Asana tasks."
  :group 'org-asana)

(defface org-asana-priority-medium
  '((t :foreground "yellow"))
  "Face for medium priority Asana tasks."
  :group 'org-asana)

(defface org-asana-priority-low
  '((t :foreground "gray"))
  "Face for low priority Asana tasks."
  :group 'org-asana)

(defface org-asana-due-today
  '((t :foreground "green" :slant italic))
  "Face for Asana tasks due today."
  :group 'org-asana)

(defface org-asana-due-soon
  '((t :foreground "orange" :slant italic))
  "Face for Asana tasks due soon (within 3 days)."
  :group 'org-asana)

;;; Face application functions

(defun org-asana--apply-task-faces ()
  "Apply custom faces to all Asana tasks in buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (when (org-entry-get nil "asana-id")
        (org-asana--fontify-task)))))

(defun org-asana--fontify-task ()
  "Apply appropriate face to current task."
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (priority (org-entry-get nil "PRIORITY")))
    (when deadline
      (org-asana--apply-deadline-face deadline))
    (when priority
      (org-asana--apply-priority-face priority))))

(defun org-asana--apply-deadline-face (deadline)
  "Apply face based on DEADLINE."
  (let ((days-until (org-asana--days-until-deadline deadline)))
    (org-asana--apply-face-for-days days-until)))

(defun org-asana--days-until-deadline (deadline)
  "Calculate days until DEADLINE from current time."
  (let ((deadline-time (org-time-string-to-time deadline))
        (current-time (current-time)))
    (/ (float-time (time-subtract deadline-time current-time))
       86400)))

(defun org-asana--apply-face-for-days (days)
  "Apply appropriate face based on DAYS until deadline."
  (let ((face (org-asana--select-deadline-face days)))
    (when face
      (org-asana--apply-line-face face))))

(defun org-asana--select-deadline-face (days)
  "Select appropriate face for DAYS until deadline."
  (cond
   ((< days 0) 'org-asana-overdue)
   ((< days 1) 'org-asana-due-today)
   ((< days 7) 'org-asana-due-soon)))

(defun org-asana--apply-line-face (face)
  "Apply FACE to current line."
  (save-excursion
    (beginning-of-line)
    (add-text-properties (point) (line-end-position)
                        `(face ,face))))

(defun org-asana--apply-priority-face (priority)
  "Apply face based on PRIORITY."
  (save-excursion
    (beginning-of-line)
    (cond
     ((string= priority "[#A]")
      (add-text-properties (point) (line-end-position)
                          '(face org-asana-priority-high)))
     ((string= priority "[#B]")
      (add-text-properties (point) (line-end-position)
                          '(face org-asana-priority-medium)))
     ((string= priority "[#C]")
      (add-text-properties (point) (line-end-position)
                          '(face org-asana-priority-low))))))

;;; Org Agenda Integration

(defun org-asana-agenda-files ()
  "Return list containing org-asana file for agenda inclusion."
  (when (and org-asana-org-file
             (file-exists-p org-asana-org-file))
    (list org-asana-org-file)))

(defun org-asana--setup-agenda ()
  "Setup org-agenda to include Asana tasks."
  (when org-asana-org-file
    (add-to-list 'org-agenda-files org-asana-org-file t)))

(defun org-asana--remove-from-agenda ()
  "Remove Asana file from org-agenda-files."
  (when org-asana-org-file
    (setq org-agenda-files
          (delete org-asana-org-file org-agenda-files))))

(defcustom org-asana-agenda-skip-completed t
  "Skip completed Asana tasks in agenda views."
  :type 'boolean
  :group 'org-asana)

(defun org-asana-agenda-skip-function ()
  "Skip function for org-agenda to handle Asana tasks."
  (when (and org-asana-agenda-skip-completed
             (org-entry-get nil "asana-id")
             (org-entry-is-done-p))
    (org-end-of-subtree t)))

(defvar org-asana-agenda-custom-commands
  '(("A" . "Asana tasks")
    ("Aa" "All Asana tasks" tags "asana-id={.+}"
     ((org-agenda-overriding-header "All Asana Tasks")))
    ("Ad" "Asana tasks due today" tags "asana-id={.+}"
     ((org-agenda-overriding-header "Asana Tasks Due Today")
      (org-agenda-skip-function
       '(org-agenda-skip-entry-if 'notdeadline))
      (org-agenda-span 'day)))
    ("Aw" "Asana tasks this week" tags "asana-id={.+}"
     ((org-agenda-overriding-header "Asana Tasks This Week")
      (org-agenda-skip-function
       '(org-agenda-skip-entry-if 'notdeadline))
      (org-agenda-span 'week)))
    ("Ap" "Asana tasks by priority" tags "asana-id={.+}"
     ((org-agenda-overriding-header "Asana Tasks by Priority")
      (org-agenda-sorting-strategy '(priority-down deadline-up)))))
  "Custom agenda commands for Asana tasks.")

(defun org-asana--add-agenda-commands ()
  "Add Asana-specific commands to org-agenda."
  (dolist (cmd org-asana-agenda-custom-commands)
    (add-to-list 'org-agenda-custom-commands cmd t)))

;;; Configuration Variables

(defgroup org-asana nil
  "Simple integration between Org-mode and Asana."
  :group 'org
  :prefix "org-asana-")

(defcustom org-asana-token nil
  "Asana Personal Access Token for API authentication."
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Personal Access Token"))
  :group 'org-asana)

(defcustom org-asana-org-file nil
  "Org file to use for Asana task synchronization."
  :type 'file
  :group 'org-asana)

(defcustom org-asana-conflict-resolution 'asana-wins
  "Strategy for resolving conflicts between Org and Asana data.
\='newest-wins uses modification timestamps to determine winner.
\='asana-wins always prefers Asana data over Org data.
\='local-wins always prefers Org data over Asana data."
  :type '(choice (const :tag "Newest modification wins" newest-wins)
                 (const :tag "Asana always wins" asana-wins)
                 (const :tag "Local org always wins" local-wins))
  :group 'org-asana)

(defcustom org-asana-sync-tags t
  "Whether to synchronize Org tags with Asana tags."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-sync-priority t
  "Whether to synchronize Org priority with Asana priority."
  :type 'boolean
  :group 'org-asana)

;;; Constants

(defconst org-asana-api-base-url "https://app.asana.com/api/1.0"
  "Base URL for Asana API endpoints.")

;;; Rate Limiting Variables

(defvar org-asana--rate-limit-remaining 150
  "Remaining API calls in current window.")

(defvar org-asana--rate-limit-reset nil
  "Time when rate limit resets.")

(defvar org-asana--last-request-time nil
  "Time of last API request.")

;;; HTTP/API Functions

(defun org-asana--check-rate-limit ()
  "Check if API call can be made."
  (when (and org-asana--rate-limit-reset
             (time-less-p (current-time) org-asana--rate-limit-reset)
             (<= org-asana--rate-limit-remaining 0))
    (org-asana--wait-for-rate-limit))
  t)

(defun org-asana--wait-for-rate-limit ()
  "Wait until rate limit allows calls."
  (let ((wait-time (if org-asana--rate-limit-reset
                       (max 1 (float-time (time-subtract org-asana--rate-limit-reset (current-time))))
                     30)))
    (message "Rate limited. Waiting %.0f seconds..." wait-time)
    (sleep-for wait-time)))

(defun org-asana--update-rate-limit (headers)
  "Update rate limit from response HEADERS."
  (let ((remaining (cdr (assoc "x-ratelimit-remaining" headers)))
        (reset (cdr (assoc "x-ratelimit-reset" headers))))
    (when remaining
      (setq org-asana--rate-limit-remaining (string-to-number remaining)))
    (when reset
      (setq org-asana--rate-limit-reset (seconds-to-time (string-to-number reset))))))

(defun org-asana--http-headers ()
  "Return HTTP headers for Asana API requests."
  `(("Authorization" . ,(format "Bearer %s" org-asana-token))
    ("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defun org-asana--make-batch-request (actions)
  "Make batch API request with ACTIONS (max 10 per request)."
  (let ((batch-data `((data . ((actions . ,actions))))))
    (org-asana--make-request "POST" "/batch" batch-data)))

(defun org-asana--build-task-data-actions (task-ids)
  "Build batch actions for fetching comments and attachments for TASK-IDS."
  (let ((actions '()))
    (dolist (task-id task-ids)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/stories?opt_fields=text,created_at,created_by.name,type" task-id))
              (data . nil))
            actions)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/attachments?opt_fields=name,download_url,view_url" task-id))
              (data . nil))
            actions))
    (nreverse actions)))

(defun org-asana--process-batch-responses (responses task-ids)
  "Process batch RESPONSES and return alist of task-id to (comments . attachments)."
  (let ((results '())
        (response-index 0))
    (dolist (task-id task-ids)
      (let* ((comments-response (nth response-index responses))
             (attachments-response (nth (1+ response-index) responses))
             (comments (when (and comments-response
                                 (alist-get 'data comments-response))
                        (alist-get 'data comments-response)))
             (attachments (when (and attachments-response
                                    (alist-get 'data attachments-response))
                           (alist-get 'data attachments-response))))
        (push (cons task-id (cons comments attachments)) results)
        (setq response-index (+ response-index 2))))
    (nreverse results)))

(defun org-asana--fetch-task-data-batch (task-ids)
  "Fetch comments and attachments for TASK-IDS using batch API."
  (let ((all-results '()))
    ;; Process in batches of 5 tasks (10 actions per batch: 5 comments + 5 attachments)
    (while task-ids
      (let* ((batch-task-ids (seq-take task-ids 5))
             (remaining-task-ids (seq-drop task-ids 5))
             (actions (org-asana--build-task-data-actions batch-task-ids)))
        (condition-case nil
            (let* ((batch-response (org-asana--make-batch-request actions))
                   (responses (alist-get 'data batch-response))
                   (batch-results (org-asana--process-batch-responses responses batch-task-ids)))
              (setq all-results (append all-results batch-results)))
          (error nil))
        (setq task-ids remaining-task-ids)))
    all-results))

(defun org-asana--encode-json-data (data)
  "Encode DATA as JSON string with error handling."
  (condition-case nil
      (json-encode data)
    (error (error "Failed to encode JSON data for Asana API"))))

(defun org-asana--api-url (endpoint)
  "Construct full API URL for ENDPOINT."
  (concat org-asana-api-base-url endpoint))

(defun org-asana--parse-json-response (buffer)
  "Parse JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (condition-case nil
        (json-parse-buffer :object-type 'alist :array-type 'list)
      (error (error "Invalid JSON response from Asana API")))))

(defun org-asana--extract-headers (buffer)
  "Extract HTTP headers from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((headers '()))
      (while (re-search-forward "^\\([^:]+\\): \\(.+\\)$" nil t)
        (push (cons (downcase (match-string 1)) (match-string 2)) headers))
      headers)))

(defun org-asana--make-request-with-retry (method endpoint &optional data max-retries)
  "Make request with retry logic and rate limiting."
  (let ((attempt 0)
        (max-attempts (or max-retries 3))
        (result nil)
        (success nil))
    (while (and (< attempt max-attempts) (not success))
      (condition-case err
          (progn
            (org-asana--check-rate-limit)
            (setq result (org-asana--make-request-internal method endpoint data)
                  success t))
        (error
         (setq attempt (1+ attempt))
         (if (>= attempt max-attempts)
             (signal (car err) (cdr err))
           (let ((delay (org-asana--calculate-retry-delay attempt)))
             (message "Request failed, retrying in %d seconds..." delay)
             (sleep-for delay))))))
    result))

(defun org-asana--make-request-internal (method endpoint &optional data)
  "Internal request function with rate limit tracking."
  (let ((buffer (org-asana--execute-http-request method endpoint data)))
    (unwind-protect
        (org-asana--process-http-response buffer)
      (kill-buffer buffer))))

(defun org-asana--execute-http-request (method endpoint data)
  "Execute HTTP METHOD request to ENDPOINT with optional DATA."
  (let ((url (org-asana--api-url endpoint))
        (url-request-method method)
        (url-request-extra-headers (org-asana--http-headers))
        (url-request-data (when data (org-asana--encode-json-data data))))
    (url-retrieve-synchronously url)))

(defun org-asana--process-http-response (buffer)
  "Process HTTP response in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status (org-asana--extract-http-status)))
      (org-asana--handle-http-status status buffer))))

(defun org-asana--extract-http-status ()
  "Extract HTTP status code from current buffer."
  (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1))
    (error "Invalid HTTP response from Asana")))

(defun org-asana--handle-http-status (status buffer)
  "Handle HTTP STATUS code and return response from BUFFER."
  (let ((headers (org-asana--extract-headers buffer)))
    (org-asana--update-rate-limit headers)
    (cond
     ((= status 429) (org-asana--handle-rate-limit))
     ((org-asana--status-ok-p status) (org-asana--parse-json-response buffer))
     (t (error "Asana API error: HTTP %d" status)))))

(defun org-asana--handle-rate-limit ()
  "Handle rate limit error."
  (org-asana--wait-for-rate-limit)
  (error "Rate limited"))

(defun org-asana--status-ok-p (status)
  "Check if STATUS indicates success."
  (and (>= status 200) (< status 300)))

(defun org-asana--make-request (method endpoint &optional data)
  "Make HTTP request to Asana API with rate limiting."
  (org-asana--make-request-with-retry method endpoint data))

;;; Public Commands

;;;###autoload
(defun org-asana-test-connection ()
  "Test connection to Asana API."
  (interactive)
  (unless org-asana-token
    (error "No Asana token configured. Set `org-asana-token'"))
  (condition-case err
      (let* ((user (alist-get 'data (org-asana--make-request "GET" "/users/me"))))
        (message "Connected to Asana as: %s" (alist-get 'name user))
        t)
    (error
     (message "Connection failed: %s" (error-message-string err))
     nil)))

(defun org-asana--validate-configuration ()
  "Validate token and org file configuration."
  (unless org-asana-token
    (error "No Asana token configured. Set `org-asana-token'"))
  (unless org-asana-org-file
    (error "No org file configured. Set `org-asana-org-file'")))

(defun org-asana--initialize-buffer (buffer)
  "Initialize BUFFER with proper structure."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (when (= (buffer-size) 0)
      (insert "* Active Projects\n\n* COMPLETED\n")
      (goto-char (point-min)))))

(defun org-asana--orchestrate-sync (buffer)
  "Orchestrate sync process in BUFFER."
  (with-current-buffer buffer
    (org-asana--perform-sync-operations)
    (org-asana--apply-visual-enhancements)
    (save-buffer)))

(defun org-asana--perform-sync-operations ()
  "Perform core sync operations in order."
  (org-asana--process-new-captures)
  (org-asana--sync-done-tasks)
  (org-asana--sync-bidirectional))

(defun org-asana--sync-bidirectional ()
  "Sync tasks between org and Asana."
  (org-asana--sync-from-asana)
  (org-asana--sync-to-asana))

(defun org-asana--apply-visual-enhancements ()
  "Apply visual enhancements to synced tasks."
  (org-asana--update-progress-indicators)
  (org-asana--apply-task-faces))

;;;###autoload
(defun org-asana-sync ()
  "Sync tasks between Org and Asana."
  (interactive)
  (org-asana--validate-configuration)
  (let ((buffer (find-file-noselect org-asana-org-file)))
    (org-asana--initialize-buffer buffer)
    (org-asana--orchestrate-sync buffer)
    (message "Sync complete")))

;;; Internal Sync Functions

(defun org-asana--get-task-project-and-section ()
  "Get project and section names for current task."
  (save-excursion
    (org-back-to-heading t)
    (let ((project "")
          (section ""))
      (when (re-search-backward "^\\*\\{3\\} " nil t)
        (setq section (org-get-heading t t t t))
        (when (re-search-backward "^\\*\\{2\\} " nil t)
          (setq project (org-get-heading t t t t))))
      (cons project section))))

(defun org-asana--mark-task-completed-in-asana (task-id)
  "Mark TASK-ID as completed in Asana."
  (condition-case nil
      (org-asana--make-request
       "PUT"
       (format "/tasks/%s" task-id)
       `((data . ((completed . t)))))
    (error nil)))

(defun org-asana--collect-done-task-info ()
  "Collect information about current DONE task."
  (let* ((task-start (line-beginning-position))
         (task-id (org-entry-get nil "ASANA_TASK_ID"))
         (task-end (save-excursion
                    (org-end-of-subtree t)
                    (point))))
    (when task-id
      (org-asana--mark-task-completed-in-asana task-id)
      (list task-start task-end
            (buffer-substring task-start task-end)
            (org-asana--get-task-project-and-section)))))

(defun org-asana--find-done-tasks ()
  "Find all DONE tasks in Active Projects section."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (let ((active-end (save-excursion
                         (if (re-search-forward "^\\* COMPLETED$" nil t)
                             (match-beginning 0)
                           (point-max))))
            (tasks-to-move '()))

        (while (re-search-forward "^\\*\\{4\\} DONE " active-end t)
          (let ((task-info (org-asana--collect-done-task-info)))
            (when task-info
              (push task-info tasks-to-move))))

        (nreverse tasks-to-move)))))

(defun org-asana--find-or-create-completed-project (project-name)
  "Find or create PROJECT-NAME in COMPLETED section."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "^\\* COMPLETED$" nil t)
    (let ((project-pos (save-excursion
                        (when (re-search-forward
                               (format "^\\*\\* %s$" (regexp-quote project-name))
                               nil t)
                          (point)))))
      (unless project-pos
        (org-end-of-subtree t)
        (insert (format "\n** %s\n" project-name))
        (setq project-pos (point)))
      project-pos)))

(defun org-asana--find-or-create-completed-section (section-name project-pos)
  "Find or create SECTION-NAME under PROJECT-POS in COMPLETED section."
  (save-excursion
    (goto-char project-pos)
    (let ((section-pos (save-excursion
                        (when (re-search-forward
                               (format "^\\*\\*\\* %s$" (regexp-quote section-name))
                               nil t)
                          (point)))))
      (unless section-pos
        (org-end-of-subtree t)
        (insert (format "*** %s\n" section-name))
        (setq section-pos (point)))
      section-pos)))

(defun org-asana--add-completion-timestamp ()
  "Add completion timestamp to current task."
  (save-excursion
    (re-search-backward "^\\*\\{4\\} DONE " nil t)
    (org-set-property "ASANA_COMPLETED_AT"
                     (format-time-string "%Y-%m-%dT%H:%M:%S.000Z"))))

(defun org-asana--move-task-to-completed (task-info)
  "Move TASK-INFO to COMPLETED section."
  (let ((start (nth 0 task-info))
        (end (nth 1 task-info))
        (content (nth 2 task-info))
        (project (car (nth 3 task-info)))
        (section (cdr (nth 3 task-info))))

    ;; Delete from active section
    (delete-region start end)

    ;; Add to completed section
    (let* ((project-pos (org-asana--find-or-create-completed-project project))
           (section-pos (org-asana--find-or-create-completed-section section project-pos)))
      (save-excursion
        (goto-char section-pos)
        (org-end-of-subtree t)
        (insert content)
        (org-asana--add-completion-timestamp)))))

(defun org-asana--get-user-and-workspace-info ()
  "Get user and workspace information from Asana."
  (let* ((user-info (alist-get 'data (org-asana--make-request "GET" "/users/me")))
         (user-gid (alist-get 'gid user-info))
         (workspaces (alist-get 'data (org-asana--make-request "GET" "/workspaces")))
         (workspace-gid (alist-get 'gid (car workspaces))))
    (list user-gid workspace-gid)))

(defun org-asana--fetch-paginated (endpoint)
  "Fetch all pages from ENDPOINT."
  (let ((all-data '())
        (next-page endpoint)
        (page-count 0))
    (while next-page
      (let* ((response (org-asana--make-request "GET" next-page))
             (data (alist-get 'data response))
             (next-page-info (alist-get 'next_page response)))
        (setq all-data (append all-data data))
        (setq page-count (1+ page-count))
        (message "Fetched page %d (%d items so far)" page-count (length all-data))
        (setq next-page (when next-page-info
                         (alist-get 'path next-page-info)))))
    all-data))

(defun org-asana--fetch-incomplete-tasks (user-gid workspace-gid)
  "Fetch incomplete tasks assigned to USER-GID from WORKSPACE-GID."
  (let ((opt-fields "gid,name,notes,completed,due_on,modified_at,priority,tags.name,memberships.project.name,memberships.section.name,permalink_url"))
    (org-asana--fetch-paginated
     (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s"
             workspace-gid user-gid opt-fields))))


(defun org-asana--format-comments (comments)
  "Format COMMENTS list for org-mode display."
  (let ((formatted-comments '()))
    (when comments
      (dolist (comment comments)
        (let ((text (alist-get 'text comment))
              (created-at (alist-get 'created_at comment))
              (created-by (alist-get 'name (alist-get 'created_by comment)))
              (type (alist-get 'type comment)))
          (when (and text (string= type "comment"))
            (push (format "- %s (%s): %s"
                         (or created-by "Unknown")
                         (if created-at
                             (format-time-string "%Y-%m-%d %H:%M"
                                               (org-asana--parse-asana-timestamp created-at))
                           "Unknown time")
                         text)
                  formatted-comments)))))
    (concat "***** Comments\n"
            (if formatted-comments
                (concat (string-join (nreverse formatted-comments) "\n") "\n")
              ""))))

(defun org-asana--format-attachments (attachments)
  "Format ATTACHMENTS list for org-mode display."
  (let ((formatted-attachments '()))
    (when attachments
      (dolist (attachment attachments)
        (let ((name (alist-get 'name attachment))
              (download-url (alist-get 'download_url attachment))
              (view-url (alist-get 'view_url attachment)))
          (when name
            (push (format "- [[%s][%s]]"
                         (or view-url download-url "#")
                         name)
                  formatted-attachments)))))
    (concat "***** Attachments\n"
            (if formatted-attachments
                (concat (string-join (nreverse formatted-attachments) "\n") "\n")
              ""))))

(defun org-asana--process-project-sections (project-entry section-start batch-data)
  "Process PROJECT-ENTRY sections under SECTION-START using BATCH-DATA."
  (let* ((project-name (car project-entry))
         (sections (cdr project-entry))
         ;; Only create project heading if it has sections with tasks
         (has-tasks nil))

    ;; Check if any section has tasks
    (dolist (section-entry sections)
      (when (cdr section-entry)
        (setq has-tasks t)))

    (when has-tasks
      (let ((project-pos (org-asana--find-or-create-heading
                         2 project-name section-start)))
        (goto-char project-pos)

        (dolist (section-entry sections)
          (let* ((section-name (car section-entry))
                 (tasks (cdr section-entry)))
            ;; Only create section if it has tasks
            (when tasks
              (let ((section-pos (org-asana--find-or-create-heading
                                 3 section-name project-pos)))
                (goto-char section-pos)

                (dolist (task tasks)
                  (org-asana--update-or-create-task task section-pos batch-data))))))))))

(defun org-asana--process-task-tree (task-tree batch-data)
  "Process TASK-TREE using BATCH-DATA and update Active Projects section."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\* Active Projects$" nil t)
    (let ((section-start (point)))

      ;; Only process projects that have tasks
      (dolist (project-entry task-tree)
        (when (cdr project-entry) ; Only if project has sections
          (org-asana--process-project-sections project-entry section-start batch-data)))

      ;; Clean up any empty sections/projects from previous syncs
      (org-asana--cleanup-empty-sections)

      (org-asana--update-statistics))))

(defun org-asana--sync-done-tasks ()
  "Find DONE tasks, sync to Asana, and move to COMPLETED section."
  (let ((done-tasks (org-asana--find-done-tasks)))
    (dolist (task-info done-tasks)
      (org-asana--move-task-to-completed task-info))))

(defun org-asana--extract-task-fields (task batch-data)
  "Extract and organize fields from TASK using BATCH-DATA."
  (let* ((task-id (alist-get 'gid task))
         (task-data (alist-get task-id batch-data))
         (comments (car task-data))
         (attachments (cdr task-data)))
    (list :task-id task-id
          :task-name (alist-get 'name task)
          :completed (alist-get 'completed task)
          :notes (alist-get 'notes task)
          :due-on (alist-get 'due_on task)
          :modified-at (alist-get 'modified_at task)
          :priority (alist-get 'priority task)
          :tags (alist-get 'tags task)
          :permalink-url (alist-get 'permalink_url task)
          :comments comments
          :attachments attachments)))

(defun org-asana--apply-asana-updates (task-fields)
  "Apply TASK-FIELDS updates to current org entry."
  (org-asana--update-task-state task-fields)
  (org-asana--update-task-heading task-fields)
  (org-asana--update-task-deadline task-fields)
  (org-asana--update-task-priority task-fields)
  (org-asana--update-task-tags task-fields)
  (org-asana--update-task-timestamp task-fields))

(defun org-asana--update-task-state (task-fields)
  "Update task TODO state from TASK-FIELDS."
  (let ((completed (plist-get task-fields :completed)))
    (org-todo (org-asana--get-org-todo-state completed))))

(defun org-asana--update-task-heading (task-fields)
  "Update task heading from TASK-FIELDS."
  (let ((task-name (plist-get task-fields :task-name)))
    (org-asana--update-heading task-name)))

(defun org-asana--update-task-deadline (task-fields)
  "Update task deadline from TASK-FIELDS."
  (let ((due-on (plist-get task-fields :due-on)))
    (when due-on
      (org-asana--set-deadline due-on))))

(defun org-asana--set-deadline (due-on)
  "Set deadline to DUE-ON date."
  (let ((formatted-date (org-asana--format-asana-date due-on)))
    (when formatted-date
      (org-deadline nil formatted-date))))

(defun org-asana--update-task-priority (task-fields)
  "Update task priority from TASK-FIELDS."
  (when org-asana-sync-priority
    (let ((priority (plist-get task-fields :priority)))
      (when priority
        (org-asana--set-priority priority)))))

(defun org-asana--update-task-tags (task-fields)
  "Update task tags from TASK-FIELDS."
  (when org-asana-sync-tags
    (let ((tags (plist-get task-fields :tags)))
      (when tags
        (org-asana--set-tags tags)))))

(defun org-asana--update-task-timestamp (task-fields)
  "Update task modification timestamp from TASK-FIELDS."
  (let ((modified-at (plist-get task-fields :modified-at)))
    (org-set-property "ASANA_MODIFIED" modified-at)))

(defun org-asana--update-existing-task (task-fields existing-pos)
  "Update existing task at EXISTING-POS with TASK-FIELDS."
  (save-excursion
    (goto-char existing-pos)
    (let* ((org-data (org-asana--extract-task-data))
           (task (list (cons 'name (plist-get task-fields :task-name))
                      (cons 'completed (plist-get task-fields :completed))
                      (cons 'modified_at (plist-get task-fields :modified-at))))
           (winner (if (org-asana--needs-sync-p org-data task)
                      (org-asana--resolve-conflict org-data task)
                    :none)))
      (when (eq winner :asana)
        (org-asana--apply-asana-updates task-fields)))))

(defun org-asana--format-new-task-heading (task-fields)
  "Format heading for new task from TASK-FIELDS."
  (let* ((task-name (plist-get task-fields :task-name))
         (completed (plist-get task-fields :completed))
         (priority (plist-get task-fields :priority))
         (tags (plist-get task-fields :tags))
         (permalink-url (plist-get task-fields :permalink-url))
         (todo-state (org-asana--get-org-todo-state completed))
         (priority-str (when (and org-asana-sync-priority priority)
                        (org-asana--format-org-priority priority)))
         (tags-str (when (and org-asana-sync-tags tags)
                    (org-asana--format-org-tags tags)))
         (linked-name (if (and permalink-url (not (string-empty-p permalink-url)))
                         (format "[[%s][%s]]" permalink-url (or task-name "Untitled Task"))
                       (or task-name "Untitled Task"))))

    (format "**** %s %s%s%s\n"
            todo-state
            (or priority-str "")
            linked-name
            (or tags-str ""))))

(defun org-asana--create-new-task (task-fields parent-pos)
  "Create new task from TASK-FIELDS under PARENT-POS."
  (let ((completed (plist-get task-fields :completed)))
    ;; Only create incomplete tasks in Active Projects section
    ;; JSON false comes as :false, not nil
    (when (org-asana--json-false-p completed)
      (save-excursion
        (goto-char parent-pos)
        (org-end-of-subtree t)
        (unless (bolp) (insert "\n"))
        (insert (org-asana--format-new-task-heading task-fields))
        (org-asana--set-task-properties task-fields)
        (org-asana--add-task-notes task-fields)
        (insert "\n")))))

(defun org-asana--set-task-properties (task-fields)
  "Set properties for task from TASK-FIELDS."
  (let ((task-id (plist-get task-fields :task-id))
        (modified-at (plist-get task-fields :modified-at))
        (due-on (plist-get task-fields :due-on)))

    (when due-on
      (let ((formatted-date (org-asana--format-asana-date due-on)))
        (when formatted-date
          (org-deadline nil formatted-date))))
    (org-set-property "ASANA_TASK_ID" task-id)
    (org-set-property "ASANA_MODIFIED" modified-at)))

(defun org-asana--insert-task-notes (notes)
  "Insert NOTES text if not empty."
  (when (and notes (not (string-empty-p notes)))
    (insert notes "\n")))

(defun org-asana--add-task-notes (task-fields)
  "Add notes to task from TASK-FIELDS."
  (let ((notes (plist-get task-fields :notes))
        (comments (plist-get task-fields :comments))
        (attachments (plist-get task-fields :attachments)))
    (org-end-of-meta-data t)
    (org-asana--insert-task-notes notes)
    (when (or attachments comments)
      (unless (bolp) (insert "\n")))
    (insert (org-asana--format-attachments attachments))
    (when (and attachments comments)
      (insert "\n"))
    (insert (org-asana--format-comments comments))))

(defun org-asana--extract-new-comments ()
  "Extract new comments from current org task."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion (org-end-of-subtree t) (point)))
          (new-comments '()))
      (goto-char start)
      (when (re-search-forward "^\\*\\*\\* New Comments$" end t)
        (let ((comments-start (point))
              (comments-end (save-excursion
                             (if (re-search-forward "^\\*\\*\\* " end t)
                                 (match-beginning 0)
                               end))))
          (goto-char comments-start)
          (while (re-search-forward "^- \\(.*\\)$" comments-end t)
            (push (match-string 1) new-comments))))
      (nreverse new-comments))))

(defun org-asana--add-comment-to-asana (task-id comment-text)
  "Add COMMENT-TEXT to TASK-ID in Asana."
  (condition-case nil
      (org-asana--make-request
       "POST"
       (format "/tasks/%s/stories" task-id)
       `((data . ((text . ,comment-text)))))
    (error nil)))

(defun org-asana--sync-new-comments ()
  "Sync new comments from org to Asana."
  (let ((task-id (org-entry-get nil "ASANA_TASK_ID"))
        (new-comments (org-asana--extract-new-comments)))
    (when (and task-id new-comments)
      (dolist (comment new-comments)
        (org-asana--add-comment-to-asana task-id comment))
      ;; Clear the new comments section after syncing
      (org-asana--clear-new-comments-section))))

(defun org-asana--clear-new-comments-section ()
  "Clear the New Comments section from current task."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion (org-end-of-subtree t) (point))))
      (goto-char start)
      (when (re-search-forward "^\\*\\*\\* New Comments$" end t)
        (let ((section-start (match-beginning 0))
              (section-end (save-excursion
                            (if (re-search-forward "^\\*\\*\\* " end t)
                                (match-beginning 0)
                              end))))
          (delete-region section-start section-end))))))

(defun org-asana--get-user-gid (user-workspace-info)
  "Extract user GID from USER-WORKSPACE-INFO."
  (nth 0 user-workspace-info))

(defun org-asana--get-workspace-gid (user-workspace-info)
  "Extract workspace GID from USER-WORKSPACE-INFO."
  (nth 1 user-workspace-info))

(defun org-asana--get-workspace-info ()
  "Get user and workspace GIDs from Asana."
  (let* ((user-workspace-info (org-asana--get-user-and-workspace-info))
         (user-gid (org-asana--get-user-gid user-workspace-info))
         (workspace-gid (org-asana--get-workspace-gid user-workspace-info)))
    (list user-gid workspace-gid)))

(defun org-asana--fetch-task-metadata (task-ids)
  "Fetch comments and attachments for TASK-IDS."
  (org-asana--fetch-task-data-batch task-ids))

(defun org-asana--extract-task-gids (tasks)
  "Extract GIDs from TASKS list."
  (mapcar (lambda (task) (alist-get 'gid task)) tasks))

(defun org-asana--sync-from-asana ()
  "Sync tasks from Asana to org."
  (let* ((tasks (org-asana--fetch-user-tasks))
         (metadata (org-asana--fetch-tasks-metadata tasks))
         (tree (org-asana--build-task-tree tasks)))
    (org-asana--process-task-tree tree metadata)))

(defun org-asana--fetch-user-tasks ()
  "Fetch all incomplete tasks for current user."
  (let ((workspace-info (org-asana--get-workspace-info)))
    (org-asana--fetch-incomplete-tasks
     (car workspace-info)
     (cadr workspace-info))))

(defun org-asana--fetch-tasks-metadata (tasks)
  "Fetch metadata for all TASKS."
  (let ((task-ids (org-asana--extract-task-gids tasks)))
    (org-asana--fetch-task-metadata task-ids)))

(defun org-asana--find-active-tasks-region ()
  "Find the start and end positions of Active Projects section."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (let ((start (point))
            (end (save-excursion
                   (if (re-search-forward "^\\* COMPLETED$" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (cons start end)))))

(defun org-asana--extract-org-task-data ()
  "Extract task data from current org entry."
  (let* ((task-id (org-entry-get nil "ASANA_TASK_ID"))
         (heading (org-get-heading t t t t))
         (todo-state (org-get-todo-state))
         (completed (string= todo-state "DONE"))
         (deadline (org-entry-get nil "DEADLINE"))
         (body (org-asana--get-task-body))
         (priority (org-entry-get nil "PRIORITY"))
         (tags (org-get-tags)))

    (when task-id
      (list :task-id task-id
            :heading heading
            :completed completed
            :deadline deadline
            :body body
            :priority priority
            :tags tags))))

(defun org-asana--build-asana-update-data (org-task-data)
  "Build Asana API update data from ORG-TASK-DATA."
  (let* ((heading (plist-get org-task-data :heading))
         (completed (plist-get org-task-data :completed))
         (deadline (plist-get org-task-data :deadline))
         (body (plist-get org-task-data :body))
         (priority (plist-get org-task-data :priority))
         (due-date (when deadline
                    (format-time-string
                     "%Y-%m-%d"
                     (org-time-string-to-time deadline))))
         (asana-priority (when (and org-asana-sync-priority priority)
                          (org-asana--get-asana-priority priority))))

    `((data . ((name . ,heading)
              (notes . ,body)
              (completed . ,completed)
              ,@(when due-date `((due_on . ,due-date)))
              ,@(when asana-priority `((priority . ,asana-priority))))))))

(defun org-asana--send-task-update (task-id update-data)
  "Send UPDATE-DATA for TASK-ID to Asana API."
  (condition-case nil
      (org-asana--make-request
       "PUT"
       (format "/tasks/%s" task-id)
       update-data)
    (error nil)))

(defun org-asana--sync-single-task ()
  "Sync current task to Asana if it has a task ID."
  (let ((org-task-data (org-asana--extract-org-task-data)))
    (when org-task-data
      (let* ((task-id (plist-get org-task-data :task-id))
             (update-data (org-asana--build-asana-update-data org-task-data)))
        (org-asana--send-task-update task-id update-data)
        (org-asana--sync-new-comments)))))

(defun org-asana--sync-to-asana ()
  "Sync local changes to Asana."
  (save-excursion
    (let ((region (org-asana--find-active-tasks-region)))
      (when region
        (goto-char (car region))
        (let ((active-end (cdr region)))
          (while (re-search-forward "^\\*\\{4\\} \\(TODO\\|DONE\\) " active-end t)
            (org-asana--sync-single-task)))))))

;;; Data Transformation Functions

(defun org-asana--json-null-p (value)
  "Check if VALUE represents JSON null."
  (or (eq value :null) (eq value nil)))

(defun org-asana--format-timestamp (timestamp)
  "Format TIMESTAMP to readable string."
  (if timestamp
      (format-time-string "%Y-%m-%d %H:%M" timestamp)
    ""))

(defun org-asana--calculate-retry-delay (attempt)
  "Calculate exponential backoff delay for ATTEMPT."
  (* attempt attempt 2))

(defun org-asana--validate-response (response)
  "Validate API RESPONSE structure."
  (and response
       (listp response)
       (alist-get 'data response)))

(defun org-asana--parse-asana-timestamp (timestamp-str)
  "Parse Asana timestamp string to Emacs time."
  (when (and timestamp-str
             (not (org-asana--json-null-p timestamp-str))
             (not (string-empty-p timestamp-str)))
    (date-to-time timestamp-str)))

(defun org-asana--format-asana-date (date-str)
  "Format Asana DATE-STR for org-mode deadline."
  (when (and date-str
             (not (org-asana--json-null-p date-str))
             (not (string-empty-p date-str)))
    (format-time-string "<%Y-%m-%d %a>" (date-to-time date-str))))

(defun org-asana--json-true-p (value)
  "Check if VALUE represents JSON true."
  (or (eq value t) (eq value :true) (eq value 'true)))

(defun org-asana--json-false-p (value)
  "Check if VALUE represents JSON false."
  (or (eq value :false) (eq value nil) (eq value 'false)))

(defun org-asana--get-org-todo-state (completed)
  "Get org TODO state based on COMPLETED boolean."
  (if (org-asana--json-true-p completed) "DONE" "TODO"))

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

(defun org-asana--compare-timestamps (org-time asana-time-str)
  "Compare ORG-TIME with ASANA-TIME-STR, return t if org is newer."
  (when (and org-time asana-time-str)
    (let ((asana-time (org-asana--parse-asana-timestamp asana-time-str)))
      (time-less-p asana-time org-time))))

(defun org-asana--needs-sync-p (org-data asana-task)
  "Check if ORG-DATA and ASANA-TASK need synchronization."
  (let* ((org-name (plist-get org-data :name))
         (asana-name (alist-get 'name asana-task))
         (org-completed (plist-get org-data :completed))
         (asana-completed (alist-get 'completed asana-task))
         (org-modified (plist-get org-data :modified))
         (asana-modified (alist-get 'modified_at asana-task)))

    (or (not (string= org-name asana-name))
        (not (eq org-completed asana-completed))
        (and org-modified asana-modified
             (not (string= org-modified asana-modified))))))

(defun org-asana--resolve-conflict (org-data asana-task)
  "Resolve conflict between ORG-DATA and ASANA-TASK.
Returns :org or :asana depending on resolution strategy."
  (cond
   ((eq org-asana-conflict-resolution 'asana-wins) :asana)
   ((eq org-asana-conflict-resolution 'local-wins) :org)
   ((eq org-asana-conflict-resolution 'newest-wins)
    (let* ((org-modified (plist-get org-data :modified))
           (asana-modified (alist-get 'modified_at asana-task)))
      (if (and org-modified asana-modified)
          (if (org-asana--compare-timestamps
               (org-asana--parse-asana-timestamp org-modified)
               asana-modified)
              :org
            :asana)
        :asana)))
   (t :asana)))

;;; Helper Functions

(defun org-asana--build-task-tree (tasks)
  "Build a tree structure from flat TASKS list."
  (let ((tree '()))
    (dolist (task tasks)
      (let* ((memberships (alist-get 'memberships task))
             (membership (car memberships))
             (project-name (or (alist-get 'name (alist-get 'project membership))
                              "No Project"))
             (section-name (or (alist-get 'name (alist-get 'section membership))
                              "No Section"))
             (project-entry (assoc project-name tree)))

        (unless project-entry
          (setq project-entry (cons project-name '()))
          (push project-entry tree))

        (let ((section-entry (assoc section-name (cdr project-entry))))
          (unless section-entry
            (setq section-entry (cons section-name '()))
            (setcdr project-entry (cons section-entry (cdr project-entry))))

          (setcdr section-entry (cons task (cdr section-entry))))))

    (nreverse tree)))

(defun org-asana--find-or-create-heading (level heading parent-pos)
  "Find or create HEADING at LEVEL after PARENT-POS."
  (save-excursion
    (goto-char parent-pos)
    (let* ((stars (make-string level ?*))
           (stats-cookie (if (or (= level 2) (= level 3)) " [/]" ""))
           (heading-with-stats (concat heading stats-cookie))
           (regexp (format "^%s %s\\(?: \\[\\d+/\\d+\\]\\)?$" stars (regexp-quote heading)))
           (end-pos (save-excursion
                     (when (org-at-heading-p)
                       (org-end-of-subtree t))
                     (point))))

      (if (re-search-forward regexp end-pos t)
          (point)
        (goto-char end-pos)
        (unless (bolp) (insert "\n"))
        (insert (format "%s %s\n" stars heading-with-stats))
        (point)))))

(defun org-asana--update-or-create-task (task parent-pos batch-data)
  "Update or create TASK under PARENT-POS using BATCH-DATA."
  (let* ((task-fields (org-asana--extract-task-fields task batch-data))
         (task-id (plist-get task-fields :task-id))
         (existing-pos (org-asana--find-task-by-id task-id parent-pos)))

    (if existing-pos
        (org-asana--update-existing-task task-fields existing-pos)
      (org-asana--create-new-task task-fields parent-pos))))

(defun org-asana--find-task-by-id (task-id parent-pos)
  "Find task with TASK-ID in subtree starting at PARENT-POS.
Uses org-map-entries for robust subtree boundary handling."
  (save-excursion
    (goto-char parent-pos)
    (let ((found-pos nil))
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get (point) "ASANA_TASK_ID") task-id)
           (setq found-pos (point))))
       nil 'tree)
      found-pos)))

(defun org-asana--update-heading (new-text)
  "Update current heading text to NEW-TEXT."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (let ((todo (match-string 2))
            (priority (match-string 3))
            (tags (match-string 5))
            (level (org-current-level)))
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (insert (make-string level ?*) " "
                (or todo "") (if todo " " "")
                (or priority "") (if priority " " "")
                new-text
                (or tags ""))))))

(defun org-asana--get-task-body ()
  "Get the body text of current task."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((start (point))
          (end (save-excursion
                 (org-end-of-subtree t)
                 (point))))
      (string-trim (buffer-substring-no-properties start end)))))

(defun org-asana--extract-task-data ()
  "Extract task data from current org entry."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading (org-get-heading t t t t))
           (todo-state (org-get-todo-state))
           (priority (org-entry-get nil "PRIORITY"))
           (tags (org-get-tags))
           (modified (org-entry-get nil "ASANA_MODIFIED"))
           (completed (org-asana--get-asana-completed todo-state)))
      `(:name ,heading
        :completed ,completed
        :priority ,priority
        :tags ,tags
        :modified ,modified))))

(defun org-asana--set-priority (asana-priority)
  "Set org priority from ASANA-PRIORITY."
  (let ((priority-char (cond
                        ((string= asana-priority "high") ?A)
                        ((string= asana-priority "medium") ?B)
                        ((string= asana-priority "low") ?C)
                        (t nil))))
    (if priority-char
        (org-priority priority-char)
      (org-priority ?\ ))))

(defun org-asana--set-tags (asana-tags)
  "Set org tags from ASANA-TAGS."
  (let ((tag-names (mapcar (lambda (tag) (alist-get 'name tag)) asana-tags)))
    (org-set-tags tag-names)))

(defun org-asana--update-statistics ()
  "Update TODO statistics for all projects and sections."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (org-update-statistics-cookies t))))

(defun org-asana--cleanup-empty-sections ()
  "Remove empty sections and projects in Active Projects."
  (let ((active-end (org-asana--find-active-projects-end)))
    (when active-end
      (org-asana--cleanup-empty-subtrees "^\\*\\*\\* " active-end)
      (org-asana--cleanup-empty-subtrees "^\\*\\* " active-end))))

(defun org-asana--find-active-projects-end ()
  "Find end position of Active Projects section."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (if (re-search-forward "^\\* COMPLETED$" nil t)
          (match-beginning 0)
        (point-max)))))

(defun org-asana--cleanup-empty-subtrees (pattern end-pos)
  "Remove empty subtrees matching PATTERN before END-POS."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (while (re-search-forward pattern end-pos t)
        (let ((start (match-beginning 0)))
          (when (org-asana--subtree-empty-p start pattern)
            (org-asana--remove-subtree start)))))))

(defun org-asana--subtree-empty-p (start pattern)
  "Check if subtree at START has children matching PATTERN."
  (save-excursion
    (goto-char start)
    (org-end-of-subtree t)
    (let ((end (point))
          (child-pattern (org-asana--get-child-pattern pattern)))
      (goto-char start)
      (not (re-search-forward child-pattern end t)))))

(defun org-asana--get-child-pattern (parent-pattern)
  "Get child heading pattern for PARENT-PATTERN."
  (cond
   ((string= parent-pattern "^\\*\\*\\* ") "^\\*\\*\\*\\* ")
   ((string= parent-pattern "^\\*\\* ") "^\\*\\*\\* ")
   (t nil)))

(defun org-asana--remove-subtree (pos)
  "Remove subtree at position POS."
  (save-excursion
    (goto-char pos)
    (org-cut-subtree)))

;;; Progress Indicators

(defcustom org-asana-show-progress-indicators t
  "Show progress indicators for projects and sections."
  :type 'boolean
  :group 'org-asana)

(defun org-asana--calculate-progress ()
  "Calculate progress statistics for current heading."
  (let ((total 0)
        (done 0))
    (save-excursion
      (when (org-goto-first-child)
        (cl-loop do
                 (when (org-entry-get nil "asana-id")
                   (cl-incf total)
                   (when (org-entry-is-done-p)
                     (cl-incf done)))
                 while (org-goto-sibling))))
    (cons done total)))

(defun org-asana--format-progress-indicator (done total)
  "Format progress indicator showing DONE of TOTAL tasks."
  (if (zerop total)
      ""
    (format "[%d/%d]" done total)))

(defun org-asana--update-progress-indicators ()
  "Update progress indicators for all projects and sections."
  (when org-asana-show-progress-indicators
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\{1,3\\} " nil t)
        (let ((level (org-current-level)))
          (when (and (<= level 3)
                     (not (org-entry-get nil "asana-id")))
            (org-asana--update-heading-progress)))))))

(defun org-asana--update-heading-progress ()
  "Update progress indicator for current heading."
  (let* ((progress (org-asana--calculate-progress))
         (done (car progress))
         (total (cdr progress))
         (indicator (org-asana--format-progress-indicator done total)))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\(\\*+\\) \\(.*?\\)\\( \\[[0-9]+/[0-9]+\\]\\)?$")
        (let ((stars (match-string 1))
              (title (match-string 2)))
          (delete-region (point) (line-end-position))
          (insert stars " " title
                  (if (string-empty-p indicator) "" (concat " " indicator))))))))

(defun org-asana--remove-progress-indicators ()
  "Remove all progress indicators from buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " \\[[0-9]+/[0-9]+\\]" nil t)
      (replace-match ""))))

;;; Capture Templates

(defcustom org-asana-default-project nil
  "Default Asana project GID for new tasks."
  :type '(choice (const :tag "No default project" nil)
                 (string :tag "Project GID"))
  :group 'org-asana)

(defvar org-asana-capture-templates
  '(("a" "Asana Task" entry
     (file+headline org-asana-org-file "Active Projects")
     "* TODO %^{Task Title}\n:PROPERTIES:\n:asana-id: new\n:asana-project: %(org-asana--select-project)\n:END:\nDEADLINE: %^t\n\n%?"
     :empty-lines-after 1)
    ("A" "Asana Task with Notes" entry
     (file+headline org-asana-org-file "Active Projects")
     "* TODO %^{Task Title}\n:PROPERTIES:\n:asana-id: new\n:asana-project: %(org-asana--select-project)\n:END:\nDEADLINE: %^t\n\n%^{Task Notes}\n\n%?"
     :empty-lines-after 1))
  "Capture templates for creating new Asana tasks.")

(defun org-asana--select-project ()
  "Select an Asana project for new task."
  (if org-asana-default-project
      org-asana-default-project
    (let* ((projects (org-asana--fetch-all-projects))
           (project-names (mapcar (lambda (p) (alist-get 'name p)) projects))
           (selected (completing-read "Select project: " project-names nil t)))
      (alist-get 'gid (cl-find selected projects
                                :key (lambda (p) (alist-get 'name p))
                                :test 'string=)))))

(defun org-asana--fetch-all-projects ()
  "Fetch all accessible Asana projects."
  (let ((endpoint "/projects?opt_fields=name,gid&workspace=")
        (projects '()))
    (dolist (workspace (org-asana--fetch-workspaces))
      (let ((workspace-gid (alist-get 'gid workspace)))
        (setq projects
              (append projects
                      (alist-get 'data
                                (org-asana--fetch-paginated
                                 (concat endpoint workspace-gid)))))))
    projects))

(defun org-asana--fetch-workspaces ()
  "Fetch all accessible Asana workspaces."
  (alist-get 'data (org-asana--make-request "GET" "/workspaces")))

(defun org-asana--add-capture-templates ()
  "Add Asana capture templates to org-capture."
  (dolist (template org-asana-capture-templates)
    (add-to-list 'org-capture-templates template t)))

(defun org-asana--process-new-captures ()
  "Process tasks marked as new and create them in Asana."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":asana-id: *new" nil t)
      (let* ((task-data (org-asana--extract-task-data))
             (project-gid (org-entry-get nil "asana-project"))
             (created-task (org-asana--create-task-in-asana task-data project-gid)))
        (when created-task
          (org-set-property "asana-id" (alist-get 'gid created-task))
          (org-delete-property "asana-project"))))))

(defun org-asana--extract-task-data ()
  "Extract task data from current org entry."
  (let ((heading (org-get-heading t t t t))
        (deadline (org-entry-get nil "DEADLINE"))
        (notes (org-asana--get-task-notes)))
    `((name . ,heading)
      ,@(when deadline `((due_on . ,(org-asana--format-date-for-asana deadline))))
      ,@(when notes `((notes . ,notes))))))

(defun org-asana--get-task-notes ()
  "Get task notes from current entry."
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (let ((start (point))
          (end (save-excursion (outline-next-heading) (point))))
      (buffer-substring-no-properties start end))))

(defun org-asana--format-date-for-asana (org-date)
  "Convert ORG-DATE to Asana date format (YYYY-MM-DD)."
  (format-time-string "%Y-%m-%d" (org-time-string-to-time org-date)))

(defun org-asana--create-task-in-asana (task-data project-gid)
  "Create TASK-DATA in PROJECT-GID on Asana."
  (let ((data `((data . ((projects . (,project-gid))
                        ,@task-data)))))
    (alist-get 'data (org-asana--make-request "POST" "/tasks" data))))

;;; Initialization

(defun org-asana-enable-agenda-integration ()
  "Enable Org Agenda integration for Asana tasks."
  (interactive)
  (org-asana--setup-agenda)
  (org-asana--add-agenda-commands)
  (message "Org-Asana agenda integration enabled"))

(defun org-asana-disable-agenda-integration ()
  "Disable Org Agenda integration for Asana tasks."
  (interactive)
  (org-asana--remove-from-agenda)
  (message "Org-Asana agenda integration disabled"))

(defun org-asana-enable-capture-templates ()
  "Enable Org Capture templates for Asana tasks."
  (interactive)
  (org-asana--add-capture-templates)
  (message "Org-Asana capture templates enabled"))

;; Auto-enable agenda integration when org-asana-org-file is set
(add-hook 'after-init-hook
          (lambda ()
            (when org-asana-org-file
              (org-asana-enable-agenda-integration))))

(provide 'org-asana)
;;; org-asana.el ends here
