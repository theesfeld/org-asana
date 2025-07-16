;;; org-asana.el --- Simple two-way sync between Org-mode and Asana -*- lexical-binding: t; -*-

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
;; Version: 3.1.0
;; Package-Requires: ((emacs "28.1") (org "9.4"))
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
;;
;; Adding Comments:
;; - Add new comments directly under the ***** Comments heading
;; - Format: - Your Name: Comment text
;; - Comments without :ASANA-COMMENT-GID: properties will be synced to Asana
;;
;; New Features:
;; - Tags: Org tags sync with Asana tags
;; - Start dates: SCHEDULED: property syncs with Asana start_on
;; - Permalinks: Stored in :ASANA-URL: property
;; - Followers: Listed in :ASANA-FOLLOWERS: property
;; - Multiple projects: Listed in :ASANA-PROJECTS: property
;; - Dependencies: Tracked in :ASANA-DEPENDENCIES: property
;; - Custom fields: Stored as :ASANA-CF-FIELD-NAME: properties
;; - Reactions: Likes/hearts shown in :ASANA-REACTIONS: property

;;; Code:

(require 'org)
(require 'url)
(require 'json)
(require 'cl-lib)
(require 'org-agenda)
(require 'org-capture)

;;; Variable declarations to suppress warnings
(defvar org-agenda-custom-commands)
(defvar org-capture-templates)
(defvar org-asana-org-file)

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

(defcustom org-asana-fetch-metadata t
  "Whether to fetch comments and attachments for tasks.
Setting this to nil can speed up sync significantly if you don't need metadata.
If you're experiencing repeated retry errors, try setting this to nil."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-show-activity-history t
  "Whether to show activity history (system stories) for tasks.
This includes events like task creation, assignments, due date changes, etc."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-debug nil
  "Enable debug messages for troubleshooting sync issues."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-max-retries 3
  "Maximum number of retry attempts for API requests.
Reduce this value if experiencing sync issues with large task lists.
Set to 1 to disable retries entirely."
  :type 'integer
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
    ;; Use single attempt for batch requests to avoid repeated failures
    (org-asana--make-request-with-retry "POST" "/batch" batch-data 1)))

(defun org-asana--build-task-data-actions (task-ids)
  "Build batch actions for fetching comments and attachments for TASK-IDS."
  (let ((actions '()))
    (dolist (task-id task-ids)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/stories" task-id))
              (options . ((opt_fields . "text,created_at,created_by.name,type"))))
            actions)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/attachments" task-id))
              (options . ((opt_fields . "name,download_url,view_url"))))
            actions))
    (nreverse actions)))

(defun org-asana--process-batch-responses (responses task-ids)
  "Process batch RESPONSES and return alist of task-id to (comments . attachments)."
  (let ((results '())
        (response-index 0))
    (dolist (task-id task-ids)
      (let* ((comments-response (nth response-index responses))
             (attachments-response (nth (1+ response-index) responses))
             ;; Extract data from the body field of each response
             (comments (when (and comments-response
                                 (alist-get 'body comments-response))
                        (alist-get 'data (alist-get 'body comments-response))))
             (attachments (when (and attachments-response
                                    (alist-get 'body attachments-response))
                           (alist-get 'data (alist-get 'body attachments-response)))))
        (push (cons task-id (cons comments attachments)) results)
        (setq response-index (+ response-index 2))))
    (nreverse results)))

(defun org-asana--fetch-task-data-batch (task-ids)
  "Fetch comments and attachments for TASK-IDS using batch API."
  (let ((all-results '())
        (total-tasks (length task-ids))
        (processed 0))
    ;; Process in batches of 5 tasks (10 actions per batch: 5 comments + 5 attachments)
    (while task-ids
      (let* ((batch-task-ids (seq-take task-ids 5))
             (remaining-task-ids (seq-drop task-ids 5))
             (actions (org-asana--build-task-data-actions batch-task-ids)))
        (condition-case err
            (let* ((batch-response (org-asana--make-batch-request actions))
                   (responses (alist-get 'data batch-response))
                   (batch-results (org-asana--process-batch-responses responses batch-task-ids)))
              (setq all-results (append all-results batch-results))
              (setq processed (+ processed (length batch-task-ids)))
              (message "Fetched metadata for %d/%d tasks" processed total-tasks))
          (error
           ;; If batch request fails, create empty results for this batch
           (dolist (task-id batch-task-ids)
             (push (cons task-id (cons nil nil)) all-results))
           (setq processed (+ processed (length batch-task-ids)))
           (if org-asana-debug
               (message "DEBUG: Batch metadata fetch failed (%d/%d): %s" 
                       processed total-tasks (error-message-string err))
             (message "WARNING: Some metadata could not be fetched (%d/%d tasks processed)" 
                     processed total-tasks))))
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
        (max-attempts (or max-retries org-asana-max-retries))
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
             (when org-asana-debug
               (message "DEBUG: Request to %s failed (attempt %d/%d): %s" 
                       endpoint attempt max-attempts (error-message-string err)))
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
     (t 
      (let ((error-body (ignore-errors (org-asana--parse-json-response buffer))))
        (if (and error-body (alist-get 'errors error-body))
            (error "Asana API error: %s" (alist-get 'message (car (alist-get 'errors error-body))))
          (error "Asana API error: HTTP %d" status)))))))

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
    ;; Collapse all property drawers and task headings
    (org-asana--collapse-all-drawers)
    (org-asana--collapse-all-tasks)
    (save-buffer)))

(defun org-asana--collapse-all-drawers ()
  "Collapse all property drawers in the buffer."
  (org-cycle-hide-drawers 'all))

(defun org-asana--collapse-all-tasks ()
  "Collapse all task headings (level 4 and below)."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\{4,\\} " nil t)
      (org-cycle))))

(defun org-asana--perform-sync-operations ()
  "Perform core sync operations in order."
  (org-asana--process-new-captures)
  (org-asana--sync-done-tasks)
  (org-asana--sync-bidirectional))

(defun org-asana--sync-bidirectional ()
  "Sync tasks between org and Asana."
  (org-asana--sync-from-asana)
  ;; Skip sync-to-asana after fetching from Asana to avoid unnecessary API calls
  )

(defun org-asana--apply-visual-enhancements ()
  "Apply visual enhancements to synced tasks."
  (org-asana--update-progress-indicators)
  (org-asana--apply-task-faces))

;;;###autoload
(defun org-asana-sync ()
  "Sync tasks between Org and Asana."
  (interactive)
  (org-asana--validate-configuration)
  (let ((buffer (find-file-noselect org-asana-org-file))
        (sync-start-time (current-time)))
    (org-asana--initialize-buffer buffer)
    (condition-case err
        (progn
          (org-asana--orchestrate-sync buffer)
          (let ((sync-time (float-time (time-subtract (current-time) sync-start-time))))
            (message "Sync complete in %.1f seconds" sync-time)))
      (error
       (if org-asana-debug
           (message "Sync failed: %s" (error-message-string err))
         (message "Sync failed. Enable `org-asana-debug' for details"))
       (signal (car err) (cdr err))))))

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

(defun org-asana--extract-page-data (response)
  "Extract data from paginated RESPONSE."
  (alist-get 'data response))

(defun org-asana--extract-next-page (response)
  "Extract next page path from RESPONSE."
  (let ((next-page-info (alist-get 'next_page response)))
    (when next-page-info
      (alist-get 'path next-page-info))))

(defun org-asana--report-page-progress (page-count total-items)
  "Report progress for PAGE-COUNT with TOTAL-ITEMS."
  (message "Fetched page %d (%d items so far)" page-count total-items))

(defun org-asana--handle-pagination-error (error-count max-errors page-count err)
  "Handle pagination error and return updated error count."
  (let ((new-error-count (1+ error-count)))
    (when org-asana-debug
      (message "Error fetching page %d: %s" (1+ page-count) (error-message-string err)))
    new-error-count))

(defun org-asana--should-stop-pagination-p (error-count max-errors)
  "Check if pagination should stop due to errors."
  (>= error-count max-errors))

(defun org-asana--report-pagination-stop (max-errors total-items)
  "Report pagination stopped due to errors."
  (message "WARNING: Stopped pagination after %d consecutive errors. Fetched %d items total." 
          max-errors total-items))

(defun org-asana--report-pagination-error (page-count error-count max-errors)
  "Report pagination error but continuing."
  (message "Error on page %d, continuing... (%d/%d errors)" 
          (1+ page-count) error-count max-errors))

(defun org-asana--report-pagination-completion (error-count total-items)
  "Report pagination completion with error summary."
  (when (> error-count 0)
    (message "Completed with %d items fetched despite %d error(s)" 
            total-items error-count)))

(defun org-asana--fetch-single-page (endpoint)
  "Fetch single page from ENDPOINT."
  (org-asana--make-request-with-retry "GET" endpoint nil 1))

(defun org-asana--process-page-success (response all-data page-count)
  "Process successful page RESPONSE and return (data next-page page-count)."
  (let ((data (org-asana--extract-page-data response))
        (next-page (org-asana--extract-next-page response))
        (new-page-count (1+ page-count)))
    (when data
      (setq all-data (append all-data data)))
    (org-asana--report-page-progress new-page-count (length all-data))
    (list all-data next-page new-page-count 0))) ; Reset error count on success

(defun org-asana--process-page-error (err error-count max-errors page-count all-data)
  "Process page error and return (data next-page page-count error-count)."
  (let ((new-error-count (org-asana--handle-pagination-error error-count max-errors page-count err)))
    (if (org-asana--should-stop-pagination-p new-error-count max-errors)
        (progn
          (org-asana--report-pagination-stop max-errors (length all-data))
          (list all-data nil page-count new-error-count))
      (progn
        (org-asana--report-pagination-error page-count new-error-count max-errors)
        (list all-data nil page-count new-error-count))))) ; Don't advance to next page on error

(defun org-asana--fetch-paginated (endpoint)
  "Fetch all pages from ENDPOINT."
  (let ((all-data '())
        (next-page endpoint)
        (page-count 0)
        (error-count 0)
        (max-errors 3))
    (while (and next-page (< error-count max-errors))
      (condition-case err
          (let ((result (org-asana--process-page-success 
                        (org-asana--fetch-single-page next-page)
                        all-data page-count)))
            (setq all-data (nth 0 result)
                  next-page (nth 1 result)
                  page-count (nth 2 result)
                  error-count (nth 3 result)))
        (error
         (let ((result (org-asana--process-page-error err error-count max-errors page-count all-data)))
           (setq all-data (nth 0 result)
                 next-page (nth 1 result)
                 page-count (nth 2 result)
                 error-count (nth 3 result))))))
    (org-asana--report-pagination-completion error-count (length all-data))
    all-data))

(defun org-asana--fetch-incomplete-tasks (user-gid workspace-gid)
  "Fetch incomplete tasks assigned to USER-GID from WORKSPACE-GID."
  (let ((opt-fields "gid,name,notes,completed,due_on,due_at,start_on,start_at,modified_at,priority,tags.gid,tags.name,memberships.project.gid,memberships.project.name,memberships.section.name,permalink_url,followers.gid,followers.name,parent.gid,parent.name,custom_fields,dependencies.gid,dependencies.name,dependents.gid,dependents.name,liked,num_likes,hearted,num_hearts"))
    (org-asana--fetch-paginated
     (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s"
             workspace-gid user-gid opt-fields))))


(defun org-asana--clean-story-text (text)
  "Clean up system story TEXT for better readability."
  (if (not text)
      "Unknown activity"
    (let ((cleaned text))
      ;; Replace common system messages with cleaner versions
      (setq cleaned (replace-regexp-in-string "added to " "added to project " cleaned))
      (setq cleaned (replace-regexp-in-string "removed from " "removed from project " cleaned))
      (setq cleaned (replace-regexp-in-string "changed the due date to " "set due date: " cleaned))
      (setq cleaned (replace-regexp-in-string "marked this task complete" "completed task" cleaned))
      (setq cleaned (replace-regexp-in-string "marked this task incomplete" "reopened task" cleaned))
      (setq cleaned (replace-regexp-in-string "assigned to " "assigned to: " cleaned))
      (setq cleaned (replace-regexp-in-string "unassigned from " "unassigned from: " cleaned))
      (setq cleaned (replace-regexp-in-string "changed the name to " "renamed to: " cleaned))
      cleaned)))

(defun org-asana--extract-story-data (story)
  "Extract relevant data from STORY."
  (list :gid (alist-get 'gid story)
        :text (alist-get 'text story)
        :created-at (alist-get 'created_at story)
        :created-by (alist-get 'name (alist-get 'created_by story))
        :type (alist-get 'type story)))

(defun org-asana--format-story-timestamp (created-at)
  "Format CREATED-AT timestamp for display."
  (if created-at
      (format-time-string "%Y-%m-%d %H:%M"
                         (org-asana--parse-asana-timestamp created-at))
    "Unknown time"))

(defun org-asana--format-single-comment (story-data)
  "Format single comment from STORY-DATA."
  (let ((gid (plist-get story-data :gid))
        (text (plist-get story-data :text))
        (created-at (plist-get story-data :created-at))
        (created-by (plist-get story-data :created-by)))
    (format "- %s (%s): %s\n  :PROPERTIES:\n  :ASANA-COMMENT-GID: %s\n  :END:"
            (or created-by "Unknown")
            (org-asana--format-story-timestamp created-at)
            text
            gid)))

(defun org-asana--format-single-history (story-data)
  "Format single history entry from STORY-DATA."
  (let ((gid (plist-get story-data :gid))
        (text (plist-get story-data :text))
        (created-at (plist-get story-data :created-at))
        (created-by (plist-get story-data :created-by)))
    (format "- [%s] %s: %s\n  :PROPERTIES:\n  :ASANA-STORY-GID: %s\n  :END:"
            (org-asana--format-story-timestamp created-at)
            (or created-by "System")
            (org-asana--clean-story-text text)
            gid)))

(defun org-asana--process-story-for-comments (story)
  "Process STORY and return formatted comment or nil."
  (when (and (listp story) (alist-get 'text story))
    (let ((story-data (org-asana--extract-story-data story))
          (type (alist-get 'type story)))
      (when (string= type "comment")
        (org-asana--format-single-comment story-data)))))

(defun org-asana--process-story-for-history (story)
  "Process STORY and return formatted history or nil."
  (when (and (listp story) (alist-get 'text story))
    (let ((story-data (org-asana--extract-story-data story))
          (type (alist-get 'type story)))
      (when (string= type "system")
        (org-asana--format-single-history story-data)))))

(defun org-asana--collect-comments (stories)
  "Collect formatted comments from STORIES."
  (let ((comments '()))
    (dolist (story stories)
      (let ((comment (org-asana--process-story-for-comments story)))
        (when comment
          (push comment comments))))
    (nreverse comments)))

(defun org-asana--collect-history (stories)
  "Collect formatted history from STORIES."
  (let ((history '()))
    (dolist (story stories)
      (let ((entry (org-asana--process-story-for-history story)))
        (when entry
          (push entry history))))
    (nreverse history)))

(defun org-asana--build-comments-section (comments)
  "Build comments section from COMMENTS list."
  (concat "***** Comments\n"
          (if comments
              (concat (string-join comments "\n\n") "\n")
            "")))

(defun org-asana--build-history-section (history)
  "Build history section from HISTORY list."
  (when (and org-asana-show-activity-history history)
    (concat "\n***** Activity History\n"
            (string-join history "\n\n") "\n")))

(defun org-asana--format-comments (stories)
  "Format STORIES list for org-mode display, separating comments and history."
  (when (and stories (listp stories))
    (let ((comments (org-asana--collect-comments stories))
          (history (org-asana--collect-history stories)))
      (concat (org-asana--build-comments-section comments)
              (org-asana--build-history-section history)))))

(defun org-asana--format-attachments (attachments)
  "Format ATTACHMENTS list for org-mode display."
  (let ((formatted-attachments '()))
    (when (and attachments (listp attachments))
      (dolist (attachment attachments)
        (when (listp attachment)
          (let ((gid (alist-get 'gid attachment))
                (name (alist-get 'name attachment))
                (download-url (alist-get 'download_url attachment))
                (view-url (alist-get 'view_url attachment)))
            (when name
              (push (format "- [[%s][%s]]\n  :PROPERTIES:\n  :ASANA-ATTACHMENT-GID: %s\n  :END:"
                           (or view-url download-url "#")
                           name
                           gid)
                    formatted-attachments))))))
    (concat "***** Attachments\n"
            (if formatted-attachments
                (concat (string-join (nreverse formatted-attachments) "\n\n") "\n")
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
          :due-at (alist-get 'due_at task)
          :start-on (alist-get 'start_on task)
          :start-at (alist-get 'start_at task)
          :modified-at (alist-get 'modified_at task)
          :priority (alist-get 'priority task)
          :tags (alist-get 'tags task)
          :permalink-url (let ((url (alist-get 'permalink_url task)))
                           (if (and url (string-prefix-p "/" url))
                               (concat "https://app.asana.com" url)
                             url))
          :followers (alist-get 'followers task)
          :parent (alist-get 'parent task)
          :custom-fields (alist-get 'custom_fields task)
          :dependencies (alist-get 'dependencies task)
          :dependents (alist-get 'dependents task)
          :liked (alist-get 'liked task)
          :num-likes (alist-get 'num_likes task)
          :hearted (alist-get 'hearted task)
          :num-hearts (alist-get 'num_hearts task)
          :projects (mapcar (lambda (m) (alist-get 'project m))
                           (alist-get 'memberships task))
          :comments comments
          :attachments attachments)))

(defun org-asana--apply-asana-updates (task-fields)
  "Apply TASK-FIELDS updates to current org entry."
  (org-asana--update-task-state task-fields)
  (org-asana--update-task-heading task-fields)
  (org-asana--update-task-deadline task-fields)
  (org-asana--update-task-scheduled task-fields)
  (org-asana--update-task-priority task-fields)
  (org-asana--update-task-tags task-fields)
  (org-asana--update-task-properties task-fields)
  (org-asana--update-task-timestamp task-fields))

(defun org-asana--update-task-state (task-fields)
  "Update task TODO state from TASK-FIELDS."
  (let ((completed (plist-get task-fields :completed)))
    (org-todo (org-asana--get-org-todo-state completed))))

(defun org-asana--update-task-heading (task-fields)
  "Update task heading from TASK-FIELDS."
  (let* ((task-name (plist-get task-fields :task-name))
         (permalink-url (plist-get task-fields :permalink-url))
         (linked-name (if (and permalink-url (not (string-empty-p permalink-url)))
                         (format "[[%s][%s]]" permalink-url task-name)
                       task-name)))
    (org-asana--update-heading linked-name)))

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

(defun org-asana--update-task-scheduled (task-fields)
  "Update task SCHEDULED date from TASK-FIELDS start date."
  (let ((start-on (plist-get task-fields :start-on))
        (start-at (plist-get task-fields :start-at)))
    (if (or start-on start-at)
        (let* ((date-str (or start-at start-on))
               (time-str (if start-at
                            (format-time-string "<%Y-%m-%d %a %H:%M>"
                                              (org-asana--parse-asana-timestamp date-str))
                          (format-time-string "<%Y-%m-%d %a>"
                                            (org-asana--parse-asana-timestamp date-str)))))
          (org-schedule nil time-str))
      ;; Remove SCHEDULED if no start date
      (org-schedule '(4)))))

(defun org-asana--update-permalink-property (task-fields)
  "Update permalink property from TASK-FIELDS."
  (let ((permalink (plist-get task-fields :permalink-url)))
    (when (and permalink (not (string-empty-p permalink)))
      (org-set-property "ASANA-URL" permalink))))

(defun org-asana--update-followers-property (task-fields)
  "Update followers property from TASK-FIELDS."
  (let ((followers (plist-get task-fields :followers)))
    (when followers
      (let ((follower-names (org-asana--format-follower-names followers)))
        (when (not (string-empty-p follower-names))
          (org-set-property "ASANA-FOLLOWERS" follower-names))))))

(defun org-asana--format-follower-names (followers)
  "Format FOLLOWERS list into comma-separated names."
  (mapconcat (lambda (f) (alist-get 'name f)) followers ", "))

(defun org-asana--update-parent-properties (task-fields)
  "Update parent task properties from TASK-FIELDS."
  (let ((parent (plist-get task-fields :parent)))
    (when parent
      (org-set-property "ASANA-PARENT-GID" (alist-get 'gid parent))
      (org-set-property "ASANA-PARENT-NAME" (alist-get 'name parent)))))

(defun org-asana--update-projects-property (task-fields)
  "Update projects property from TASK-FIELDS."
  (let ((projects (plist-get task-fields :projects)))
    (when projects
      (let ((project-names (org-asana--format-project-names projects)))
        (when (not (string-empty-p project-names))
          (org-set-property "ASANA-PROJECTS" project-names))))))

(defun org-asana--format-project-names (projects)
  "Format PROJECTS list into comma-separated names."
  (mapconcat (lambda (p) (alist-get 'name p)) projects ", "))

(defun org-asana--update-dependencies-property (task-fields)
  "Update dependencies property from TASK-FIELDS."
  (let ((dependencies (plist-get task-fields :dependencies)))
    (when dependencies
      (let ((dep-names (org-asana--format-dependency-names dependencies)))
        (when (not (string-empty-p dep-names))
          (org-set-property "ASANA-DEPENDENCIES" dep-names))))))

(defun org-asana--update-dependents-property (task-fields)
  "Update dependents property from TASK-FIELDS."
  (let ((dependents (plist-get task-fields :dependents)))
    (when dependents
      (let ((dep-names (org-asana--format-dependency-names dependents)))
        (when (not (string-empty-p dep-names))
          (org-set-property "ASANA-DEPENDENTS" dep-names))))))

(defun org-asana--format-dependency-names (dependencies)
  "Format DEPENDENCIES list into name(gid) pairs."
  (mapconcat
   (lambda (d) (format "%s (%s)" (alist-get 'name d) (alist-get 'gid d)))
   dependencies ", "))

(defun org-asana--update-reactions-property (task-fields)
  "Update reactions property from TASK-FIELDS."
  (let ((num-likes (plist-get task-fields :num-likes))
        (num-hearts (plist-get task-fields :num-hearts))
        (liked (plist-get task-fields :liked))
        (hearted (plist-get task-fields :hearted)))
    (when (org-asana--has-reactions-p num-likes num-hearts)
      (org-set-property "ASANA-REACTIONS"
                       (org-asana--format-reactions num-likes liked num-hearts hearted)))))

(defun org-asana--has-reactions-p (num-likes num-hearts)
  "Check if task has any reactions."
  (or (> num-likes 0) (> num-hearts 0)))

(defun org-asana--format-reactions (num-likes liked num-hearts hearted)
  "Format reactions string from counts and user status."
  (format "👍 %d%s | ❤️ %d%s"
          num-likes (if liked " (liked)" "")
          num-hearts (if hearted " (hearted)" "")))

(defun org-asana--update-custom-fields-properties (task-fields)
  "Update custom field properties from TASK-FIELDS."
  (let ((custom-fields (plist-get task-fields :custom-fields)))
    (when custom-fields
      (dolist (field custom-fields)
        (org-asana--set-custom-field-property field)))))

(defun org-asana--set-custom-field-property (field)
  "Set property for single custom FIELD."
  (let ((field-name (alist-get 'name field))
        (field-value (org-asana--extract-custom-field-value field)))
    (when (and field-name field-value)
      (org-set-property (org-asana--format-custom-field-property-name field-name)
                       field-value))))

(defun org-asana--format-custom-field-property-name (field-name)
  "Format custom FIELD-NAME into property name."
  (format "ASANA-CF-%s" (upcase (replace-regexp-in-string " " "-" field-name))))

(defun org-asana--update-task-properties (task-fields)
  "Update all task properties from TASK-FIELDS."
  (org-asana--update-permalink-property task-fields)
  (org-asana--update-followers-property task-fields)
  (org-asana--update-parent-properties task-fields)
  (org-asana--update-projects-property task-fields)
  (org-asana--update-dependencies-property task-fields)
  (org-asana--update-dependents-property task-fields)
  (org-asana--update-reactions-property task-fields)
  (org-asana--update-custom-fields-properties task-fields))

(defun org-asana--extract-custom-field-value (field)
  "Extract value from custom FIELD based on its type."
  (let ((field-type (alist-get 'type field)))
    (cond
     ((string= field-type "text")
      (alist-get 'text_value field))
     ((string= field-type "number")
      (let ((num (alist-get 'number_value field)))
        (when num (number-to-string num))))
     ((string= field-type "enum")
      (let ((enum-val (alist-get 'enum_value field)))
        (when enum-val (alist-get 'name enum-val))))
     ((string= field-type "multi_enum")
      (let ((values (alist-get 'multi_enum_values field)))
        (when values
          (mapconcat (lambda (v) (alist-get 'name v)) values ", "))))
     ((string= field-type "date")
      (let ((date-val (alist-get 'date_value field)))
        (when date-val
          (or (alist-get 'date_time date-val)
              (alist-get 'date date-val)))))
     ((string= field-type "people")
      (let ((people (alist-get 'people_value field)))
        (when people
          (mapconcat (lambda (p) (alist-get 'name p)) people ", "))))
     (t (alist-get 'display_value field)))))

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

(defun org-asana--remove-metadata-sections ()
  "Remove existing Comments, Attachments, and Activity History sections."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion (org-end-of-subtree t) (point))))
      ;; Remove each metadata section
      (dolist (section '("***** Comments" "***** Attachments" "***** Activity History"))
        (goto-char start)
        (when (re-search-forward (concat "^" (regexp-quote section) "$") end t)
          (let ((section-start (match-beginning 0))
                (section-end (save-excursion
                              (if (re-search-forward "^\\*\\*\\*\\*\\* " end t)
                                  (match-beginning 0)
                                (goto-char end)
                                ;; Skip trailing newlines
                                (skip-chars-backward "\n")
                                (forward-char)
                                (point)))))
            (delete-region section-start section-end)))))))

(defun org-asana--add-task-notes (task-fields)
  "Add notes to task from TASK-FIELDS."
  (let ((notes (plist-get task-fields :notes))
        (comments (plist-get task-fields :comments))
        (attachments (plist-get task-fields :attachments)))
    (org-end-of-meta-data t)
    ;; Remove old metadata sections before adding new ones
    (org-asana--remove-metadata-sections)
    (org-asana--insert-task-notes notes)
    (when (or attachments comments)
      (unless (bolp) (insert "\n")))
    (insert (org-asana--format-attachments attachments))
    (insert (org-asana--format-comments comments))))

(defun org-asana--extract-new-comments ()
  "Extract new comments from Comments section that don't have GIDs."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion (org-end-of-subtree t) (point)))
          (new-comments '()))
      (goto-char start)
      (when (re-search-forward "^\\*\\*\\*\\*\\* Comments$" end t)
        (let ((comments-start (point))
              (comments-end (save-excursion
                             (if (re-search-forward "^\\*\\*\\*\\*\\* " end t)
                                 (match-beginning 0)
                               end))))
          (goto-char comments-start)
          ;; Look for comments without GID properties
          (while (re-search-forward "^- \\(.+?\\): \\(.+\\)$" comments-end t)
            (let ((text (match-string 2)))
              ;; Check if this comment has a GID property
              (save-excursion
                (forward-line)
                (unless (looking-at "  :PROPERTIES:")
                  ;; This is a new comment without GID
                  (push text new-comments)))))))
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
      ;; Comments will be marked as synced when task is refetched
      (org-asana--mark-comments-synced))))

(defun org-asana--mark-comments-synced ()
  "Mark newly added comments as synced by refetching from Asana."
  ;; After creating comments, we'll refetch the task to get the GIDs
  ;; This is handled by the main sync process
  )

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
  (if org-asana-fetch-metadata
      (let ((task-ids (org-asana--extract-task-gids tasks)))
        (org-asana--fetch-task-metadata task-ids))
    ;; Return empty metadata for all tasks
    (let ((empty-results '()))
      (dolist (task tasks)
        (push (cons (alist-get 'gid task) (cons nil nil)) empty-results))
      empty-results)))

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
         (scheduled (org-entry-get nil "SCHEDULED"))
         (body (org-asana--get-task-body))
         (priority (org-entry-get nil "PRIORITY"))
         (tags (org-get-tags)))

    (when task-id
      (list :task-id task-id
            :heading heading
            :completed completed
            :deadline deadline
            :scheduled scheduled
            :body body
            :priority priority
            :tags tags))))

(defun org-asana--build-asana-update-data (org-task-data)
  "Build Asana API update data from ORG-TASK-DATA."
  (let* ((heading (plist-get org-task-data :heading))
         (completed (plist-get org-task-data :completed))
         (deadline (plist-get org-task-data :deadline))
         (scheduled (plist-get org-task-data :scheduled))
         (body (plist-get org-task-data :body))
         (priority (plist-get org-task-data :priority))
         (tags (plist-get org-task-data :tags))
         (due-date (when deadline
                    (format-time-string
                     "%Y-%m-%d"
                     (org-time-string-to-time deadline))))
         (start-date (when scheduled
                      (format-time-string
                       "%Y-%m-%d"
                       (org-time-string-to-time scheduled))))
         (asana-priority (when (and org-asana-sync-priority priority)
                          (org-asana--get-asana-priority priority))))

    `((data . ((name . ,heading)
              (notes . ,body)
              (completed . ,(if completed t :false))
              ,@(when due-date `((due_on . ,due-date)))
              ,@(when start-date `((start_on . ,start-date)))
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
        (org-asana--sync-tags-to-asana task-id (plist-get org-task-data :tags))
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
    (let ((current-heading (org-get-heading t t t t))
          (current-todo (org-get-todo-state))
          (current-priority (org-entry-get nil "PRIORITY"))
          (current-tags (org-get-tags)))
      ;; Only update if the heading text is actually different
      (unless (string= current-heading new-text)
        (org-edit-headline new-text)
        ;; Ensure TODO state is preserved or set to TODO if missing
        (unless current-todo
          (org-todo "TODO"))))))

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

(defun org-asana--sync-tags-to-asana (task-id org-tags)
  "Sync ORG-TAGS to TASK-ID in Asana."
  (when (and org-asana-sync-tags task-id org-tags)
    ;; For now, we'll just log this - full implementation would need:
    ;; 1. Fetch current tags from Asana
    ;; 2. Compare with org tags
    ;; 3. Add new tags (creating if necessary)
    ;; 4. Remove old tags
    ;; This requires workspace context and tag creation API
    (message "Tag sync to Asana not yet implemented for task %s with tags: %s"
             task-id (string-join org-tags ", "))))

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

(defun org-asana--get-capture-file ()
  "Get the org-asana file for capture templates."
  org-asana-org-file)

(defvar org-asana-capture-templates
  '(("a" "Asana Task" entry
     (file+headline org-asana--get-capture-file "Active Projects")
     "* TODO %^{Task Title}\n:PROPERTIES:\n:asana-id: new\n:asana-project: %(org-asana--select-project)\n:END:\nDEADLINE: %^t\n\n%?"
     :empty-lines-after 1)
    ("A" "Asana Task with Notes" entry
     (file+headline org-asana--get-capture-file "Active Projects")
     "* TODO %^{Task Title}\n:PROPERTIES:\n:asana-id: new\n:asana-project: %(org-asana--select-project)\n:END:\nDEADLINE: %^t\n\n%^{Task Notes}\n\n%?"
     :empty-lines-after 1))
  "Capture templates for creating new Asana tasks.")

(defun org-asana-setup-capture ()
  "Set up org-capture templates for Asana tasks."
  (setq org-capture-templates
        (append org-capture-templates org-asana-capture-templates)))

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
      (let* ((task-data (org-asana--extract-new-task-data))
             (project-gid (org-entry-get nil "asana-project"))
             (created-task (org-asana--create-task-in-asana task-data project-gid)))
        (when created-task
          (org-set-property "asana-id" (alist-get 'gid created-task))
          (org-delete-property "asana-project"))))))

(defun org-asana--extract-new-task-data ()
  "Extract task data from current org entry for new task creation."
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
