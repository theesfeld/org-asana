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
      (condition-case err
          (when (org-entry-get nil "asana-id")
            (org-asana--fontify-task))
        (error
         (when (bound-and-true-p org-asana-debug)
           (message "Error applying faces at line %d: %s"
                   (line-number-at-pos)
                   (error-message-string err))))))))

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
        (max-attempts (or max-retries org-asana-max-retries)))
    (org-asana--retry-loop method endpoint data attempt max-attempts)))

(defun org-asana--retry-loop (method endpoint data attempt max-attempts)
  "Execute retry loop for request METHOD to ENDPOINT with DATA."
  (let ((result nil)
        (success nil))
    (while (and (< attempt max-attempts) (not success))
      (condition-case err
          (setq result (org-asana--attempt-request method endpoint data)
                success t)
        (error
         (setq attempt (org-asana--handle-retry-error err attempt max-attempts endpoint)))))
    result))

(defun org-asana--attempt-request (method endpoint data)
  "Attempt single request METHOD to ENDPOINT with DATA."
  (org-asana--check-rate-limit)
  (org-asana--make-request-internal method endpoint data))

(defun org-asana--handle-retry-error (err attempt max-attempts endpoint)
  "Handle retry error ERR on ATTEMPT of MAX-ATTEMPTS to ENDPOINT."
  (let ((next-attempt (1+ attempt)))
    (if (>= next-attempt max-attempts)
        (signal (car err) (cdr err))
      (org-asana--wait-and-log-retry err next-attempt max-attempts endpoint))
    next-attempt))

(defun org-asana--wait-and-log-retry (err attempt max-attempts endpoint)
  "Wait and log retry for ERR on ATTEMPT of MAX-ATTEMPTS to ENDPOINT."
  (let ((delay (org-asana--calculate-retry-delay attempt)))
    (when org-asana-debug
      (message "DEBUG: Request to %s failed (attempt %d/%d): %s"
               endpoint attempt max-attempts (error-message-string err)))
    (message "Request failed, retrying in %d seconds..." delay)
    (sleep-for delay)))

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
    (goto-char (point-min))))

(defun org-asana--orchestrate-sync (buffer)
  "Orchestrate sync process in BUFFER."
  (with-current-buffer buffer
    (org-asana--perform-sync-operations)
    ;; Skip visual enhancements for now
    ;; (org-asana--apply-visual-enhancements)
    (save-buffer)))


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

(defun org-asana--extract-task-location (task-info)
  "Extract start, end, and content from TASK-INFO."
  (list :start (nth 0 task-info)
        :end (nth 1 task-info)
        :content (nth 2 task-info)))

(defun org-asana--extract-task-project (task-info)
  "Extract project name from TASK-INFO."
  (car (nth 3 task-info)))

(defun org-asana--extract-task-section (task-info)
  "Extract section name from TASK-INFO."
  (let ((sec-data (cdr (nth 3 task-info))))
    (cond
     ((stringp sec-data) sec-data)
     ((and (listp sec-data) (alist-get 'name sec-data))
      (alist-get 'name sec-data))
     ((listp sec-data) (car sec-data))
     (t (format "%s" sec-data)))))

(defun org-asana--delete-task-region (start end)
  "Delete task region from START to END."
  (delete-region start end))

(defun org-asana--insert-task-in-completed (content section-pos)
  "Insert task CONTENT at completed SECTION-POS."
  (save-excursion
    (goto-char section-pos)
    (org-end-of-subtree t)
    (insert content)
    (org-asana--add-completion-timestamp)))

(defun org-asana--move-task-to-completed (task-info)
  "Move TASK-INFO to COMPLETED section."
  (let* ((location (org-asana--extract-task-location task-info))
         (project (org-asana--extract-task-project task-info))
         (section (org-asana--extract-task-section task-info)))
    (org-asana--delete-task-region (plist-get location :start)
                                   (plist-get location :end))
    (let* ((project-pos (org-asana--find-or-create-completed-project project))
           (section-pos (org-asana--find-or-create-completed-section section project-pos)))
      (org-asana--insert-task-in-completed (plist-get location :content) section-pos))))

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

(defun org-asana--handle-pagination-error (error-count _max-errors page-count err)
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

(defun org-asana--init-pagination-state ()
  "Initialize pagination state variables."
  (list :all-data '()
        :next-page nil
        :page-count 0
        :error-count 0
        :max-errors 3))

(defun org-asana--should-continue-pagination-p (state)
  "Check if pagination should continue based on STATE."
  (and (plist-get state :next-page)
       (< (plist-get state :error-count) 
          (plist-get state :max-errors))))

(defun org-asana--update-pagination-state (state result)
  "Update pagination STATE with RESULT values."
  (plist-put state :all-data (nth 0 result))
  (plist-put state :next-page (nth 1 result))
  (plist-put state :page-count (nth 2 result))
  (plist-put state :error-count (nth 3 result))
  state)

(defun org-asana--fetch-page-with-error-handling (state)
  "Fetch next page and handle errors for STATE."
  (condition-case err
      (org-asana--process-page-success
       (org-asana--fetch-single-page (plist-get state :next-page))
       (plist-get state :all-data)
       (plist-get state :page-count))
    (error
     (org-asana--process-page-error 
      err 
      (plist-get state :error-count)
      (plist-get state :max-errors)
      (plist-get state :page-count)
      (plist-get state :all-data)))))

(defun org-asana--fetch-paginated (endpoint)
  "Fetch all pages from ENDPOINT."
  (let ((state (org-asana--init-pagination-state)))
    (plist-put state :next-page endpoint)
    (while (org-asana--should-continue-pagination-p state)
      (setq state (org-asana--update-pagination-state 
                   state 
                   (org-asana--fetch-page-with-error-handling state))))
    (org-asana--report-pagination-completion 
     (plist-get state :error-count)
     (length (plist-get state :all-data)))
    (plist-get state :all-data)))



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



(defun org-asana--sync-done-tasks ()
  "Find DONE tasks, sync to Asana, and move to COMPLETED section."
  (let ((done-tasks (org-asana--find-done-tasks)))
    (dolist (task-info done-tasks)
      (org-asana--move-task-to-completed task-info))))

(defun org-asana--extract-basic-fields (task)
  "Extract basic fields from TASK."
  (list :task-id (alist-get 'gid task)
        :task-name (alist-get 'name task)
        :completed (alist-get 'completed task)
        :notes (alist-get 'notes task)
        :modified-at (alist-get 'modified_at task)
        :priority (alist-get 'priority task)
        :tags (alist-get 'tags task)))

(defun org-asana--extract-date-fields (task)
  "Extract date-related fields from TASK."
  (list :due-on (alist-get 'due_on task)
        :due-at (alist-get 'due_at task)
        :start-on (alist-get 'start_on task)
        :start-at (alist-get 'start_at task)))

(defun org-asana--extract-relationship-fields (task)
  "Extract relationship fields from TASK."
  (list :followers (alist-get 'followers task)
        :parent (alist-get 'parent task)
        :dependencies (alist-get 'dependencies task)
        :dependents (alist-get 'dependents task)
        :projects (mapcar (lambda (m) (alist-get 'project m))
                         (alist-get 'memberships task))))

(defun org-asana--extract-reaction-fields (task)
  "Extract reaction fields from TASK."
  (list :liked (alist-get 'liked task)
        :num-likes (alist-get 'num_likes task)
        :hearted (alist-get 'hearted task)
        :num-hearts (alist-get 'num_hearts task)))

(defun org-asana--format-permalink-url (task)
  "Format permalink URL from TASK."
  (let ((url (alist-get 'permalink_url task)))
    (if (and url (string-prefix-p "/" url))
        (concat "https://app.asana.com" url)
      url)))

(defun org-asana--extract-metadata-from-batch (task-id batch-data)
  "Extract comments and attachments for TASK-ID from BATCH-DATA."
  (let ((task-data (alist-get task-id batch-data)))
    (list :comments (car task-data)
          :attachments (cdr task-data))))

(defun org-asana--extract-task-fields (task batch-data)
  "Extract and organize fields from TASK using BATCH-DATA."
  (let ((task-id (alist-get 'gid task)))
    (append (org-asana--extract-basic-fields task)
            (org-asana--extract-date-fields task)
            (org-asana--extract-relationship-fields task)
            (org-asana--extract-reaction-fields task)
            (list :permalink-url (org-asana--format-permalink-url task)
                  :custom-fields (alist-get 'custom_fields task))
            (org-asana--extract-metadata-from-batch task-id batch-data))))

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
    (when (org-at-heading-p)
      (org-todo (org-asana--get-org-todo-state completed)))))

(defun org-asana--update-task-heading (task-fields)
  "Update task heading from TASK-FIELDS."
  (let* ((raw-task-name (plist-get task-fields :task-name))
         (clean-task-name (org-asana--strip-org-links raw-task-name))
         (permalink-url (plist-get task-fields :permalink-url))
         (linked-name (if (and permalink-url (not (string-empty-p permalink-url)))
                         (format "[[%s][%s]]" permalink-url clean-task-name)
                       clean-task-name)))
    (org-asana--update-heading linked-name)))

(defun org-asana--update-task-deadline (task-fields)
  "Update task deadline from TASK-FIELDS."
  (let ((due-on (plist-get task-fields :due-on)))
    (when due-on
      (org-asana--set-deadline due-on))))

(defun org-asana--set-deadline (due-on)
  "Set deadline to DUE-ON date."
  (when (org-at-heading-p)
    (let ((formatted-date (org-asana--format-asana-date due-on)))
      (when formatted-date
        (org-deadline nil formatted-date)))))

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

(defun org-asana--update-assignee-property (task-fields)
  "Update assignee property from TASK-FIELDS."
  (let ((created-by (plist-get task-fields :created-by))
        (created-at (plist-get task-fields :created-at)))
    (when created-by
      (let ((creator-name (alist-get 'name created-by)))
        (when creator-name
          (org-set-property "ASANA-CREATED-BY" creator-name))))
    (when created-at
      (org-set-property "ASANA-CREATED-AT" created-at))))

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
  ;; Ensure we're at a valid heading
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  ;; Set the task GID first
  (let ((task-gid (plist-get task-fields :task-id)))
    (when task-gid
      (org-set-property "ASANA-TASK-GID" task-gid)))
  (org-asana--update-permalink-property task-fields)
  (org-asana--update-assignee-property task-fields)
  (org-asana--update-followers-property task-fields)
  (org-asana--update-parent-properties task-fields)
  (org-asana--update-projects-property task-fields)
  (org-asana--update-dependencies-property task-fields)
  (org-asana--update-dependents-property task-fields)
  (org-asana--update-reactions-property task-fields)
  (org-asana--update-custom-fields-properties task-fields))

(defun org-asana--extract-text-field-value (field)
  "Extract text value from FIELD."
  (alist-get 'text_value field))

(defun org-asana--extract-number-field-value (field)
  "Extract number value from FIELD as string."
  (let ((num (alist-get 'number_value field)))
    (when num (number-to-string num))))

(defun org-asana--extract-enum-field-value (field)
  "Extract enum value name from FIELD."
  (let ((enum-val (alist-get 'enum_value field)))
    (when enum-val (alist-get 'name enum-val))))

(defun org-asana--extract-multi-enum-field-value (field)
  "Extract multi-enum values from FIELD as comma-separated string."
  (let ((values (alist-get 'multi_enum_values field)))
    (when values
      (mapconcat (lambda (v) (alist-get 'name v)) values ", "))))

(defun org-asana--extract-date-field-value (field)
  "Extract date value from FIELD."
  (let ((date-val (alist-get 'date_value field)))
    (when date-val
      (or (alist-get 'date_time date-val)
          (alist-get 'date date-val)))))

(defun org-asana--extract-people-field-value (field)
  "Extract people values from FIELD as comma-separated string."
  (let ((people (alist-get 'people_value field)))
    (when people
      (mapconcat (lambda (p) (alist-get 'name p)) people ", "))))

(defun org-asana--extract-custom-field-value (field)
  "Extract value from custom FIELD based on its type."
  (let ((field-type (alist-get 'type field)))
    (cond
     ((string= field-type "text") (org-asana--extract-text-field-value field))
     ((string= field-type "number") (org-asana--extract-number-field-value field))
     ((string= field-type "enum") (org-asana--extract-enum-field-value field))
     ((string= field-type "multi_enum") (org-asana--extract-multi-enum-field-value field))
     ((string= field-type "date") (org-asana--extract-date-field-value field))
     ((string= field-type "people") (org-asana--extract-people-field-value field))
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
  "Insert NOTES text if not empty, stripping metadata sections."
  (when (and notes (not (string-empty-p notes)))
    (let ((clean-notes (org-asana--strip-metadata-sections notes)))
      (when (and clean-notes (not (string-empty-p clean-notes)))
        (insert clean-notes "\n")))))

(defun org-asana--remove-metadata-sections ()
  "Remove existing Comments, Attachments, and Activity History sections."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion (org-end-of-subtree t) (point))))
      ;; Remove each metadata section
      (dolist (section '("***** Comments" "***** Attachments" "***** Activity History"))
        (goto-char start)
        (when (and (< (point) end)
                   (re-search-forward (concat "^" (regexp-quote section) "$") end t))
          (let ((section-start (match-beginning 0))
                (section-end (save-excursion
                              (if (and (< (point) end)
                                       (re-search-forward "^\\*\\*\\*\\*\\* " end t))
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
    (when org-asana-debug
      (message "Adding notes - notes: %s comments: %s attachments: %s"
               (if (and notes (not (string-empty-p notes))) "yes" "no")
               (if comments "yes" "no")
               (if attachments "yes" "no")))
    ;; Ensure we're at a valid heading
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (org-end-of-meta-data t)
    ;; Remove old metadata sections before adding new ones
    (org-asana--remove-metadata-sections)
    (org-asana--insert-task-notes notes)
    (when (or attachments comments)
      (unless (bolp) (insert "\n")))
    (insert (org-asana--format-attachments attachments))
    (insert (org-asana--format-comments comments))))

(defun org-asana--get-task-region ()
  "Get start and end positions of current task."
  (save-excursion
    (org-back-to-heading t)
    (list :start (point)
          :end (save-excursion (org-end-of-subtree t) (point)))))

(defun org-asana--find-comments-section ()
  "Find Comments section and return its boundaries."
  (let ((region (org-asana--get-task-region)))
    (save-excursion
      (goto-char (plist-get region :start))
      (when (re-search-forward "^\\*\\*\\*\\*\\* Comments$" 
                               (plist-get region :end) t)
        (list :start (point)
              :end (save-excursion
                     (if (re-search-forward "^\\*\\*\\*\\*\\* " 
                                            (plist-get region :end) t)
                         (match-beginning 0)
                       (plist-get region :end))))))))

(defun org-asana--has-comment-gid-p ()
  "Check if current comment has a GID property."
  (save-excursion
    (forward-line)
    (looking-at "  :PROPERTIES:")))

(defun org-asana--extract-comment-text ()
  "Extract comment text from current line."
  (when (looking-at "^- \\(.+?\\): \\(.+\\)$")
    (match-string 2)))

(defun org-asana--collect-new-comments-in-region (start end)
  "Collect new comments without GIDs between START and END."
  (let ((new-comments '()))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^- \\(.+?\\): \\(.+\\)$" end t)
        (let ((text (match-string 2)))
          (unless (org-asana--has-comment-gid-p)
            (push text new-comments)))))
    (nreverse new-comments)))

(defun org-asana--extract-new-comments ()
  "Extract new comments from Comments section that don't have GIDs."
  (let ((comments-section (org-asana--find-comments-section)))
    (if comments-section
        (org-asana--collect-new-comments-in-region 
         (plist-get comments-section :start)
         (plist-get comments-section :end))
      '())))

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



(defun org-asana--sync-from-asana ()
  "Sync tasks from Asana to org using optimized approach."
  (message "Fetching all task data in single call...")
  (let* ((rich-tasks (org-asana--fetch-all-task-data))
         (task-count (length rich-tasks)))
    (message "Fetched %d tasks" task-count)
    (if org-asana-fetch-metadata
        (progn
          (message "Fetching metadata for all tasks in single batch...")
          (let ((task-metadata (org-asana--fetch-all-metadata-batch rich-tasks)))
            (message "Processing tasks with metadata...")
            (org-asana--process-rich-tasks rich-tasks task-metadata)))
      (message "Processing tasks without metadata...")
      (org-asana--process-rich-tasks rich-tasks nil))))

(defun org-asana--fetch-all-task-data ()
  "Fetch all task data in single comprehensive call."
  (let* ((workspace-info (org-asana--get-workspace-info))
         (user-gid (car workspace-info))
         (workspace-gid (cadr workspace-info))
         (opt-fields "gid,name,notes,completed,due_on,due_at,start_on,start_at,created_at,modified_at,created_by.name,priority,tags.gid,tags.name,memberships.project.gid,memberships.project.name,memberships.section.name,permalink_url,followers.gid,followers.name,parent.gid,parent.name,custom_fields,dependencies.gid,dependencies.name,dependents.gid,dependents.name")
         (opt-expand "assignee,projects,followers,memberships.project,memberships.section"))
    (org-asana--fetch-paginated
     (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s&opt_expand=%s"
             workspace-gid user-gid opt-fields opt-expand))))

(defun org-asana--fetch-all-metadata-batch (tasks)
  "Fetch all stories and attachments in single batch call."
  (when (and org-asana-fetch-metadata tasks)
    (let ((task-ids (mapcar (lambda (task) (alist-get 'gid task)) tasks)))
      (org-asana--execute-metadata-batch task-ids))))

(defun org-asana--execute-metadata-batch (task-ids)
  "Execute metadata batch request for TASK-IDS."
  (let ((all-results '())
        (total-tasks (length task-ids))
        (processed 0))
    ;; Process in batches of 5 tasks (10 actions per batch: 5 stories + 5 attachments)
    (while task-ids
      (let* ((batch-task-ids (seq-take task-ids 5))
             (remaining-task-ids (seq-drop task-ids 5))
             (actions (org-asana--build-metadata-batch-actions batch-task-ids))
             (action-count (length actions))
             (batch-count (length batch-task-ids)))
        (message "Building batch request with %d actions for %d tasks..." action-count batch-count)
        (when actions
          (let ((batch-results (org-asana--process-batch-request actions batch-task-ids)))
            (when batch-results
              (setq all-results (append all-results batch-results)))))
        (setq processed (+ processed batch-count))
        (message "Processed %d/%d tasks" processed total-tasks)
        (setq task-ids remaining-task-ids)))
    all-results))

(defun org-asana--process-batch-request (actions task-ids)
  "Process batch request with ACTIONS for TASK-IDS."
  (condition-case err
      (let* ((batch-response (org-asana--make-batch-request actions))
             (responses (alist-get 'data batch-response)))
        (message "Batch response received, processing %d responses..." (length responses))
        (org-asana--process-metadata-batch-responses responses task-ids))
    (error
     (message "Batch metadata fetch failed: %s" (error-message-string err))
     nil)))

(defun org-asana--build-metadata-batch-actions (task-ids)
  "Build batch actions for all task stories and attachments."
  (let ((actions '()))
    (dolist (task-id task-ids)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/stories" task-id))
              (options . ((opt_fields . "text,created_at,created_by.name,type,resource_subtype"))))
            actions)
      (push `((method . "GET")
              (relative_path . ,(format "/tasks/%s/attachments" task-id))
              (options . ((opt_fields . "name,download_url,view_url,gid"))))
            actions))
    (nreverse actions)))

(defun org-asana--process-metadata-batch-responses (responses task-ids)
  "Process batch responses into task-id -> (stories . attachments) alist."
  (let ((results '())
        (response-index 0))
    (dolist (task-id task-ids)
      ;; Each task has 2 responses: stories then attachments
      (let* ((stories-response (nth response-index responses))
             (attachments-response (nth (1+ response-index) responses))
             (stories (when (eq (alist-get 'status_code stories-response) 200)
                       (alist-get 'data (alist-get 'body stories-response))))
             (attachments (when (eq (alist-get 'status_code attachments-response) 200)
                           (alist-get 'data (alist-get 'body attachments-response)))))
        (push (cons task-id (cons stories attachments)) results)
        (setq response-index (+ response-index 2))))
    (nreverse results)))

(defun org-asana--process-rich-tasks (rich-tasks task-metadata)
  "Process rich task data with metadata in single pass."
  (save-excursion
    (when org-asana-debug
      (message "Processing %d rich tasks with %d metadata entries" 
               (length rich-tasks) (length task-metadata)))
    ;; Sort tasks by creation date (oldest first) before processing
    (let ((sorted-tasks (sort (copy-sequence rich-tasks)
                              (lambda (task-a task-b)
                                (let ((created-a (alist-get 'created_at task-a))
                                      (created-b (alist-get 'created_at task-b)))
                                  (if (and created-a created-b)
                                      (string< created-a created-b)
                                    nil))))))
      ;; Process sorted tasks
      (dolist (task sorted-tasks)
        (org-asana--process-single-rich-task task task-metadata)))
    ;; Clean up and update statistics
    (org-asana--cleanup-empty-sections)
    (org-asana--update-statistics)))

(defun org-asana--process-single-rich-task (task task-metadata)
  "Process a single rich TASK with TASK-METADATA."
  (let* ((_ (alist-get 'gid task))
         (task-name (alist-get 'name task))
         (memberships (alist-get 'memberships task))
         (membership (car memberships))
         (project (alist-get 'project membership))
         (section (alist-get 'section membership))
         (project-name (alist-get 'name project))
         (section-name (alist-get 'name section))
         (project-gid (alist-get 'gid project))
         (section-gid (alist-get 'gid section)))
    (when org-asana-debug
      (message "Task: %s, Project: %s, Section: %s, Completed: %s" 
               task-name project-name section-name (alist-get 'completed task)))
    (when (and project-name section-name)
      (let* ((project-pos (org-asana--find-or-create-project-by-gid project-gid project-name))
             (section-pos (org-asana--find-or-create-section-by-gid section-gid section-name project-pos)))
        (when section-pos
          (org-asana--update-or-create-rich-task task section-pos task-metadata))))))

(defun org-asana--update-or-create-rich-task (task section-pos task-metadata)
  "Update or create rich TASK at SECTION-POS with TASK-METADATA."
  (let* ((task-id (alist-get 'gid task))
         (task-name (alist-get 'name task))
         (completed (alist-get 'completed task))
         (metadata (alist-get task-id task-metadata)))
    (when org-asana-debug
      (message "Task %s: completed=%s, processing=%s" task-name completed (not completed)))
    (unless (eq completed :true) ; Only process incomplete tasks
      (when org-asana-debug
        (message "Processing task: %s (ID: %s)" task-name task-id))
      (let ((task-pos (or (org-asana--find-task-by-gid task-id section-pos)
                           (org-asana--create-task-heading task-name section-pos))))
        (if task-pos
            (progn
              (when org-asana-debug
                (message "Found/created task at pos: %s" task-pos))
              (save-excursion
                (goto-char task-pos)
                (org-asana--update-rich-task-properties task metadata)))
          (when org-asana-debug
            (message "Failed to find/create task: %s" task-name)))))))

(defun org-asana--extract-rich-task-data (task metadata)
  "Extract task data from TASK and METADATA."
  (list :task-id (alist-get 'gid task)
        :due-on (alist-get 'due_on task)
        :created-at (alist-get 'created_at task)
        :modified-at (alist-get 'modified_at task)
        :creator-name (alist-get 'name (alist-get 'created_by task))
        :followers (alist-get 'followers task)
        :notes (alist-get 'notes task)
        :stories (car metadata)
        :attachments (cdr metadata)))

(defun org-asana--set-task-deadline-safe (due-on)
  "Set task deadline to DUE-ON safely."
  (when (and due-on (org-at-heading-p))
    (condition-case nil
        (org-deadline nil due-on)
      (error
       (when org-asana-debug
         (message "Failed to set deadline: %s" due-on))))))

(defun org-asana--set-basic-task-properties (task-data)
  "Set basic properties from TASK-DATA."
  (when (org-at-heading-p)
    (when org-asana-debug
      (message "Setting properties for task at heading: %s" 
               (org-get-heading t t t t)))
    (org-set-property "ASANA-TASK-GID" (plist-get task-data :task-id))
    (when-let ((modified-at (plist-get task-data :modified-at)))
      (org-set-property "ASANA-MODIFIED-AT" modified-at))
    (when-let ((created-at (plist-get task-data :created-at)))
      (when org-asana-debug
        (message "Setting ASANA-CREATED-AT: %s" created-at))
      (org-set-property "ASANA-CREATED-AT" created-at))
    (when-let ((creator (plist-get task-data :creator-name)))
      (org-set-property "ASANA-CREATED-BY" creator))))

(defun org-asana--set-follower-property (followers)
  "Set follower property from FOLLOWERS list."
  (when (and followers (org-at-heading-p))
    (let ((names (mapcar (lambda (f) (alist-get 'name f)) followers)))
      (org-set-property "ASANA-FOLLOWERS" (string-join names ", ")))))

(defun org-asana--add-task-metadata-notes (task-data)
  "Add notes/comments from TASK-DATA if present."
  (let ((stories (plist-get task-data :stories))
        (attachments (plist-get task-data :attachments)))
    (when (or stories attachments)
      (let ((fields (list :notes (plist-get task-data :notes)
                         :comments stories
                         :attachments attachments)))
        (org-asana--add-task-notes fields)))))

(defun org-asana--update-rich-task-properties (task metadata)
  "Update task properties from rich TASK data and METADATA."
  (let ((task-data (org-asana--extract-rich-task-data task metadata)))
    (org-asana--set-task-deadline-safe (plist-get task-data :due-on))
    (org-asana--set-basic-task-properties task-data)
    (org-asana--set-follower-property (plist-get task-data :followers))
    (org-asana--add-task-metadata-notes task-data)))


(defun org-asana--should-process-task-p (task)
  "Check if TASK should be processed (not completed)."
  (org-asana--json-false-p (alist-get 'completed task)))

(defun org-asana--extract-task-metadata (task-gid task-metadata)
  "Extract metadata for TASK-GID from TASK-METADATA."
  (let ((entry (assoc task-gid task-metadata)))
    (when entry
      (list :stories (cadr entry)
            :attachments (cddr entry)))))

(defun org-asana--find-or-create-task-heading (task-gid task-name section-pos)
  "Find existing task by TASK-GID or create new heading with TASK-NAME at SECTION-POS."
  (or (org-asana--find-task-by-gid task-gid section-pos)
      (org-asana--create-task-heading 
       (org-asana--strip-org-links task-name) section-pos)))

(defun org-asana--format-task-heading-with-link (task-fields)
  "Format task heading with permalink from TASK-FIELDS."
  (let* ((raw-name (plist-get task-fields :task-name))
         (clean-name (org-asana--strip-org-links raw-name))
         (permalink (plist-get task-fields :permalink-url)))
    (if (string-match "\\[\\[.*\\]\\]" raw-name)
        raw-name  ; Already linked
      (if (and permalink (not (string-empty-p permalink)))
          (format "[[%s][%s]]" permalink clean-name)
        clean-name))))

(defun org-asana--set-new-task-deadline (task-fields)
  "Set deadline for new task from TASK-FIELDS."
  (let ((due-on (plist-get task-fields :due-on)))
    (when (and due-on (not (org-asana--json-null-p due-on))
               (org-at-heading-p))
      (org-deadline nil (concat "<" due-on ">")))))

(defun org-asana--initialize-new-task (task-fields)
  "Initialize new task with TASK-FIELDS."
  (org-edit-headline (org-asana--format-task-heading-with-link task-fields))
  (org-asana--set-new-task-deadline task-fields)
  (org-asana--update-task-properties task-fields)
  (org-asana--add-task-notes task-fields))

(defun org-asana--process-task-at-position (task-fields existing-pos)
  "Process task with TASK-FIELDS at position, updating if EXISTING-POS."
  (if existing-pos
      (org-asana--update-rich-task task-fields)
    (org-asana--initialize-new-task task-fields)))

(defun org-asana--process-rich-single-task (task task-metadata section-pos)
  "Process single rich TASK with TASK-METADATA at SECTION-POS."
  (unless section-pos
    (error "org-asana--process-rich-single-task: section-pos is nil"))
  (when (org-asana--should-process-task-p task)
    (let* ((task-gid (alist-get 'gid task))
           (task-name (alist-get 'name task))
           (metadata (org-asana--extract-task-metadata task-gid task-metadata))
           (task-fields (org-asana--extract-rich-task-fields 
                        task (plist-get metadata :stories) 
                        (plist-get metadata :attachments)))
           (existing-pos (org-asana--find-task-by-gid task-gid section-pos))
           (heading-pos (org-asana--find-or-create-task-heading 
                        task-gid task-name section-pos)))
      (when heading-pos
        (save-excursion
          (goto-char heading-pos)
          (org-asana--process-task-at-position task-fields existing-pos))))))

(defun org-asana--extract-rich-task-fields (task stories attachments)
  "Extract task fields from rich TASK data with STORIES and ATTACHMENTS."
  (list :task-id (alist-get 'gid task)
        :task-name (alist-get 'name task)
        :permalink-url (alist-get 'permalink_url task)
        :completed (alist-get 'completed task)
        :due-on (alist-get 'due_on task)
        :start-on (alist-get 'start_on task)
        :created-at (alist-get 'created_at task)
        :modified-at (alist-get 'modified_at task)
        :priority (alist-get 'priority task)
        :notes (alist-get 'notes task)
        :comments stories
        :attachments attachments
        :tags (alist-get 'tags task)
        :created-by (alist-get 'created_by task)
        :followers (alist-get 'followers task)))

(defun org-asana--update-rich-task (task-fields)
  "Update task using rich TASK-FIELDS data."
  (condition-case err
      (progn
        (org-asana--update-task-heading task-fields)
        (org-asana--update-task-state task-fields)
        (org-asana--update-task-deadline task-fields)
        (org-asana--update-task-priority task-fields)
        (org-asana--update-task-properties task-fields)
        (org-asana--add-task-notes task-fields))
    (error
     (message "Error updating task %s: %s" 
              (plist-get task-fields :task-id) 
              (error-message-string err)))))


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

(defun org-asana--format-date-for-api (date-string)
  "Format DATE-STRING to YYYY-MM-DD for Asana API."
  (when date-string
    (format-time-string "%Y-%m-%d" 
                       (org-time-string-to-time date-string))))

(defun org-asana--get-sync-priority (priority)
  "Get Asana priority value if PRIORITY sync is enabled."
  (when (and org-asana-sync-priority priority)
    (org-asana--get-asana-priority priority)))

(defun org-asana--build-basic-update-data (org-task-data)
  "Build basic update data from ORG-TASK-DATA."
  (let ((heading (plist-get org-task-data :heading))
        (body (plist-get org-task-data :body))
        (completed (plist-get org-task-data :completed)))
    `((name . ,(org-asana--strip-org-links heading))
      (notes . ,body)
      (completed . ,(if completed t :false)))))

(defun org-asana--build-date-update-data (org-task-data)
  "Build date-related update data from ORG-TASK-DATA."
  (let ((due-date (org-asana--format-date-for-api 
                   (plist-get org-task-data :deadline)))
        (start-date (org-asana--format-date-for-api 
                     (plist-get org-task-data :scheduled))))
    (append (when due-date `((due_on . ,due-date)))
            (when start-date `((start_on . ,start-date))))))

(defun org-asana--build-priority-update-data (org-task-data)
  "Build priority update data from ORG-TASK-DATA."
  (let ((priority (org-asana--get-sync-priority 
                   (plist-get org-task-data :priority))))
    (when priority `((priority . ,priority)))))

(defun org-asana--build-asana-update-data (org-task-data)
  "Build Asana API update data from ORG-TASK-DATA."
  `((data . ,(append (org-asana--build-basic-update-data org-task-data)
                     (org-asana--build-date-update-data org-task-data)
                     (org-asana--build-priority-update-data org-task-data)))))

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


;;; Data Transformation Functions

(defun org-asana--json-null-p (value)
  "Check if VALUE represents JSON null."
  (or (eq value :null) (eq value nil)))


(defun org-asana--calculate-retry-delay (attempt)
  "Calculate exponential backoff delay for ATTEMPT."
  (* attempt attempt 2))


(defun org-asana--strip-org-links (text)
  "Strip org-mode links from TEXT, returning only the display text.
Converts [[url][display]] to display, [[url]] to url."
  (when text
    (let ((result text))
      (while (string-match "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" result)
        (setq result (replace-match (match-string 2 result) nil nil result)))
      (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" result)
        (setq result (replace-match (match-string 1 result) nil nil result)))
      result)))

(defun org-asana--strip-metadata-sections (text)
  "Strip metadata sections from TEXT to prevent duplication."
  (when text
    ;; If the text starts with a metadata section marker, it's probably
    ;; not actual notes but metadata that was stored in the notes field
    (if (string-match-p "^\\*\\*\\*\\*\\* " text)
        ""  ; Return empty string for pure metadata
      text)))  ; Return text as-is if it's actual notes

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


(defun org-asana--find-or-create-project-by-gid (gid name)
  "Find or create project by GID and NAME."
  (save-excursion
    (goto-char (point-min))
    ;; Search for existing project by GID
    (let ((found-pos nil))
      (while (and (not found-pos)
                  (re-search-forward "^\\*\\* " nil t))
        (when (string= (org-entry-get nil "ASANA-PROJECT-GID") gid)
          (setq found-pos (point-at-bol))))
      (if found-pos
          found-pos
        ;; Create new project at end of buffer
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "** %s [/]\n" name))
        (forward-line -1)
        (org-set-property "ASANA-PROJECT-GID" gid)
        (point-at-bol)))))

(defun org-asana--find-or-create-section-by-gid (gid name project-pos)
  "Find or create section by GID and NAME under PROJECT-POS."
  (save-excursion
    (goto-char project-pos)
    (let ((end-pos (save-excursion (org-end-of-subtree t) (point)))
          (found-pos nil))
      ;; Search within project for section by GID
      (while (and (not found-pos)
                  (< (point) end-pos)
                  (re-search-forward "^\\*\\*\\* " end-pos t))
        (when (string= (org-entry-get nil "ASANA-SECTION-GID") gid)
          (setq found-pos (point-at-bol))))
      (if found-pos
          found-pos
        ;; Create new section at end of project
        (goto-char end-pos)
        (unless (bolp) (insert "\n"))
        (insert (format "*** %s [/]\n" name))
        (forward-line -1)
        (org-set-property "ASANA-SECTION-GID" gid)
        (point-at-bol)))))

(defun org-asana--find-or-create-heading (level heading parent-pos)
  "Find or create HEADING at LEVEL after PARENT-POS."
  (unless parent-pos
    (error "org-asana--find-or-create-heading: parent-pos is nil for heading '%s'" heading))
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
          (progn
            (beginning-of-line)
            (point))
        (goto-char end-pos)
        (unless (bolp) (insert "\n"))
        (insert (format "%s %s\n" stars heading-with-stats))
        (forward-line -1)
        (org-back-to-heading t)
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

(defun org-asana--find-task-by-gid (task-gid parent-pos)
  "Find task with TASK-GID in subtree starting at PARENT-POS."
  (when (and parent-pos task-gid)
    (save-excursion
      (goto-char parent-pos)
      ;; Make sure we're at a valid position
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (let ((found-pos nil)
            (end-pos (save-excursion (org-end-of-subtree t t) (point))))
        (while (and (not found-pos)
                    (< (point) end-pos)
                    (re-search-forward "^\\*\\*\\*\\* " end-pos t))
          (beginning-of-line)
          (when (string= (org-entry-get nil "ASANA-TASK-GID") task-gid)
            (setq found-pos (point)))
          (end-of-line))
        found-pos))))

(defun org-asana--create-task-heading (task-name parent-pos)
  "Create new task heading with TASK-NAME under PARENT-POS."
  (unless parent-pos
    (error "org-asana--create-task-heading: parent-pos is nil for task '%s'" task-name))
  (save-excursion
    (goto-char parent-pos)
    ;; Make sure we're at a valid position
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (org-end-of-subtree t)
    (unless (bolp) (insert "\n"))
    (insert "**** TODO " task-name "\n")
    (forward-line -1)
    (beginning-of-line)
    (point)))

(defun org-asana--update-heading (new-text)
  "Update current heading text to NEW-TEXT."
  (condition-case nil
      (save-excursion
        (org-back-to-heading t)
        (let ((current-heading (org-get-heading t t t t))
              (current-todo (org-get-todo-state))
              (_current-priority (org-entry-get nil "PRIORITY"))
              (_current-tags (org-get-tags)))
          ;; Only update if the heading text is actually different
          (unless (string= current-heading new-text)
            (org-edit-headline new-text)
            ;; Ensure TODO state is preserved or set to TODO if missing
            (unless current-todo
              (org-todo "TODO")))))
    (error nil)))

(defun org-asana--get-task-body ()
  "Get the body text of current task, excluding metadata sections."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let* ((start (point))
           (subtree-end (save-excursion
                         (org-end-of-subtree t)
                         (point)))
           (metadata-start (save-excursion
                            (when (and (< (point) subtree-end)
                                      (re-search-forward "^\\*\\{5\\} " subtree-end t))
                              (line-beginning-position))))
           (end (or metadata-start subtree-end)))
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
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (org-update-statistics-cookies t))
    (error nil)))

(defun org-asana--cleanup-empty-sections ()
  "Remove empty sections and projects."
  ;; Skip cleanup for now - it may be causing issues
  nil)



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
