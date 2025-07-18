;;; org-asana-users.el --- User and assignee management for org-asana -*- lexical-binding: t; -*-

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

;; This module provides user and assignee management for org-asana.
;; Handles fetching users, updating assignees, and user caching.

;;; Code:

(require 'org-asana)

;;; Cache Variables

(defvar org-asana--users-cache nil
  "Cache for workspace users.")

(defvar org-asana--users-cache-time nil
  "Time when users cache was last updated.")

(defconst org-asana--users-cache-ttl 3600
  "TTL for users cache in seconds.")

;;; API Functions

(defun org-asana--fetch-workspace-users (workspace-gid)
  "Fetch all users in WORKSPACE-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/workspaces/%s/users?opt_fields=name,email"
                                           workspace-gid))))

(defun org-asana--fetch-team-users (team-gid)
  "Fetch all users in TEAM-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/teams/%s/users?opt_fields=name,email"
                                           team-gid))))

(defun org-asana--fetch-current-user ()
  "Fetch current authenticated user."
  (alist-get 'data
             (org-asana--make-request "GET" "/users/me")))

(defun org-asana--update-task-assignee (task-gid user-gid)
  "Update TASK-GID assignee to USER-GID."
  (org-asana--make-request
   "PUT"
   (format "/tasks/%s" task-gid)
   `((data . ((assignee . ,user-gid))))))

(defun org-asana--remove-task-assignee (task-gid)
  "Remove assignee from TASK-GID."
  (org-asana--make-request
   "PUT"
   (format "/tasks/%s" task-gid)
   '((data . ((assignee . nil))))))

;;; Cache Functions

(defun org-asana--users-cache-valid-p ()
  "Return t if users cache is still valid."
  (and org-asana--users-cache
       org-asana--users-cache-time
       (< (float-time (time-subtract (current-time)
                                    org-asana--users-cache-time))
          org-asana--users-cache-ttl)))

(defun org-asana--get-cached-users (workspace-gid)
  "Get users for WORKSPACE-GID, using cache if valid."
  (if (org-asana--users-cache-valid-p)
      org-asana--users-cache
    (let ((users (org-asana--fetch-workspace-users workspace-gid)))
      (setq org-asana--users-cache users
            org-asana--users-cache-time (current-time))
      users)))

(defun org-asana--clear-users-cache ()
  "Clear the users cache."
  (setq org-asana--users-cache nil
        org-asana--users-cache-time nil))

;;; User Search Functions

(defun org-asana--find-user-by-email (email users)
  "Find user by EMAIL in USERS list."
  (seq-find (lambda (user)
              (equal (downcase (or (alist-get 'email user) ""))
                     (downcase email)))
            users))

(defun org-asana--find-users-by-name (name users)
  "Find users by NAME (partial match) in USERS list."
  (seq-filter (lambda (user)
                (string-match-p (regexp-quote name)
                               (or (alist-get 'name user) "")))
              users))

(defun org-asana--user-display-name (user)
  "Get display name for USER."
  (let ((name (alist-get 'name user))
        (email (alist-get 'email user)))
    (if email
        (format "%s <%s>" name email)
      name)))

;;; Completion Functions

(defun org-asana--user-completion-table (users)
  "Build completion table from USERS list."
  (mapcar (lambda (user)
            (cons (org-asana--user-display-name user)
                  (alist-get 'gid user)))
          users))

(defun org-asana--read-user (prompt &optional workspace-gid initial)
  "Read user with PROMPT, optionally from WORKSPACE-GID with INITIAL value."
  (let* ((workspace-gid (or workspace-gid
                           (cadr (org-asana--fetch-workspace-info))))
         (users (org-asana--get-cached-users workspace-gid))
         (completion-table (org-asana--user-completion-table users))
         (selection (completing-read prompt completion-table nil t initial)))
    (cdr (assoc selection completion-table))))

;;; Property Management

(defun org-asana--set-assignee-property (user)
  "Set assignee property from USER object."
  (if user
      (let ((name (alist-get 'name user))
            (gid (alist-get 'gid user)))
        (org-set-property "ASANA-ASSIGNEE" name)
        (org-set-property "ASANA-ASSIGNEE-GID" gid))
    (org-delete-property "ASANA-ASSIGNEE")
    (org-delete-property "ASANA-ASSIGNEE-GID")))

(defun org-asana--get-assignee-gid ()
  "Get assignee GID from current entry."
  (org-entry-get (point) "ASANA-ASSIGNEE-GID"))

;;; Interactive Functions

(defun org-asana-assign-task ()
  "Assign current task to a user."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((user-gid (org-asana--read-user "Assign to: ")))
      (when user-gid
        (org-asana--update-task-assignee task-gid user-gid)
        ;; Update local properties
        (let ((user (seq-find (lambda (u) (equal (alist-get 'gid u) user-gid))
                             org-asana--users-cache)))
          (org-asana--set-assignee-property user))
        (message "Task assigned to %s" (alist-get 'name user))))))

(defun org-asana-unassign-task ()
  "Remove assignee from current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (when (yes-or-no-p "Remove assignee from this task? ")
      (org-asana--remove-task-assignee task-gid)
      (org-asana--set-assignee-property nil)
      (message "Assignee removed"))))

(defun org-asana-assign-to-me ()
  "Assign current task to the authenticated user."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let* ((me (org-asana--fetch-current-user))
           (my-gid (alist-get 'gid me)))
      (org-asana--update-task-assignee task-gid my-gid)
      (org-asana--set-assignee-property me)
      (message "Task assigned to you"))))

(defun org-asana-show-user-info ()
  "Show information about task assignee."
  (interactive)
  (let ((assignee-gid (org-asana--get-assignee-gid))
        (assignee-name (org-entry-get (point) "ASANA-ASSIGNEE")))
    (if assignee-gid
        (message "Assigned to: %s (GID: %s)" assignee-name assignee-gid)
      (message "Task is unassigned"))))

(defun org-asana-list-workspace-users ()
  "List all users in current workspace."
  (interactive)
  (let* ((workspace-gid (cadr (org-asana--fetch-workspace-info)))
         (users (org-asana--get-cached-users workspace-gid))
         (buffer (get-buffer-create "*Asana Users*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Asana Workspace Users\n")
      (insert "=====================\n\n")
      (dolist (user users)
        (insert (format "%-30s %s\n"
                       (alist-get 'name user)
                       (or (alist-get 'email user) "(no email)"))))
      (goto-char (point-min))
      (view-mode))
    (display-buffer buffer)))

;;; Batch Operations

(defun org-asana-batch-assign ()
  "Assign multiple tasks to a user."
  (interactive)
  (let ((user-gid (org-asana--read-user "Assign all to: "))
        (count 0))
    (when user-gid
      (org-map-entries
       (lambda ()
         (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
           (when task-gid
             (org-asana--update-task-assignee task-gid user-gid)
             (setq count (1+ count)))))
       "ASANA-TASK-GID={.+}" 'region)
      (message "Assigned %d tasks" count))))

(defun org-asana-filter-by-assignee ()
  "Filter tasks by assignee."
  (interactive)
  (let ((assignee-name (completing-read "Filter by assignee: "
                                       (org-asana--get-all-assignees))))
    (org-match-sparse-tree nil (format "ASANA-ASSIGNEE=\"%s\"" assignee-name))))

(defun org-asana--get-all-assignees ()
  "Get list of all unique assignees in current buffer."
  (let ((assignees '()))
    (org-map-entries
     (lambda ()
       (let ((assignee (org-entry-get (point) "ASANA-ASSIGNEE")))
         (when (and assignee (not (member assignee assignees)))
           (push assignee assignees))))
     "ASANA-ASSIGNEE={.+}" 'file)
    (sort assignees #'string<)))

;;; Tag-based Assignment

(defcustom org-asana-assignee-tags nil
  "Alist mapping Org tags to Asana user GIDs.
Example: '((\"john\" . \"1234567890\") (\"jane\" . \"0987654321\"))"
  :type '(alist :key-type string :value-type string)
  :group 'org-asana)

(defun org-asana-assign-by-tag ()
  "Assign task based on Org tags."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (tags (org-get-tags)))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((assignee-gid nil))
      (dolist (tag tags)
        (when-let ((gid (cdr (assoc tag org-asana-assignee-tags))))
          (setq assignee-gid gid)))
      (if assignee-gid
          (progn
            (org-asana--update-task-assignee task-gid assignee-gid)
            (message "Task assigned based on tag"))
        (message "No assignee tag found")))))

;;; Integration

(defun org-asana--add-assignee-to-properties (task)
  "Add assignee properties from TASK."
  (let ((assignee (alist-get 'assignee task)))
    (when assignee
      (list (cons "ASANA-ASSIGNEE" (alist-get 'name assignee))
            (cons "ASANA-ASSIGNEE-GID" (alist-get 'gid assignee))))))

(provide 'org-asana-users)
;;; org-asana-users.el ends here