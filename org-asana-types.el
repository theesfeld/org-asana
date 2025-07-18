;;; org-asana-types.el --- Task type support for org-asana -*- lexical-binding: t; -*-

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

;; This module provides task type indicators for org-asana.
;; Handles milestones, approvals, and separators.

;;; Code:

(require 'org-asana)

;;; Custom Faces

(defface org-asana-milestone
  '((t :background "light blue" :weight bold))
  "Face for milestone tasks."
  :group 'org-asana)

(defface org-asana-approval
  '((t :background "light green" :weight bold))
  "Face for approval tasks."
  :group 'org-asana)

(defface org-asana-separator
  '((t :foreground "gray" :strike-through t))
  "Face for separator tasks."
  :group 'org-asana)

;;; Custom Variables

(defcustom org-asana-show-type-indicators t
  "Whether to show task type indicators."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-milestone-prefix "[MILESTONE] "
  "Prefix for milestone tasks."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-approval-prefix "[APPROVAL] "
  "Prefix for approval tasks."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-separator-prefix "────── "
  "Prefix for separator tasks."
  :type 'string
  :group 'org-asana)

;;; Type Detection

(defun org-asana--task-type (task)
  "Get type of TASK."
  (let ((subtype (alist-get 'resource_subtype task)))
    (cond
     ((equal subtype "milestone") 'milestone)
     ((equal subtype "approval") 'approval)
     ((equal subtype "section") 'separator)
     (t 'default))))

(defun org-asana--task-is-milestone-p (task)
  "Return t if TASK is a milestone."
  (equal (alist-get 'resource_subtype task) "milestone"))

(defun org-asana--task-is-approval-p (task)
  "Return t if TASK is an approval."
  (equal (alist-get 'resource_subtype task) "approval"))

(defun org-asana--task-is-separator-p (task)
  "Return t if TASK is a separator."
  (or (equal (alist-get 'resource_subtype task) "section")
      (alist-get 'is_rendered_as_separator task)))

;;; Title Formatting

(defun org-asana--format-task-title-with-type (task)
  "Format TASK title with type indicator."
  (let ((title (alist-get 'name task))
        (type (org-asana--task-type task)))
    (if org-asana-show-type-indicators
        (pcase type
          ('milestone (concat org-asana-milestone-prefix title))
          ('approval (concat org-asana-approval-prefix title))
          ('separator (concat org-asana-separator-prefix title " ──────"))
          (_ title))
      title)))

;;; Property Management

(defun org-asana--add-type-properties (task)
  "Add type-related properties from TASK."
  (let ((properties '())
        (subtype (alist-get 'resource_subtype task)))
    (when subtype
      (push (cons "ASANA-TYPE" subtype) properties))
    (when (org-asana--task-is-approval-p task)
      (let ((approval-status (alist-get 'approval_status task)))
        (when approval-status
          (push (cons "ASANA-APPROVAL-STATUS" approval-status) properties))))
    (when (alist-get 'is_rendered_as_separator task)
      (push (cons "ASANA-SEPARATOR" "t") properties))
    properties))

;;; Face Application

(defun org-asana--apply-type-face ()
  "Apply face based on task type at point."
  (let ((task-type (org-entry-get (point) "ASANA-TYPE")))
    (when task-type
      (let ((face (pcase task-type
                    ("milestone" 'org-asana-milestone)
                    ("approval" 'org-asana-approval)
                    ("section" 'org-asana-separator)
                    (_ nil))))
        (when face
          (org-asana--apply-face-to-heading face))))))

(defun org-asana--apply-type-faces-buffer ()
  "Apply type faces to all tasks in buffer."
  (save-excursion
    (org-map-entries #'org-asana--apply-type-face
                     "ASANA-TYPE={.+}" 'file)))

;;; Approval Management

(defun org-asana--approval-status-string (status)
  "Convert approval STATUS to readable string."
  (pcase status
    ("pending" "⏳ Pending")
    ("approved" "✅ Approved")
    ("rejected" "❌ Rejected")
    ("changes_requested" "🔄 Changes Requested")
    (_ status)))

(defun org-asana--update-approval-status (task-gid status)
  "Update TASK-GID approval STATUS."
  (org-asana--make-request
   "PUT"
   (format "/tasks/%s" task-gid)
   `((data . ((approval_status . ,status))))))

;;; Interactive Functions

(defun org-asana-approve-task ()
  "Approve the current approval task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (task-type (org-entry-get (point) "ASANA-TYPE")))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless (equal task-type "approval")
      (error "Not an approval task"))
    (org-asana--update-approval-status task-gid "approved")
    (org-set-property "ASANA-APPROVAL-STATUS" "approved")
    (message "Task approved")))

(defun org-asana-reject-task ()
  "Reject the current approval task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (task-type (org-entry-get (point) "ASANA-TYPE")))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless (equal task-type "approval")
      (error "Not an approval task"))
    (let ((reason (read-string "Rejection reason (optional): ")))
      (org-asana--update-approval-status task-gid "rejected")
      (org-set-property "ASANA-APPROVAL-STATUS" "rejected")
      (when (not (string-empty-p reason))
        (org-asana--create-comment task-gid
                                  (format "Rejected: %s" reason)))
      (message "Task rejected"))))

(defun org-asana-request-changes ()
  "Request changes for the current approval task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (task-type (org-entry-get (point) "ASANA-TYPE")))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless (equal task-type "approval")
      (error "Not an approval task"))
    (let ((changes (read-string "Changes needed: ")))
      (org-asana--update-approval-status task-gid "changes_requested")
      (org-set-property "ASANA-APPROVAL-STATUS" "changes_requested")
      (when (not (string-empty-p changes))
        (org-asana--create-comment task-gid
                                  (format "Changes requested: %s" changes)))
      (message "Changes requested"))))

(defun org-asana-show-approval-status ()
  "Show approval status of current task."
  (interactive)
  (let ((status (org-entry-get (point) "ASANA-APPROVAL-STATUS")))
    (if status
        (message "Approval status: %s"
                (org-asana--approval-status-string status))
      (message "Not an approval task or no status"))))

;;; Milestone Functions

(defun org-asana-convert-to-milestone ()
  "Convert current task to a milestone."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (org-asana--make-request
     "PUT"
     (format "/tasks/%s" task-gid)
     '((data . ((resource_subtype . "milestone")))))
    (org-set-property "ASANA-TYPE" "milestone")
    (message "Task converted to milestone")))

(defun org-asana-list-milestones ()
  "List all milestones in current buffer."
  (interactive)
  (occur "ASANA-TYPE:.*milestone"))

;;; Section/Separator Functions

(defun org-asana-create-separator (title)
  "Create a separator task with TITLE."
  (interactive "sSeparator title: ")
  ;; Create at current position
  (org-insert-heading-respect-content)
  (insert (org-asana--format-task-title-with-type
           `((name . ,title)
             (resource_subtype . "section"))))
  (org-set-property "ASANA-TASK-GID" "new")
  (org-set-property "ASANA-TYPE" "section")
  (org-set-property "ASANA-SEPARATOR" "t")
  (message "Separator created. Run sync to create in Asana."))

;;; Filtering Functions

(defun org-asana-filter-by-type (type)
  "Filter tasks by TYPE."
  (interactive
   (list (completing-read "Task type: "
                         '("default" "milestone" "approval" "section")
                         nil t)))
  (if (equal type "default")
      (org-match-sparse-tree nil "-ASANA-TYPE={.+}")
    (org-match-sparse-tree nil (format "ASANA-TYPE=\"%s\"" type))))

;;; Integration

(defun org-asana--enhance-task-with-type (task)
  "Enhance TASK with type information."
  (let ((type-props (org-asana--add-type-properties task)))
    ;; Add to existing properties
    (dolist (prop type-props)
      (push prop (alist-get 'properties task))))
  task)

(defun org-asana--add-type-to-opt-fields (opt-fields)
  "Add type fields to OPT-FIELDS."
  (concat opt-fields ",resource_subtype,approval_status,is_rendered_as_separator"))

;;; Hook Functions

(defun org-asana--setup-type-keymap ()
  "Setup keymap for task type operations."
  (define-key org-mode-map (kbd "C-c A a") #'org-asana-approve-task)
  (define-key org-mode-map (kbd "C-c A r") #'org-asana-reject-task)
  (define-key org-mode-map (kbd "C-c A c") #'org-asana-request-changes)
  (define-key org-mode-map (kbd "C-c A m") #'org-asana-convert-to-milestone)
  (define-key org-mode-map (kbd "C-c A s") #'org-asana-create-separator))

(add-hook 'org-mode-hook #'org-asana--setup-type-keymap)

(provide 'org-asana-types)
;;; org-asana-types.el ends here