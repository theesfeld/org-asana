;;; org-asana-subtasks.el --- Subtasks support for org-asana -*- lexical-binding: t; -*-

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

;; This module provides subtasks synchronization for org-asana.
;; Maps Asana subtasks to Org-mode heading hierarchy.

;;; Code:

(require 'org-asana)

;;; Constants

(defconst org-asana--max-subtask-depth 5
  "Maximum nesting depth for subtasks.")

;;; API Functions

(defun org-asana--fetch-task-subtasks (task-gid)
  "Fetch subtasks for TASK-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/tasks/%s/subtasks" task-gid))))

(defun org-asana--create-subtask (parent-gid subtask-data)
  "Create a subtask under PARENT-GID with SUBTASK-DATA."
  (org-asana--make-request
   "POST"
   (format "/tasks/%s/subtasks" parent-gid)
   `((data . ,subtask-data))))

(defun org-asana--set-task-parent (task-gid parent-gid)
  "Set PARENT-GID as parent of TASK-GID."
  (org-asana--make-request
   "POST"
   (format "/tasks/%s/setParent" task-gid)
   `((data . ((parent . ,parent-gid))))))

(defun org-asana--remove-task-parent (task-gid)
  "Remove parent from TASK-GID, making it a top-level task."
  (org-asana--make-request
   "POST"
   (format "/tasks/%s/setParent" task-gid)
   '((data . ((parent . nil))))))

;;; Depth Tracking

(defun org-asana--calculate-task-depth (task)
  "Calculate nesting depth of TASK."
  (let ((depth 0)
        (current task))
    (while (and current (alist-get 'parent current))
      (setq depth (1+ depth))
      (setq current (alist-get 'parent current)))
    depth))

(defun org-asana--get-heading-level ()
  "Get Org heading level at point."
  (save-excursion
    (org-back-to-heading t)
    (org-current-level)))

(defun org-asana--validate-subtask-depth (parent-level)
  "Validate that creating subtask at PARENT-LEVEL won't exceed limit."
  (when (>= parent-level (+ 3 org-asana--max-subtask-depth))
    (error "Cannot create subtask: would exceed maximum depth of %d"
           org-asana--max-subtask-depth)))

;;; Recursive Fetching

(defun org-asana--fetch-subtasks-recursively (task-gid current-depth)
  "Recursively fetch all subtasks for TASK-GID at CURRENT-DEPTH."
  (when (< current-depth org-asana--max-subtask-depth)
    (let ((subtasks (org-asana--fetch-task-subtasks task-gid))
          (all-subtasks '()))
      (dolist (subtask subtasks)
        (let* ((subtask-gid (alist-get 'gid subtask))
               (full-subtask (org-asana--get-task subtask-gid)))
          (when full-subtask
            (push full-subtask all-subtasks)
            ;; Recursively fetch sub-subtasks
            (when (> (alist-get 'num_subtasks full-subtask 0) 0)
              (let ((sub-subtasks (org-asana--fetch-subtasks-recursively
                                  subtask-gid (1+ current-depth))))
                (setq all-subtasks (append all-subtasks sub-subtasks)))))))
      (nreverse all-subtasks))))

;;; Tree Building

(defun org-asana--create-subtask-node (subtask level metadata-map)
  "Create subtask node from SUBTASK at LEVEL with METADATA-MAP."
  (let* ((subtask-gid (alist-get 'gid subtask))
         (metadata (gethash subtask-gid metadata-map))
         (properties (org-asana--task-to-properties subtask metadata))
         (body-text (org-asana--format-task-body properties metadata))
         (task-title (alist-get 'name subtask))
         (completed (alist-get 'completed subtask))
         (todo-keyword (if completed "DONE" "TODO"))
         (subtasks (when (> (alist-get 'num_subtasks subtask 0) 0)
                    (org-asana--fetch-subtasks-recursively subtask-gid 0))))
    (list :level level
          :title task-title
          :todo-keyword todo-keyword
          :properties (org-asana--plist-to-alist properties)
          :body body-text
          :children (mapcar (lambda (st)
                             (org-asana--create-subtask-node
                              st (1+ level) metadata-map))
                           subtasks))))

(defun org-asana--add-subtasks-to-task-node (task-node task metadata-map)
  "Add subtasks to TASK-NODE from TASK with METADATA-MAP."
  (let ((subtasks (when (> (alist-get 'num_subtasks task 0) 0)
                   (org-asana--fetch-subtasks-recursively
                    (alist-get 'gid task) 0))))
    (when subtasks
      (let ((subtask-nodes (mapcar (lambda (st)
                                    (org-asana--create-subtask-node
                                     st (1+ (plist-get task-node :level))
                                     metadata-map))
                                  subtasks)))
        (plist-put task-node :children
                   (append (plist-get task-node :children) subtask-nodes))))
    task-node))

;;; Org Integration

(defun org-asana--find-parent-task-gid ()
  "Find parent task GID by looking at parent heading."
  (save-excursion
    (when (org-up-heading-safe)
      (org-entry-get (point) "ASANA-TASK-GID"))))

(defun org-asana--get-subtask-gids ()
  "Get GIDs of all direct subtasks of current heading."
  (let ((subtask-gids '()))
    (save-excursion
      (when (org-goto-first-child)
        (setq subtask-gids (cons (org-entry-get (point) "ASANA-TASK-GID")
                                subtask-gids))
        (while (org-get-next-sibling)
          (let ((gid (org-entry-get (point) "ASANA-TASK-GID")))
            (when gid
              (setq subtask-gids (cons gid subtask-gids)))))))
    (nreverse subtask-gids)))

;;; Sync Functions

(defun org-asana--sync-subtasks-structure ()
  "Sync subtask structure from Org to Asana."
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (parent-gid (org-asana--find-parent-task-gid)))
    (when task-gid
      ;; Update parent relationship
      (let ((current-parent (alist-get 'gid
                                     (alist-get 'parent
                                               (org-asana--get-task task-gid)))))
        (unless (equal current-parent parent-gid)
          (if parent-gid
              (org-asana--set-task-parent task-gid parent-gid)
            (org-asana--remove-task-parent task-gid)))))))

(defun org-asana--create-new-subtask ()
  "Create a new subtask under current task."
  (let ((parent-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (parent-level (org-asana--get-heading-level)))
    (unless parent-gid
      (error "Parent task must be synced with Asana first"))
    (org-asana--validate-subtask-depth parent-level)
    ;; Create Org heading
    (org-insert-heading-respect-content)
    (org-demote)
    (insert "TODO New Subtask")
    ;; Mark for creation
    (org-set-property "ASANA-TASK-GID" "new")
    (org-set-property "ASANA-PARENT-GID" parent-gid)))

;;; Interactive Functions

(defun org-asana-create-subtask ()
  "Create a new subtask under the current task."
  (interactive)
  (unless (org-entry-get (point) "ASANA-TASK-GID")
    (error "Not on an Asana task"))
  (org-asana--create-new-subtask)
  (message "Subtask created. Edit title and run sync to create in Asana."))

(defun org-asana-make-subtask ()
  "Make current task a subtask of another task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((parent-gid (read-string "Parent task GID: ")))
      (when (and parent-gid (not (string-empty-p parent-gid)))
        (org-asana--set-task-parent task-gid parent-gid)
        (message "Task moved under parent. Re-sync to update Org structure.")))))

(defun org-asana-promote-subtask ()
  "Promote subtask to top-level task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (parent-gid (org-asana--find-parent-task-gid)))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless parent-gid
      (error "Task is already top-level"))
    (org-asana--remove-task-parent task-gid)
    (message "Task promoted to top-level. Re-sync to update Org structure.")))

;;; Creation Handling

(defun org-asana--handle-new-subtasks ()
  "Handle creation of new subtasks marked with 'new' GID."
  (org-map-entries
   (lambda ()
     (let ((gid (org-entry-get (point) "ASANA-TASK-GID"))
           (parent-gid (org-entry-get (point) "ASANA-PARENT-GID")))
       (when (equal gid "new")
         (let* ((title (org-get-heading t t t t))
                (notes (org-asana--extract-task-notes))
                (workspace-gid (cadr (org-asana--fetch-workspace-info)))
                (subtask-data `((name . ,title)
                              (notes . ,notes)
                              (workspace . ,workspace-gid)))
                (response (org-asana--create-subtask parent-gid subtask-data))
                (new-task (alist-get 'data response)))
           (when new-task
             (org-set-property "ASANA-TASK-GID" (alist-get 'gid new-task))
             (org-delete-property "ASANA-PARENT-GID"))))))
   "ASANA-TASK-GID=\"new\"" 'file))

(defun org-asana--extract-task-notes ()
  "Extract task notes from current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (buffer-substring-no-properties
       (save-excursion
         (org-end-of-meta-data t)
         (point))
       (save-excursion
         (goto-char end)
         (skip-chars-backward " \t\n")
         (point))))))

;;; Integration

(defun org-asana--enhance-task-node-with-subtasks (task-node task metadata-map)
  "Enhance TASK-NODE with subtasks from TASK using METADATA-MAP."
  (if (> (alist-get 'num_subtasks task 0) 0)
      (org-asana--add-subtasks-to-task-node task-node task metadata-map)
    task-node))

(provide 'org-asana-subtasks)
;;; org-asana-subtasks.el ends here