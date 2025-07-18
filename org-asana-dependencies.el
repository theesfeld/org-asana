;;; org-asana-dependencies.el --- Task dependencies for org-asana -*- lexical-binding: t; -*-

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

;; This module provides task dependencies support for org-asana.
;; Maps Asana dependencies to Org-mode's BLOCKER property.

;;; Code:

(require 'org-asana)

;; Try to load org-depend if available
(require 'org-depend nil t)  ; For BLOCKER property support

;;; Constants

(defconst org-asana--max-dependencies 30
  "Maximum combined dependencies and dependents per task.")

;;; API Functions

(defun org-asana--fetch-task-dependencies (task-gid)
  "Fetch dependencies for TASK-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/tasks/%s/dependencies" task-gid))))

(defun org-asana--fetch-task-dependents (task-gid)
  "Fetch dependents for TASK-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/tasks/%s/dependents" task-gid))))

(defun org-asana--add-task-dependencies (task-gid dependency-gids)
  "Add DEPENDENCY-GIDS to TASK-GID."
  (when dependency-gids
    (org-asana--make-request
     "POST"
     (format "/tasks/%s/addDependencies" task-gid)
     `((data . ((dependencies . ,dependency-gids)))))))

(defun org-asana--add-task-dependents (task-gid dependent-gids)
  "Add DEPENDENT-GIDS to TASK-GID."
  (when dependent-gids
    (org-asana--make-request
     "POST"
     (format "/tasks/%s/addDependents" task-gid)
     `((data . ((dependents . ,dependent-gids)))))))

(defun org-asana--remove-task-dependencies (task-gid dependency-gids)
  "Remove DEPENDENCY-GIDS from TASK-GID."
  (when dependency-gids
    (org-asana--make-request
     "POST"
     (format "/tasks/%s/removeDependencies" task-gid)
     `((data . ((dependencies . ,dependency-gids)))))))

(defun org-asana--remove-task-dependents (task-gid dependent-gids)
  "Remove DEPENDENT-GIDS from TASK-GID."
  (when dependent-gids
    (org-asana--make-request
     "POST"
     (format "/tasks/%s/removeDependents" task-gid)
     `((data . ((dependents . ,dependent-gids)))))))

;;; Conversion Functions

(defun org-asana--dependencies-to-blocker-string (dependencies)
  "Convert DEPENDENCIES list to BLOCKER property string."
  (when dependencies
    (mapconcat (lambda (dep) (alist-get 'gid dep))
               dependencies " ")))

(defun org-asana--blocker-string-to-gids (blocker-string)
  "Convert BLOCKER-STRING to list of GIDs."
  (when (and blocker-string (not (string-empty-p blocker-string)))
    (split-string blocker-string " " t)))

(defun org-asana--extract-blocker-property ()
  "Extract BLOCKER property from current entry."
  (org-entry-get (point) "BLOCKER"))

;;; Validation Functions

(defun org-asana--validate-dependency-count (task-gid new-deps new-dependents)
  "Validate total dependencies for TASK-GID with NEW-DEPS and NEW-DEPENDENTS."
  (let* ((current-deps (length (org-asana--fetch-task-dependencies task-gid)))
         (current-dependents (length (org-asana--fetch-task-dependents task-gid)))
         (total (+ current-deps current-dependents
                   (length new-deps) (length new-dependents))))
    (when (> total org-asana--max-dependencies)
      (error "Cannot add dependencies: would exceed limit of %d combined dependencies/dependents"
             org-asana--max-dependencies))))

(defun org-asana--validate-circular-dependency (task-gid dep-gid)
  "Check for circular dependency between TASK-GID and DEP-GID."
  ;; Simple check: ensure dep-gid doesn't depend on task-gid
  (let ((dep-dependencies (org-asana--fetch-task-dependencies dep-gid)))
    (when (seq-find (lambda (d) (equal (alist-get 'gid d) task-gid))
                    dep-dependencies)
      (error "Circular dependency detected between %s and %s" task-gid dep-gid))))

;;; Property Management

(defun org-asana--set-blocker-property (dependencies)
  "Set BLOCKER property based on DEPENDENCIES."
  (let ((blocker-string (org-asana--dependencies-to-blocker-string dependencies)))
    (if blocker-string
        (org-set-property "BLOCKER" blocker-string)
      (org-delete-property "BLOCKER"))))

(defun org-asana--set-blocks-property (dependents)
  "Set BLOCKS property based on DEPENDENTS."
  (let ((blocks-string (org-asana--dependencies-to-blocker-string dependents)))
    (if blocks-string
        (org-set-property "BLOCKS" blocks-string)
      (org-delete-property "BLOCKS"))))

;;; Sync Functions

(defun org-asana--sync-dependencies-from-asana (task-gid)
  "Sync dependencies from Asana for TASK-GID to Org."
  (let ((dependencies (org-asana--fetch-task-dependencies task-gid))
        (dependents (org-asana--fetch-task-dependents task-gid)))
    (org-asana--set-blocker-property dependencies)
    (org-asana--set-blocks-property dependents)))

(defun org-asana--sync-dependencies-to-asana (task-gid)
  "Sync dependencies from Org to Asana for TASK-GID."
  (let* ((blocker-string (org-asana--extract-blocker-property))
         (new-dep-gids (org-asana--blocker-string-to-gids blocker-string))
         (current-deps (org-asana--fetch-task-dependencies task-gid))
         (current-dep-gids (mapcar (lambda (d) (alist-get 'gid d)) current-deps)))
    ;; Find additions and removals
    (let ((to-add (seq-difference new-dep-gids current-dep-gids))
          (to-remove (seq-difference current-dep-gids new-dep-gids)))
      ;; Validate before making changes
      (when to-add
        (org-asana--validate-dependency-count task-gid to-add nil)
        (dolist (dep-gid to-add)
          (org-asana--validate-circular-dependency task-gid dep-gid)))
      ;; Apply changes
      (when to-remove
        (org-asana--remove-task-dependencies task-gid to-remove))
      (when to-add
        (org-asana--add-task-dependencies task-gid to-add)))))

;;; Interactive Functions

(defun org-asana-add-dependency ()
  "Add a dependency to the current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((dep-gid (read-string "Dependency task GID: ")))
      (when (and dep-gid (not (string-empty-p dep-gid)))
        ;; Validate
        (org-asana--validate-dependency-count task-gid (list dep-gid) nil)
        (org-asana--validate-circular-dependency task-gid dep-gid)
        ;; Add to BLOCKER property
        (let ((current-blocker (org-asana--extract-blocker-property)))
          (org-set-property "BLOCKER"
                           (if current-blocker
                               (concat current-blocker " " dep-gid)
                             dep-gid)))
        (message "Dependency added. Run sync to update Asana.")))))

(defun org-asana-remove-dependency ()
  "Remove a dependency from the current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID"))
        (blocker-string (org-asana--extract-blocker-property)))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless blocker-string
      (error "No dependencies to remove"))
    (let* ((dep-gids (org-asana--blocker-string-to-gids blocker-string))
           (dep-to-remove (completing-read "Remove dependency: " dep-gids nil t)))
      (when dep-to-remove
        (let ((new-deps (delete dep-to-remove dep-gids)))
          (if new-deps
              (org-set-property "BLOCKER" (string-join new-deps " "))
            (org-delete-property "BLOCKER")))
        (message "Dependency removed. Run sync to update Asana.")))))

(defun org-asana-show-dependencies ()
  "Show dependencies for the current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((dependencies (org-asana--fetch-task-dependencies task-gid))
          (dependents (org-asana--fetch-task-dependents task-gid)))
      (with-current-buffer (get-buffer-create "*Asana Dependencies*")
        (erase-buffer)
        (insert "Task Dependencies\n")
        (insert "=================\n\n")
        (insert "Blocked by (dependencies):\n")
        (if dependencies
            (dolist (dep dependencies)
              (insert (format "  - %s (GID: %s)\n"
                             (or (alist-get 'name dep) "Unnamed")
                             (alist-get 'gid dep))))
          (insert "  (none)\n"))
        (insert "\nBlocking (dependents):\n")
        (if dependents
            (dolist (dep dependents)
              (insert (format "  - %s (GID: %s)\n"
                             (or (alist-get 'name dep) "Unnamed")
                             (alist-get 'gid dep))))
          (insert "  (none)\n"))
        (goto-char (point-min))
        (view-mode))
      (display-buffer "*Asana Dependencies*"))))

;;; Integration

(defun org-asana--add-dependencies-to-opt-fields (opt-fields)
  "Add dependencies fields to OPT-FIELDS."
  (concat opt-fields ",dependencies.gid,dependencies.name,"
          "dependents.gid,dependents.name"))

(provide 'org-asana-dependencies)
;;; org-asana-dependencies.el ends here