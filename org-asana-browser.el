;;; org-asana-browser.el --- Interactive task browser for org-asana -*- lexical-binding: t; -*-

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

;; This module provides an interactive task browser for org-asana.
;; Browse and manage Asana tasks without full sync.

;;; Code:

(require 'org-asana)
(require 'tabulated-list)

;;; Custom Variables

(defcustom org-asana-browser-page-size 50
  "Number of tasks to show per page."
  :type 'integer
  :group 'org-asana)

(defcustom org-asana-browser-show-completed t
  "Whether to show completed tasks in browser."
  :type 'boolean
  :group 'org-asana)

;;; Variables

(defvar org-asana-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-asana-browser-open-task)
    (define-key map (kbd "o") #'org-asana-browser-open-in-asana)
    (define-key map (kbd "e") #'org-asana-browser-edit-task)
    (define-key map (kbd "c") #'org-asana-browser-toggle-complete)
    (define-key map (kbd "d") #'org-asana-browser-set-due-date)
    (define-key map (kbd "a") #'org-asana-browser-assign-task)
    (define-key map (kbd "p") #'org-asana-browser-set-project)
    (define-key map (kbd "t") #'org-asana-browser-add-tag)
    (define-key map (kbd "r") #'org-asana-browser-refresh)
    (define-key map (kbd "g") #'org-asana-browser-refresh)
    (define-key map (kbd "s") #'org-asana-browser-search)
    (define-key map (kbd "f") #'org-asana-browser-filter)
    (define-key map (kbd "n") #'org-asana-browser-next-page)
    (define-key map (kbd "p") #'org-asana-browser-prev-page)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for org-asana-browser-mode.")

(defvar org-asana-browser--current-project nil
  "Current project being browsed.")

(defvar org-asana-browser--current-filter nil
  "Current filter applied.")

(defvar org-asana-browser--current-page 0
  "Current page number.")

(defvar org-asana-browser--tasks nil
  "Current list of tasks.")

;;; Mode Definition

(define-derived-mode org-asana-browser-mode tabulated-list-mode "Asana Browser"
  "Major mode for browsing Asana tasks."
  (setq tabulated-list-format
        [("Done" 4 t)
         ("Task" 40 t)
         ("Project" 20 t)
         ("Assignee" 15 t)
         ("Due" 10 t)
         ("Tags" 20 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Due" nil))
  (add-hook 'tabulated-list-revert-hook #'org-asana-browser--refresh nil t)
  (tabulated-list-init-header))

;;; Data Fetching

(defun org-asana-browser--fetch-tasks (&optional project-gid filter)
  "Fetch tasks for PROJECT-GID with optional FILTER."
  (let* ((endpoint (if project-gid
                      (format "/projects/%s/tasks" project-gid)
                    "/tasks"))
         (params (org-asana-browser--build-params filter))
         (tasks (org-asana--fetch-all-with-pagination endpoint params)))
    (setq org-asana-browser--tasks tasks)))

(defun org-asana-browser--build-params (filter)
  "Build query params from FILTER."
  (let ((params '()))
    (when (not org-asana-browser-show-completed)
      (push '("completed_since" . "now") params))
    (when (plist-get filter :assignee)
      (push (cons "assignee" (plist-get filter :assignee)) params))
    (when (plist-get filter :tag)
      (push (cons "tag" (plist-get filter :tag)) params))
    (when (plist-get filter :section)
      (push (cons "section" (plist-get filter :section)) params))
    (push '("opt_fields" . "name,completed,assignee.name,due_on,tags.name,projects.name") params)
    params))

;;; Display Functions

(defun org-asana-browser--refresh ()
  "Refresh the task list."
  (org-asana-browser--fetch-tasks org-asana-browser--current-project
                                 org-asana-browser--current-filter)
  (org-asana-browser--populate-list))

(defun org-asana-browser--populate-list ()
  "Populate the tabulated list with tasks."
  (let* ((start (* org-asana-browser--current-page org-asana-browser-page-size))
         (end (min (+ start org-asana-browser-page-size)
                   (length org-asana-browser--tasks)))
         (page-tasks (seq-subseq org-asana-browser--tasks start end))
         (entries (mapcar #'org-asana-browser--task-to-entry page-tasks)))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

(defun org-asana-browser--task-to-entry (task)
  "Convert TASK to tabulated list entry."
  (let* ((gid (alist-get 'gid task))
         (name (alist-get 'name task))
         (completed (if (alist-get 'completed task) "✓" ""))
         (assignee (or (alist-get 'name (alist-get 'assignee task)) ""))
         (due (or (alist-get 'due_on task) ""))
         (projects (mapconcat (lambda (p) (alist-get 'name p))
                             (alist-get 'projects task) ", "))
         (tags (mapconcat (lambda (tag) (alist-get 'name tag))
                         (alist-get 'tags task) ", ")))
    (list gid (vector completed name projects assignee due tags))))

;;; Task Operations

(defun org-asana-browser-open-task ()
  "Open task at point in org file."
  (interactive)
  (let ((task-gid (tabulated-list-get-id)))
    (when task-gid
      (org-asana-browser--sync-single-task task-gid)
      (org-asana-browser--jump-to-task task-gid))))

(defun org-asana-browser--sync-single-task (task-gid)
  "Sync single TASK-GID to org file."
  (let ((task (org-asana--get-task task-gid)))
    (when task
      (with-current-buffer (find-file-noselect org-asana-org-file)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward (format ":ASANA-TASK-GID: %s" task-gid) nil t)
              (org-asana--update-task-in-org task)
            (org-asana--insert-new-task task)))))))

(defun org-asana-browser--jump-to-task (task-gid)
  "Jump to TASK-GID in org file."
  (find-file org-asana-org-file)
  (goto-char (point-min))
  (when (re-search-forward (format ":ASANA-TASK-GID: %s" task-gid) nil t)
    (org-back-to-heading t)
    (org-show-context)))

(defun org-asana-browser-open-in-asana ()
  "Open task at point in web browser."
  (interactive)
  (let ((task-gid (tabulated-list-get-id)))
    (when task-gid
      (browse-url (format "https://app.asana.com/0/0/%s/f" task-gid)))))

(defun org-asana-browser-edit-task ()
  "Edit task name at point."
  (interactive)
  (let ((task-gid (tabulated-list-get-id))
        (current-name (aref (tabulated-list-get-entry) 1)))
    (when task-gid
      (let ((new-name (read-string "Task name: " current-name)))
        (org-asana--update-task task-gid `((name . ,new-name)))
        (org-asana-browser-refresh)))))

(defun org-asana-browser-toggle-complete ()
  "Toggle completion status of task at point."
  (interactive)
  (let* ((task-gid (tabulated-list-get-id))
         (completed (string= (aref (tabulated-list-get-entry) 0) "✓")))
    (when task-gid
      (org-asana--update-task task-gid `((completed . ,(not completed))))
      (org-asana-browser-refresh))))

(defun org-asana-browser-set-due-date ()
  "Set due date for task at point."
  (interactive)
  (let ((task-gid (tabulated-list-get-id)))
    (when task-gid
      (let ((date (org-read-date nil nil nil "Due date: ")))
        (org-asana--update-task task-gid `((due_on . ,date)))
        (org-asana-browser-refresh)))))

(defun org-asana-browser-assign-task ()
  "Assign task at point to a user."
  (interactive)
  (let ((task-gid (tabulated-list-get-id)))
    (when task-gid
      (let ((user-gid (org-asana--read-user "Assign to: ")))
        (when user-gid
          (org-asana--update-task task-gid `((assignee . ,user-gid)))
          (org-asana-browser-refresh))))))

;;; Navigation

(defun org-asana-browser-next-page ()
  "Go to next page."
  (interactive)
  (let ((max-page (/ (length org-asana-browser--tasks) 
                    org-asana-browser-page-size)))
    (when (< org-asana-browser--current-page max-page)
      (cl-incf org-asana-browser--current-page)
      (org-asana-browser--populate-list))))

(defun org-asana-browser-prev-page ()
  "Go to previous page."
  (interactive)
  (when (> org-asana-browser--current-page 0)
    (cl-decf org-asana-browser--current-page)
    (org-asana-browser--populate-list)))

;;; Filtering

(defun org-asana-browser-filter ()
  "Apply filter to task list."
  (interactive)
  (let ((filter-type (completing-read "Filter by: " 
                                     '("assignee" "tag" "section" "clear"))))
    (pcase filter-type
      ("assignee" (org-asana-browser--filter-by-assignee))
      ("tag" (org-asana-browser--filter-by-tag))
      ("section" (org-asana-browser--filter-by-section))
      ("clear" (setq org-asana-browser--current-filter nil)))
    (org-asana-browser-refresh)))

(defun org-asana-browser--filter-by-assignee ()
  "Filter by assignee."
  (let ((user-gid (org-asana--read-user "Filter by assignee: ")))
    (when user-gid
      (setq org-asana-browser--current-filter
            (plist-put org-asana-browser--current-filter :assignee user-gid)))))

(defun org-asana-browser--filter-by-tag ()
  "Filter by tag."
  (let* ((tags (org-asana--fetch-workspace-tags))
         (tag-names (mapcar (lambda (tag) (alist-get 'name tag)) tags))
         (selected (completing-read "Filter by tag: " tag-names))
         (tag (seq-find (lambda (t) (equal (alist-get 'name t) selected)) tags)))
    (when tag
      (setq org-asana-browser--current-filter
            (plist-put org-asana-browser--current-filter :tag (alist-get 'gid tag))))))

(defun org-asana-browser--filter-by-section ()
  "Filter by section."
  (unless org-asana-browser--current-project
    (error "Select a project first"))
  (let* ((sections (org-asana--fetch-project-sections org-asana-browser--current-project))
         (section-names (mapcar (lambda (s) (alist-get 'name s)) sections))
         (selected (completing-read "Filter by section: " section-names))
         (section (seq-find (lambda (s) (equal (alist-get 'name s) selected)) sections)))
    (when section
      (setq org-asana-browser--current-filter
            (plist-put org-asana-browser--current-filter :section (alist-get 'gid section))))))

;;; Search

(defun org-asana-browser-search ()
  "Search for tasks."
  (interactive)
  (let ((query (read-string "Search tasks: ")))
    (when (not (string-empty-p query))
      (org-asana-browser--search-tasks query))))

(defun org-asana-browser--search-tasks (query)
  "Search for tasks matching QUERY."
  (let* ((workspace-gid (cadr (org-asana--fetch-workspace-info)))
         (results (org-asana--search-tasks-in-workspace workspace-gid query)))
    (setq org-asana-browser--tasks results)
    (setq org-asana-browser--current-page 0)
    (org-asana-browser--populate-list)))

(defun org-asana--search-tasks-in-workspace (workspace-gid query)
  "Search for tasks in WORKSPACE-GID matching QUERY."
  (let ((params `(("text" . ,query)
                 ("resource_subtype" . "task")
                 ("opt_fields" . "name,completed,assignee.name,due_on,tags.name,projects.name"))))
    (alist-get 'data
               (org-asana--make-request "GET"
                                       (format "/workspaces/%s/tasks/search" workspace-gid)
                                       params))))

;;; Entry Points

(defun org-asana-browser-refresh ()
  "Refresh current view."
  (interactive)
  (org-asana-browser--refresh))

(defun org-asana-browse-project (project-gid)
  "Browse tasks in PROJECT-GID."
  (interactive
   (list (org-asana--read-project "Browse project: ")))
  (setq org-asana-browser--current-project project-gid)
  (setq org-asana-browser--current-filter nil)
  (setq org-asana-browser--current-page 0)
  (let ((buffer (get-buffer-create "*Asana Browser*")))
    (with-current-buffer buffer
      (org-asana-browser-mode)
      (org-asana-browser--refresh))
    (switch-to-buffer buffer)))

(defun org-asana-browse-my-tasks ()
  "Browse my tasks."
  (interactive)
  (let* ((workspace-gid (cadr (org-asana--fetch-workspace-info)))
         (me (org-asana--fetch-current-user))
         (my-gid (alist-get 'gid me)))
    (setq org-asana-browser--current-project nil)
    (setq org-asana-browser--current-filter `(:assignee ,my-gid))
    (setq org-asana-browser--current-page 0)
    (let ((buffer (get-buffer-create "*Asana Browser*")))
      (with-current-buffer buffer
        (org-asana-browser-mode)
        (org-asana-browser--refresh))
      (switch-to-buffer buffer))))

(defun org-asana-browse-workspace ()
  "Browse all tasks in workspace."
  (interactive)
  (setq org-asana-browser--current-project nil)
  (setq org-asana-browser--current-filter nil)
  (setq org-asana-browser--current-page 0)
  (let ((buffer (get-buffer-create "*Asana Browser*")))
    (with-current-buffer buffer
      (org-asana-browser-mode)
      (org-asana-browser--refresh))
    (switch-to-buffer buffer)))

;;; Utilities

(defun org-asana--read-project (prompt)
  "Read project with PROMPT."
  (let* ((projects (org-asana--fetch-workspace-projects))
         (completion-table (mapcar (lambda (p)
                                    (cons (alist-get 'name p)
                                          (alist-get 'gid p)))
                                  projects))
         (selection (completing-read prompt completion-table nil t)))
    (cdr (assoc selection completion-table))))

(defun org-asana--fetch-workspace-projects ()
  "Fetch all projects in workspace."
  (let ((workspace-gid (cadr (org-asana--fetch-workspace-info))))
    (org-asana--fetch-all-with-pagination
     (format "/workspaces/%s/projects" workspace-gid)
     '(("opt_fields" . "name,archived")
       ("archived" . "false")))))

(defun org-asana--fetch-project-sections (project-gid)
  "Fetch sections for PROJECT-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                     (format "/projects/%s/sections" project-gid))))

(defun org-asana--fetch-workspace-tags ()
  "Fetch all tags in workspace."
  (let ((workspace-gid (cadr (org-asana--fetch-workspace-info))))
    (alist-get 'data
               (org-asana--make-request "GET"
                                       (format "/workspaces/%s/tags" workspace-gid)))))

(provide 'org-asana-browser)
;;; org-asana-browser.el ends here