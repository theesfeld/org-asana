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

;;; Constants

(defconst org-asana-api-base-url "https://app.asana.com/api/1.0"
  "Base URL for Asana API endpoints.")

;;; HTTP/API Functions

(defun org-asana--http-headers ()
  "Return HTTP headers for Asana API requests."
  `(("Authorization" . ,(format "Bearer %s" org-asana-token))
    ("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defun org-asana--api-url (endpoint)
  "Construct full API URL for ENDPOINT."
  (concat org-asana-api-base-url endpoint))

(defun org-asana--parse-json-response (buffer)
  "Parse JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun org-asana--make-request (method endpoint &optional data)
  "Make HTTP request to Asana API."
  (let* ((url (org-asana--api-url endpoint))
         (url-request-method method)
         (url-request-extra-headers (org-asana--http-headers))
         (url-request-data (when data (json-encode data)))
         (buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (let ((status (string-to-number (match-string 1))))
                (if (and (>= status 200) (< status 300))
                    (org-asana--parse-json-response buffer)
                  (error "Asana API error: HTTP %d" status)))
            (error "Invalid HTTP response from Asana")))
      (kill-buffer buffer))))

;;; Public Commands

;;;###autoload
(defun org-asana-test-connection ()
  "Test connection to Asana API."
  (interactive)
  (unless org-asana-token
    (error "No Asana token configured. Set `org-asana-token'"))
  (condition-case err
      (let* ((response (org-asana--make-request "GET" "/users/me"))
             (user (alist-get 'data response)))
        (message "Connected to Asana as: %s" (alist-get 'name user))
        t)
    (error
     (message "Connection failed: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun org-asana-sync ()
  "Sync tasks between Org and Asana."
  (interactive)
  (unless org-asana-token
    (error "No Asana token configured. Set `org-asana-token'"))
  (unless org-asana-org-file
    (error "No org file configured. Set `org-asana-org-file'"))
  
  ;; Open or create the target file
  (let ((buffer (find-file-noselect org-asana-org-file)))
    (with-current-buffer buffer
      ;; Ensure we're in org-mode
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      
      ;; Initialize file structure if empty
      (when (= (buffer-size) 0)
        (insert "* Active Projects\n\n* COMPLETED\n")
        (goto-char (point-min)))
      
      ;; Sync local DONE tasks to Asana and move to COMPLETED
      (org-asana--sync-done-tasks)
      
      ;; Sync from Asana
      (org-asana--sync-from-asana)
      
      ;; Sync pending changes to Asana
      (org-asana--sync-to-asana)
      
      (save-buffer)
      (message "Sync complete"))))

;;; Internal Sync Functions

(defun org-asana--sync-done-tasks ()
  "Find DONE tasks, sync to Asana, and move to COMPLETED section."
  (save-excursion
    (goto-char (point-min))
    ;; Find Active Projects section
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (let ((active-end (save-excursion
                         (if (re-search-forward "^\\* COMPLETED$" nil t)
                             (match-beginning 0)
                           (point-max))))
            (tasks-to-move '()))
        
        ;; Find all DONE tasks in active section
        (while (re-search-forward "^\\*\\{4\\} DONE " active-end t)
          (let* ((task-start (line-beginning-position))
                 (task-id (org-entry-get nil "ASANA_TASK_ID"))
                 (task-end (save-excursion
                            (org-end-of-subtree t)
                            (point))))
            (when task-id
              ;; Update Asana
              (condition-case nil
                  (org-asana--make-request 
                   "PUT" 
                   (format "/tasks/%s" task-id)
                   `((data . ((completed . t)))))
                (error nil))
              
              ;; Collect task info for moving
              (push (list task-start task-end
                         (buffer-substring task-start task-end)
                         (save-excursion
                           (org-back-to-heading t)
                           (let ((project "")
                                 (section ""))
                             (when (re-search-backward "^\\*\\{3\\} " nil t)
                               (setq section (org-get-heading t t t t))
                               (when (re-search-backward "^\\*\\{2\\} " nil t)
                                 (setq project (org-get-heading t t t t))))
                             (cons project section))))
                    tasks-to-move))))
        
        ;; Move tasks to COMPLETED section
        (dolist (task-info (nreverse tasks-to-move))
          (let ((start (nth 0 task-info))
                (end (nth 1 task-info))
                (content (nth 2 task-info))
                (project (car (nth 3 task-info)))
                (section (cdr (nth 3 task-info))))
            
            ;; Delete from active section
            (delete-region start end)
            
            ;; Add to completed section
            (save-excursion
              (goto-char (point-max))
              (re-search-backward "^\\* COMPLETED$" nil t)
              (org-end-of-subtree t)
              
              ;; Find or create project
              (let ((project-pos (save-excursion
                                  (when (re-search-forward 
                                         (format "^\\*\\* %s$" (regexp-quote project))
                                         nil t)
                                    (point)))))
                (unless project-pos
                  (insert (format "\n** %s\n" project))
                  (setq project-pos (point)))
                
                (goto-char project-pos)
                
                ;; Find or create section
                (let ((section-pos (save-excursion
                                    (when (re-search-forward
                                           (format "^\\*\\*\\* %s$" (regexp-quote section))
                                           nil t)
                                      (point)))))
                  (unless section-pos
                    (org-end-of-subtree t)
                    (insert (format "*** %s\n" section))
                    (setq section-pos (point)))
                  
                  (goto-char section-pos)
                  (org-end-of-subtree t)
                  (insert content)
                  
                  ;; Add completion timestamp
                  (save-excursion
                    (re-search-backward "^\\*\\{4\\} DONE " nil t)
                    (org-set-property "ASANA_COMPLETED_AT" 
                                     (format-time-string "%Y-%m-%dT%H:%M:%S.000Z")))))))))))

(defun org-asana--sync-from-asana ()
  "Sync tasks from Asana to org."
  (let* ((user-info (alist-get 'data (org-asana--make-request "GET" "/users/me")))
         (user-gid (alist-get 'gid user-info))
         (workspaces (alist-get 'data (org-asana--make-request "GET" "/workspaces")))
         (workspace-gid (alist-get 'gid (car workspaces)))
         (opt-fields "gid,name,notes,completed,due_on,modified_at,memberships.project.name,memberships.section.name")
         (tasks (alist-get 'data 
                          (org-asana--make-request 
                           "GET" 
                           (format "/workspaces/%s/tasks/search?assignee.any=%s&completed=false&limit=100&opt_fields=%s"
                                   workspace-gid user-gid opt-fields)))))
    
    ;; Group tasks by project and section
    (let ((task-tree (org-asana--build-task-tree tasks)))
      
      ;; Update Active Projects section
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* Active Projects$" nil t)
        (let ((section-start (point)))
          
          ;; Process each project
          (dolist (project-entry task-tree)
            (let* ((project-name (car project-entry))
                   (sections (cdr project-entry))
                   (project-pos (org-asana--find-or-create-heading 
                                2 project-name section-start)))
              
              (goto-char project-pos)
              
              ;; Process each section
              (dolist (section-entry sections)
                (let* ((section-name (car section-entry))
                       (tasks (cdr section-entry))
                       (section-pos (org-asana--find-or-create-heading 
                                    3 section-name project-pos)))
                  
                  (goto-char section-pos)
                  
                  ;; Process tasks
                  (dolist (task tasks)
                    (org-asana--update-or-create-task task section-pos))))))
          
          ;; Update statistics
          (org-asana--update-statistics))))))

(defun org-asana--sync-to-asana ()
  "Sync local changes to Asana."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (let ((active-end (save-excursion
                         (if (re-search-forward "^\\* COMPLETED$" nil t)
                             (match-beginning 0)
                           (point-max)))))
        
        ;; Find all tasks with IDs
        (while (re-search-forward "^\\*\\{4\\} \\(TODO\\|DONE\\) " active-end t)
          (let ((task-id (org-entry-get nil "ASANA_TASK_ID")))
            (when task-id
              (let* ((heading (org-get-heading t t t t))
                     (todo-state (org-get-todo-state))
                     (completed (string= todo-state "DONE"))
                     (deadline (org-entry-get nil "DEADLINE"))
                     (body (org-asana--get-task-body))
                     (due-date (when deadline
                                (format-time-string 
                                 "%Y-%m-%d"
                                 (org-time-string-to-time deadline)))))
                
                (condition-case nil
                    (org-asana--make-request
                     "PUT"
                     (format "/tasks/%s" task-id)
                     `((data . ((name . ,heading)
                               (notes . ,body)
                               (completed . ,completed)
                               ,@(when due-date `((due_on . ,due-date)))))))
                  (error nil))))))))))

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
           (regexp (format "^%s %s$" stars (regexp-quote heading)))
           (end-pos (save-excursion
                     (org-end-of-subtree t)
                     (point))))
      
      (if (re-search-forward regexp end-pos t)
          (point)
        (goto-char end-pos)
        (unless (bolp) (insert "\n"))
        (insert (format "%s %s\n" stars heading))
        (point)))))

(defun org-asana--update-or-create-task (task parent-pos)
  "Update or create TASK under PARENT-POS."
  (let* ((task-id (alist-get 'gid task))
         (task-name (alist-get 'name task))
         (completed (alist-get 'completed task))
         (notes (alist-get 'notes task))
         (due-on (alist-get 'due_on task))
         (modified-at (alist-get 'modified_at task))
         (existing-pos (org-asana--find-task-by-id task-id parent-pos)))
    
    (if existing-pos
        ;; Update existing task
        (save-excursion
          (goto-char existing-pos)
          (org-todo (if completed "DONE" "TODO"))
          (org-asana--update-heading task-name)
          (when due-on
            (org-deadline nil due-on))
          (org-set-property "ASANA_MODIFIED" modified-at))
      
      ;; Create new task
      (unless completed  ; Don't create new completed tasks
        (save-excursion
          (goto-char parent-pos)
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          (insert (format "**** TODO %s\n" task-name))
          (when due-on
            (org-deadline nil due-on))
          (org-set-property "ASANA_TASK_ID" task-id)
          (org-set-property "ASANA_MODIFIED" modified-at)
          (when (and notes (not (string-empty-p notes)))
            (org-end-of-meta-data t)
            (insert notes "\n")))))))

(defun org-asana--find-task-by-id (task-id parent-pos)
  "Find task with TASK-ID after PARENT-POS."
  (save-excursion
    (goto-char parent-pos)
    (let ((end-pos (save-excursion
                    (org-end-of-subtree t)
                    (point))))
      (when (re-search-forward 
             (format "^[ \t]*:ASANA_TASK_ID:[ \t]+%s" task-id)
             end-pos t)
        (org-back-to-heading t)
        (point)))))

(defun org-asana--update-heading (new-text)
  "Update current heading text to NEW-TEXT."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (let ((todo (match-string 2))
            (priority (match-string 3))
            (tags (match-string 5)))
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (insert (org-make-heading-string 
                new-text 
                (org-current-level)
                todo
                priority
                tags))))))

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

(defun org-asana--update-statistics ()
  "Update TODO statistics for all projects and sections."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Active Projects$" nil t)
      (org-update-statistics-cookies t))))

(provide 'org-asana)
;;; org-asana.el ends here