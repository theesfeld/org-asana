;;; org-asana-cache.el --- Caching layer for org-asana -*- lexical-binding: t; -*-

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

;; This module provides a comprehensive caching layer for org-asana.
;; Reduces API calls and improves performance.

;;; Code:

(require 'org-asana)

;;; Custom Variables

(defcustom org-asana-enable-cache t
  "Whether to enable caching."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-cache-directory
  (expand-file-name "org-asana-cache" user-emacs-directory)
  "Directory for persistent cache storage."
  :type 'directory
  :group 'org-asana)

;;; Cache Structures

(defvar org-asana--cache-registry nil
  "Registry of all active caches.")

(cl-defstruct org-asana-cache
  "Cache structure for org-asana data."
  name          ; Cache identifier
  data          ; Hash table of cached data
  ttl           ; Time to live in seconds
  timestamps    ; Hash table of entry timestamps
  hit-count     ; Number of cache hits
  miss-count    ; Number of cache misses
  persistent-p) ; Whether to persist to disk

;;; Cache Creation

(defun org-asana--create-cache (name &optional ttl persistent-p)
  "Create a cache with NAME, optional TTL and PERSISTENT-P."
  (let ((cache (make-org-asana-cache
                :name name
                :data (make-hash-table :test 'equal)
                :ttl (or ttl 3600)
                :timestamps (make-hash-table :test 'equal)
                :hit-count 0
                :miss-count 0
                :persistent-p persistent-p)))
    (push (cons name cache) org-asana--cache-registry)
    cache))

(defun org-asana--get-cache (name)
  "Get cache by NAME."
  (cdr (assoc name org-asana--cache-registry)))

(defun org-asana--ensure-cache (name &optional ttl persistent-p)
  "Ensure cache NAME exists, creating if necessary."
  (or (org-asana--get-cache name)
      (org-asana--create-cache name ttl persistent-p)))

;;; Cache Operations

(defun org-asana--cache-valid-p (cache key)
  "Check if KEY in CACHE is still valid."
  (when-let ((timestamp (gethash key (org-asana-cache-timestamps cache))))
    (< (float-time (time-subtract (current-time) timestamp))
       (org-asana-cache-ttl cache))))

(defun org-asana--cache-get (cache key)
  "Get KEY from CACHE if valid."
  (when (and org-asana-enable-cache
             (org-asana--cache-valid-p cache key))
    (cl-incf (org-asana-cache-hit-count cache))
    (gethash key (org-asana-cache-data cache))))

(defun org-asana--cache-put (cache key value)
  "Put VALUE for KEY in CACHE."
  (when org-asana-enable-cache
    (puthash key value (org-asana-cache-data cache))
    (puthash key (current-time) (org-asana-cache-timestamps cache))
    (when (org-asana-cache-persistent-p cache)
      (org-asana--cache-persist cache))))

(defun org-asana--cache-remove (cache key)
  "Remove KEY from CACHE."
  (remhash key (org-asana-cache-data cache))
  (remhash key (org-asana-cache-timestamps cache)))

(defun org-asana--cache-clear (cache)
  "Clear all entries from CACHE."
  (clrhash (org-asana-cache-data cache))
  (clrhash (org-asana-cache-timestamps cache))
  (setf (org-asana-cache-hit-count cache) 0)
  (setf (org-asana-cache-miss-count cache) 0))

;;; Persistence

(defun org-asana--cache-file-path (cache)
  "Get file path for persistent CACHE."
  (expand-file-name (format "%s.el" (org-asana-cache-name cache))
                    org-asana-cache-directory))

(defun org-asana--cache-persist (cache)
  "Persist CACHE to disk."
  (when (org-asana-cache-persistent-p cache)
    (unless (file-exists-p org-asana-cache-directory)
      (make-directory org-asana-cache-directory t))
    (let ((file (org-asana--cache-file-path cache))
          (data (org-asana--serialize-cache cache)))
      (with-temp-file file
        (prin1 data (current-buffer))))))

(defun org-asana--cache-load (cache)
  "Load persistent CACHE from disk."
  (let ((file (org-asana--cache-file-path cache)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (read (current-buffer))))
          (org-asana--deserialize-cache cache data))))))

(defun org-asana--serialize-cache (cache)
  "Serialize CACHE for storage."
  (let ((entries '()))
    (maphash (lambda (key value)
               (when (org-asana--cache-valid-p cache key)
                 (push (list key value
                           (gethash key (org-asana-cache-timestamps cache)))
                       entries)))
             (org-asana-cache-data cache))
    entries))

(defun org-asana--deserialize-cache (cache data)
  "Deserialize DATA into CACHE."
  (dolist (entry data)
    (puthash (car entry) (cadr entry) (org-asana-cache-data cache))
    (puthash (car entry) (caddr entry) (org-asana-cache-timestamps cache))))

;;; Specialized Caches

(defvar org-asana--request-cache nil
  "Cache for API requests.")

(defvar org-asana--workspace-cache nil
  "Cache for workspace data.")

(defvar org-asana--project-cache nil
  "Cache for project data.")

(defun org-asana--init-caches ()
  "Initialize all caches."
  (setq org-asana--request-cache
        (org-asana--ensure-cache "requests" 300 nil))
  (setq org-asana--workspace-cache
        (org-asana--ensure-cache "workspace" 3600 t))
  (setq org-asana--project-cache
        (org-asana--ensure-cache "projects" 1800 t))
  ;; Load persistent caches
  (org-asana--cache-load org-asana--workspace-cache)
  (org-asana--cache-load org-asana--project-cache))

;;; Request Caching

(defun org-asana--request-cache-key (method endpoint params)
  "Generate cache key for METHOD ENDPOINT with PARAMS."
  (format "%s:%s:%s" method endpoint (prin1-to-string params)))

(defun org-asana--cached-request (method endpoint &optional data)
  "Make cached request to METHOD ENDPOINT with optional DATA."
  (if (and org-asana-enable-cache (equal method "GET"))
      (let* ((cache-key (org-asana--request-cache-key method endpoint data))
             (cached (org-asana--cache-get org-asana--request-cache cache-key)))
        (if cached
            cached
          (cl-incf (org-asana-cache-miss-count org-asana--request-cache))
          (let ((result (org-asana--make-request method endpoint data)))
            (org-asana--cache-put org-asana--request-cache cache-key result)
            result)))
    (org-asana--make-request method endpoint data)))

;;; Cache Management

(defun org-asana-cache-statistics ()
  "Display cache statistics."
  (interactive)
  (let ((buffer (get-buffer-create "*Asana Cache Statistics*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Asana Cache Statistics\n")
      (insert "======================\n\n")
      (dolist (entry org-asana--cache-registry)
        (let ((cache (cdr entry)))
          (insert (format "Cache: %s\n" (org-asana-cache-name cache)))
          (insert (format "  Entries: %d\n" 
                         (hash-table-count (org-asana-cache-data cache))))
          (insert (format "  Hit rate: %.1f%% (%d hits, %d misses)\n"
                         (org-asana--cache-hit-rate cache)
                         (org-asana-cache-hit-count cache)
                         (org-asana-cache-miss-count cache)))
          (insert (format "  TTL: %d seconds\n" (org-asana-cache-ttl cache)))
          (insert (format "  Persistent: %s\n\n" 
                         (if (org-asana-cache-persistent-p cache) "Yes" "No")))))
      (goto-char (point-min))
      (view-mode))
    (display-buffer buffer)))

(defun org-asana--cache-hit-rate (cache)
  "Calculate hit rate for CACHE."
  (let ((hits (org-asana-cache-hit-count cache))
        (misses (org-asana-cache-miss-count cache)))
    (if (zerop (+ hits misses))
        0.0
      (* 100.0 (/ (float hits) (+ hits misses))))))

(defun org-asana-clear-cache (&optional cache-name)
  "Clear cache by CACHE-NAME or all caches."
  (interactive
   (list (completing-read "Clear cache (empty for all): "
                         (mapcar #'car org-asana--cache-registry)
                         nil nil nil nil "")))
  (if (string-empty-p cache-name)
      (progn
        (dolist (entry org-asana--cache-registry)
          (org-asana--cache-clear (cdr entry)))
        (message "All caches cleared"))
    (if-let ((cache (org-asana--get-cache cache-name)))
        (progn
          (org-asana--cache-clear cache)
          (message "Cache '%s' cleared" cache-name))
      (error "Cache '%s' not found" cache-name))))

(defun org-asana-save-caches ()
  "Save all persistent caches."
  (interactive)
  (dolist (entry org-asana--cache-registry)
    (let ((cache (cdr entry)))
      (when (org-asana-cache-persistent-p cache)
        (org-asana--cache-persist cache))))
  (message "Persistent caches saved"))

;;; Automatic Cache Management

(defun org-asana--cache-cleanup ()
  "Clean up expired entries from all caches."
  (dolist (entry org-asana--cache-registry)
    (let ((cache (cdr entry)))
      (maphash (lambda (key _value)
                 (unless (org-asana--cache-valid-p cache key)
                   (org-asana--cache-remove cache key)))
               (org-asana-cache-data cache)))))

(defvar org-asana--cache-cleanup-timer nil
  "Timer for periodic cache cleanup.")

(defun org-asana--start-cache-cleanup-timer ()
  "Start periodic cache cleanup."
  (when org-asana--cache-cleanup-timer
    (cancel-timer org-asana--cache-cleanup-timer))
  (setq org-asana--cache-cleanup-timer
        (run-with-timer 600 600 #'org-asana--cache-cleanup)))

(defun org-asana--stop-cache-cleanup-timer ()
  "Stop periodic cache cleanup."
  (when org-asana--cache-cleanup-timer
    (cancel-timer org-asana--cache-cleanup-timer)
    (setq org-asana--cache-cleanup-timer nil)))

;;; Hook Integration

(add-hook 'kill-emacs-hook #'org-asana-save-caches)

;;; Initialization

(org-asana--init-caches)
(when org-asana-enable-cache
  (org-asana--start-cache-cleanup-timer))

(provide 'org-asana-cache)
;;; org-asana-cache.el ends here