;;; test-org-asana.el --- Test script for org-asana -*- lexical-binding: t; -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Simple test script to verify org-asana functionality

;;; Code:

(require 'org-asana)

(defun test-org-asana-modules ()
  "Test loading of all org-asana modules."
  (interactive)
  (let ((modules '(org-asana-cache
                   org-asana-custom-fields
                   org-asana-dependencies
                   org-asana-subtasks
                   org-asana-webhooks
                   org-asana-attachments
                   org-asana-users
                   org-asana-types
                   org-asana-browser))
        (failed-modules '()))
    (dolist (module modules)
      (condition-case err
          (progn
            (require module nil t)
            (message "✓ Successfully loaded %s" module))
        (error
         (push module failed-modules)
         (message "✗ Failed to load %s: %s" module (error-message-string err)))))
    (if failed-modules
        (message "\nFailed modules: %s" failed-modules)
      (message "\nAll modules loaded successfully!"))))

(defun test-org-asana-connection ()
  "Test the Asana API connection."
  (interactive)
  (condition-case err
      (progn
        (org-asana-test-connection)
        (message "✓ Connection test passed"))
    (error
     (message "✗ Connection test failed: %s" (error-message-string err)))))

(defun test-org-asana-cache ()
  "Test cache functionality if module is loaded."
  (interactive)
  (if (fboundp 'org-asana-cache-statistics)
      (progn
        (org-asana-cache-statistics)
        (message "✓ Cache module working"))
    (message "✗ Cache module not loaded")))

(defun test-org-asana-browser ()
  "Test browser functionality if module is loaded."
  (interactive)
  (if (fboundp 'org-asana-browse-workspace)
      (progn
        (message "Opening task browser...")
        (org-asana-browse-workspace))
    (message "✗ Browser module not loaded")))

(defun test-org-asana-all ()
  "Run all org-asana tests."
  (interactive)
  (test-org-asana-modules)
  (sit-for 1)
  (test-org-asana-connection)
  (sit-for 1)
  (test-org-asana-cache))

(provide 'test-org-asana)
;;; test-org-asana.el ends here