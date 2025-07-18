;;; org-asana-custom-fields.el --- Custom fields support for org-asana -*- lexical-binding: t; -*-

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

;; This module provides custom fields support for org-asana.
;; Handles reading, displaying, and updating Asana custom fields.

;;; Code:

(require 'org-asana)

;;; Custom Variables

(defcustom org-asana-custom-field-prefix "ASANA-CF-"
  "Prefix for custom field properties in Org."
  :type 'string
  :group 'org-asana)

;;; Cache Management

(defvar org-asana--custom-fields-cache nil
  "Cache for workspace custom field definitions.")

(defvar org-asana--custom-fields-cache-time nil
  "Time when custom fields cache was last updated.")

(defconst org-asana--custom-fields-cache-ttl 3600
  "TTL for custom fields cache in seconds.")

;;; Cache Functions

(defun org-asana--custom-fields-cache-valid-p ()
  "Return t if custom fields cache is still valid."
  (and org-asana--custom-fields-cache
       org-asana--custom-fields-cache-time
       (< (float-time (time-subtract (current-time)
                                    org-asana--custom-fields-cache-time))
          org-asana--custom-fields-cache-ttl)))

(defun org-asana--clear-custom-fields-cache ()
  "Clear the custom fields cache."
  (setq org-asana--custom-fields-cache nil
        org-asana--custom-fields-cache-time nil))

;;; API Functions

(defun org-asana--fetch-workspace-custom-fields (workspace-gid)
  "Fetch custom field definitions for WORKSPACE-GID."
  (let ((endpoint (format "/workspaces/%s/custom_fields" workspace-gid)))
    (alist-get 'data (org-asana--make-request "GET" endpoint))))

(defun org-asana--get-cached-custom-fields (workspace-gid)
  "Get custom fields for WORKSPACE-GID, using cache if valid."
  (if (org-asana--custom-fields-cache-valid-p)
      org-asana--custom-fields-cache
    (let ((fields (org-asana--fetch-workspace-custom-fields workspace-gid)))
      (setq org-asana--custom-fields-cache fields
            org-asana--custom-fields-cache-time (current-time))
      fields)))

;;; Field Type Functions

(defun org-asana--custom-field-type (field)
  "Get type of custom FIELD."
  (alist-get 'type field))

(defun org-asana--custom-field-value (field)
  "Extract value from custom FIELD based on its type."
  (let ((type (org-asana--custom-field-type field)))
    (cond
     ((equal type "text") (alist-get 'text_value field))
     ((equal type "number") (alist-get 'number_value field))
     ((equal type "enum") (alist-get 'name (alist-get 'enum_value field)))
     ((equal type "multi_enum")
      (mapconcat (lambda (v) (alist-get 'name v))
                 (alist-get 'multi_enum_values field) ", "))
     ((equal type "date")
      (let ((date-value (alist-get 'date_value field)))
        (when date-value
          (alist-get 'date date-value))))
     ((equal type "people")
      (mapconcat (lambda (p) (alist-get 'name p))
                 (alist-get 'people_value field) ", "))
     (t (alist-get 'display_value field)))))

(defun org-asana--format-custom-field-name (field-name)
  "Format FIELD-NAME for use as Org property."
  (concat org-asana-custom-field-prefix
          (upcase (replace-regexp-in-string "[^A-Za-z0-9]" "-" field-name))))

;;; Property Conversion Functions

(defun org-asana--custom-fields-to-properties (custom-fields)
  "Convert CUSTOM-FIELDS to Org property list."
  (let ((props '()))
    (dolist (field custom-fields)
      (let ((name (org-asana--format-custom-field-name
                   (alist-get 'name field)))
            (value (org-asana--custom-field-value field)))
        (when value
          (push (cons name (format "%s" value)) props))))
    (nreverse props)))

(defun org-asana--extract-custom-field-updates ()
  "Extract custom field updates from current Org entry."
  (let ((props (org-entry-properties))
        (updates '()))
    (dolist (prop props)
      (when (string-prefix-p org-asana-custom-field-prefix (car prop))
        (push prop updates)))
    updates))

;;; Update Preparation Functions

(defun org-asana--find-custom-field-by-name (name custom-fields)
  "Find custom field definition by NAME in CUSTOM-FIELDS."
  (seq-find (lambda (field)
              (equal (upcase (replace-regexp-in-string "[^A-Za-z0-9]" "-"
                                                      (alist-get 'name field)))
                     (upcase (string-remove-prefix org-asana-custom-field-prefix name))))
            custom-fields))

(defun org-asana--prepare-enum-value (value field-def)
  "Prepare enum VALUE using FIELD-DEF."
  (let ((enum-options (alist-get 'enum_options field-def)))
    (seq-find (lambda (opt)
                (equal (alist-get 'name opt) value))
              enum-options)))

(defun org-asana--prepare-multi-enum-values (value field-def)
  "Prepare multi-enum VALUES using FIELD-DEF."
  (let ((enum-options (alist-get 'enum_options field-def))
        (values (split-string value ", ")))
    (delq nil
          (mapcar (lambda (v)
                    (let ((opt (seq-find (lambda (opt)
                                          (equal (alist-get 'name opt) v))
                                        enum-options)))
                      (when opt (alist-get 'gid opt))))
                  values))))

(defun org-asana--prepare-custom-field-value (value field-def)
  "Prepare VALUE for custom field update based on FIELD-DEF."
  (let ((type (alist-get 'type field-def)))
    (cond
     ((equal type "text") value)
     ((equal type "number") (string-to-number value))
     ((equal type "enum")
      (let ((enum-opt (org-asana--prepare-enum-value value field-def)))
        (when enum-opt (alist-get 'gid enum-opt))))
     ((equal type "multi_enum")
      (org-asana--prepare-multi-enum-values value field-def))
     ((equal type "date") value)
     ((equal type "people")
      ;; For now, people fields are read-only
      nil)
     (t value))))

;;; Main Update Function

(defun org-asana--prepare-custom-fields-update (workspace-gid)
  "Prepare custom fields update data for WORKSPACE-GID."
  (let ((updates (org-asana--extract-custom-field-updates))
        (field-defs (org-asana--get-cached-custom-fields workspace-gid))
        (custom-fields-data '()))
    (dolist (update updates)
      (let* ((prop-name (car update))
             (prop-value (cdr update))
             (field-def (org-asana--find-custom-field-by-name prop-name field-defs)))
        (when field-def
          (let ((field-gid (alist-get 'gid field-def))
                (prepared-value (org-asana--prepare-custom-field-value
                                prop-value field-def)))
            (when prepared-value
              (push (cons field-gid prepared-value) custom-fields-data))))))
    custom-fields-data))

;;; Integration Functions

(defun org-asana--update-task-custom-fields (task-gid workspace-gid)
  "Update custom fields for TASK-GID in WORKSPACE-GID."
  (let ((custom-fields (org-asana--prepare-custom-fields-update workspace-gid)))
    (when custom-fields
      (org-asana--make-request
       "PUT"
       (format "/tasks/%s" task-gid)
       `((data . ((custom_fields . ,custom-fields))))))))

(defun org-asana--add-custom-fields-to-opt-fields (opt-fields)
  "Add custom fields parameters to OPT-FIELDS."
  (concat opt-fields
          ",custom_fields.gid,custom_fields.name,custom_fields.type,"
          "custom_fields.display_value,custom_fields.enum_value,"
          "custom_fields.text_value,custom_fields.number_value,"
          "custom_fields.date_value,custom_fields.people_value,"
          "custom_fields.multi_enum_values"))

(provide 'org-asana-custom-fields)
;;; org-asana-custom-fields.el ends here