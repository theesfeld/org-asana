;;; org-asana.el --- Two-way sync between Org-mode and Asana  -*- lexical-binding: t; -*-

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

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (json "1.5") (url "0"))
;; Keywords: tools, asana, org-mode, productivity
;; URL: https://github.com/username/org-asana

;;; Commentary:

;; This package provides two-way synchronization between Emacs Org-mode
;; and Asana tasks.  It follows a functional programming approach with
;; clear separation between data fetching, transformation, and rendering.

;; Main features:
;; - Fetch tasks from Asana and convert to Org headings
;; - Sync changes back to Asana
;; - Rate limiting protection
;; - Visual task faces for priorities and deadlines
;; - Progress indicators
;; - Org-agenda integration

;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'subr-x)

;;; Custom Variables

(defgroup org-asana nil
  "Options for org-asana."
  :tag "Org Asana"
  :group 'org)

(defcustom org-asana-token nil
  "Personal Access Token for Asana API."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-org-file "~/org/asana.org"
  "Path to the Org file for Asana tasks."
  :type 'file
  :group 'org-asana)

(defcustom org-asana-fetch-metadata t
  "Whether to fetch comments and attachments for tasks."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-show-progress-indicators t
  "Whether to show [done/total] progress indicators on headings."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-rate-limit-delay 0.4
  "Delay in seconds between API requests to respect rate limits."
  :type 'number
  :group 'org-asana)

(defcustom org-asana-debug nil
  "Enable debug messages during sync operations."
  :type 'boolean
  :group 'org-asana)

;;; Constants

(defconst org-asana-api-base-url "https://app.asana.com/api/1.0"
  "Base URL for Asana API requests.")

(defconst org-asana-rate-limit-max 150
  "Maximum API requests per minute.")

(defvar org-asana--rate-limit-remaining org-asana-rate-limit-max
  "Remaining API calls in current rate limit window.")

(defvar org-asana--rate-limit-reset nil
  "Time when rate limit resets.")

(provide 'org-asana)
;;; org-asana.el ends here