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

(provide 'org-asana)
;;; org-asana.el ends here