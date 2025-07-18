;;; org-asana-attachments.el --- Attachment support for org-asana -*- lexical-binding: t; -*-

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

;; This module provides attachment upload/download support for org-asana.
;; Handles file attachments for Asana tasks.

;;; Code:

(require 'org-asana)
(require 'url)
(require 'mm-util)

;;; Constants

(defconst org-asana--max-attachment-size (* 100 1024 1024)
  "Maximum attachment size in bytes (100MB).")

(defconst org-asana--attachment-url-ttl 3600
  "TTL for attachment download URLs in seconds.")

;;; Custom Variables

(defcustom org-asana-attachment-directory "~/org/asana-attachments/"
  "Directory for storing downloaded attachments."
  :type 'directory
  :group 'org-asana)

(defcustom org-asana-auto-download-attachments nil
  "Whether to automatically download attachments during sync."
  :type 'boolean
  :group 'org-asana)

;;; API Functions

(defun org-asana--fetch-attachment (attachment-gid)
  "Fetch attachment metadata for ATTACHMENT-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/attachments/%s" attachment-gid))))

(defun org-asana--fetch-task-attachments (task-gid)
  "Fetch all attachments for TASK-GID."
  (alist-get 'data
             (org-asana--make-request "GET"
                                    (format "/attachments?parent=%s" task-gid))))

(defun org-asana--delete-attachment (attachment-gid)
  "Delete attachment with ATTACHMENT-GID."
  (org-asana--make-request "DELETE"
                          (format "/attachments/%s" attachment-gid)))

;;; Upload Functions

(defun org-asana--validate-file-size (file-path)
  "Validate FILE-PATH size is within limits."
  (let ((size (nth 7 (file-attributes file-path))))
    (when (> size org-asana--max-attachment-size)
      (error "File too large: %s exceeds 100MB limit" 
             (file-name-nondirectory file-path)))
    size))

(defun org-asana--generate-multipart-boundary ()
  "Generate a unique multipart boundary string."
  (format "----EmacsBoundary%s" (md5 (format "%s%s" (current-time) (random)))))

(defun org-asana--build-multipart-body (task-gid file-path boundary)
  "Build multipart body for TASK-GID and FILE-PATH with BOUNDARY."
  (let ((file-name (file-name-nondirectory file-path)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Parent field
      (insert "--" boundary "\r\n")
      (insert "Content-Disposition: form-data; name=\"parent\"\r\n\r\n")
      (insert task-gid "\r\n")
      ;; File field
      (insert "--" boundary "\r\n")
      (insert (format "Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n"
                     (url-hexify-string file-name)))
      (insert "Content-Type: application/octet-stream\r\n\r\n")
      (insert-file-contents-literally file-path)
      (goto-char (point-max))
      (insert "\r\n--" boundary "--\r\n")
      (buffer-string))))

(defun org-asana--upload-attachment (task-gid file-path)
  "Upload FILE-PATH as attachment to TASK-GID."
  (org-asana--validate-file-size file-path)
  (let* ((boundary (org-asana--generate-multipart-boundary))
         (body (org-asana--build-multipart-body task-gid file-path boundary))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (org-asana--get-token)))
            ("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
         (url-request-data body))
    (with-current-buffer (url-retrieve-synchronously
                         (concat org-asana-api-base-url "/attachments") t t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-array-type 'list))
        (json-read)))))

;;; Download Functions

(defun org-asana--ensure-attachment-directory ()
  "Ensure attachment directory exists."
  (unless (file-exists-p org-asana-attachment-directory)
    (make-directory org-asana-attachment-directory t)))

(defun org-asana--generate-local-filename (attachment)
  "Generate local filename for ATTACHMENT."
  (let* ((name (alist-get 'name attachment))
         (gid (alist-get 'gid attachment))
         (extension (file-name-extension name))
         (base (file-name-sans-extension name)))
    (format "%s_%s%s" base gid
            (if extension (concat "." extension) ""))))

(defun org-asana--download-file (url local-path)
  "Download file from URL to LOCAL-PATH."
  (url-copy-file url local-path t))

(defun org-asana--download-attachment (attachment)
  "Download ATTACHMENT to local storage."
  (org-asana--ensure-attachment-directory)
  (let* ((download-url (alist-get 'download_url attachment))
         (local-filename (org-asana--generate-local-filename attachment))
         (local-path (expand-file-name local-filename
                                      org-asana-attachment-directory)))
    (when download-url
      (message "Downloading %s..." (alist-get 'name attachment))
      (org-asana--download-file download-url local-path)
      (message "Downloaded to %s" local-path)
      local-path)))

;;; Org Integration

(defun org-asana--format-attachment-link (attachment &optional local-path)
  "Format ATTACHMENT as Org link, optionally with LOCAL-PATH."
  (let ((name (alist-get 'name attachment))
        (permanent-url (alist-get 'permanent_url attachment)))
    (if local-path
        (format "[[file:%s][%s]]" local-path name)
      (format "[[%s][%s]]" permanent-url name))))

(defun org-asana--insert-attachments-section (attachments)
  "Insert ATTACHMENTS section in current buffer."
  (when attachments
    (insert "\n***** Attachments\n")
    (dolist (attachment attachments)
      (let ((local-path (when org-asana-auto-download-attachments
                         (org-asana--download-attachment attachment))))
        (insert (format "- %s\n" 
                       (org-asana--format-attachment-link attachment local-path)))))))

(defun org-asana--find-attachment-links ()
  "Find all attachment links in current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (links '()))
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" end t)
        (let ((target (match-string 1))
              (desc (match-string 2)))
          (when (or (string-prefix-p "file:" target)
                   (string-match-p "asana.*get_asset" target))
            (push (cons target desc) links))))
      (nreverse links))))

;;; Interactive Functions

(defun org-asana-attach-file (file-path)
  "Attach FILE-PATH to current task."
  (interactive "fFile to attach: ")
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (unless (file-exists-p file-path)
      (error "File not found: %s" file-path))
    (let ((response (org-asana--upload-attachment task-gid file-path)))
      (if (alist-get 'errors response)
          (error "Failed to upload: %s"
                 (alist-get 'message (car (alist-get 'errors response))))
        (let ((attachment (alist-get 'data response)))
          (org-asana--add-attachment-to-entry attachment)
          (message "File attached: %s" (alist-get 'name attachment)))))))

(defun org-asana--add-attachment-to-entry (attachment)
  "Add ATTACHMENT link to current entry."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    ;; Find or create attachments section
    (unless (search-forward "***** Attachments" (org-end-of-subtree t t) t)
      (insert "\n***** Attachments\n"))
    (end-of-line)
    (insert (format "\n- %s" (org-asana--format-attachment-link attachment)))))

(defun org-asana-download-attachment ()
  "Download attachment at point."
  (interactive)
  (let ((link (org-element-context)))
    (when (eq (org-element-type link) 'link)
      (let ((path (org-element-property :path link)))
        (cond
         ((string-prefix-p "file:" path)
          (message "Attachment already downloaded: %s" path))
         ((string-match "asset_id=\\([0-9]+\\)" path)
          (let ((attachment-gid (match-string 1 path)))
            (org-asana--download-attachment-by-gid attachment-gid)))
         (t (error "Not an Asana attachment link")))))))

(defun org-asana--download-attachment-by-gid (attachment-gid)
  "Download attachment by ATTACHMENT-GID."
  (let ((attachment (org-asana--fetch-attachment attachment-gid)))
    (when attachment
      (let ((local-path (org-asana--download-attachment attachment)))
        (when local-path
          ;; Update link to local file
          (let ((link (org-element-context)))
            (when (eq (org-element-type link) 'link)
              (let ((begin (org-element-property :begin link))
                    (end (org-element-property :end link)))
                (delete-region begin end)
                (insert (format "[[file:%s][%s]]" local-path
                               (alist-get 'name attachment)))))))))))

(defun org-asana-download-all-attachments ()
  "Download all attachments for current task."
  (interactive)
  (let ((task-gid (org-entry-get (point) "ASANA-TASK-GID")))
    (unless task-gid
      (error "Not on an Asana task"))
    (let ((attachments (org-asana--fetch-task-attachments task-gid))
          (count 0))
      (dolist (attachment attachments)
        (when (org-asana--download-attachment attachment)
          (setq count (1+ count))))
      (message "Downloaded %d attachments" count))))

(defun org-asana-delete-attachment ()
  "Delete attachment at point from Asana."
  (interactive)
  (let ((link (org-element-context)))
    (when (eq (org-element-type link) 'link)
      (let ((path (org-element-property :path link)))
        (when (string-match "asset_id=\\([0-9]+\\)" path)
          (let ((attachment-gid (match-string 1 path)))
            (when (yes-or-no-p "Delete this attachment from Asana? ")
              (org-asana--delete-attachment attachment-gid)
              ;; Remove link from buffer
              (let ((begin (org-element-property :begin link))
                    (end (org-element-property :end link)))
                (delete-region begin end))
              (message "Attachment deleted"))))))))

(defun org-asana-open-attachment ()
  "Open attachment at point."
  (interactive)
  (let ((link (org-element-context)))
    (when (eq (org-element-type link) 'link)
      (let ((path (org-element-property :path link)))
        (cond
         ((string-prefix-p "file:" path)
          (find-file (substring path 5)))
         ((string-match-p "asana.*get_asset" path)
          (browse-url path))
         (t (error "Not an attachment link")))))))

;;; Drag and Drop Support

(defun org-asana-dnd-attach-file (event)
  "Attach file dragged to EVENT location."
  (interactive "e")
  (let ((file (car (nth 2 event))))
    (when (and file (file-exists-p file))
      (goto-char (posn-point (event-end event)))
      (org-asana-attach-file file))))

(provide 'org-asana-attachments)
;;; org-asana-attachments.el ends here