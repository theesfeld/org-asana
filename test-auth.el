;;; test-auth.el --- Test org-asana with provided token -*- lexical-binding: t; -*-

;; Test the org-asana sync with the provided token

(require 'org-asana)

;; Set the token
(setq org-asana-token "2/1210175301082801/1210805683922026:b321aa0433c14828f017821a06f00684")

;; Set a test file
(setq org-asana-org-file "/tmp/test-asana.org")

;; Enable debug mode
(setq org-asana-debug t)

;; Test the sync
(condition-case err
    (progn
      (message "Starting org-asana sync test...")
      (org-asana-sync)
      (message "Sync completed successfully!"))
  (error
   (message "Error during sync: %s" (error-message-string err))
   (backtrace)))

;; Display the result
(when (file-exists-p org-asana-org-file)
  (find-file org-asana-org-file))

;;; test-auth.el ends here