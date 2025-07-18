# org-asana

Advanced two-way synchronization between Emacs Org-mode and Asana tasks. This package provides comprehensive integration with the Asana API, allowing you to manage your Asana tasks directly from Org-mode with full bidirectional synchronization.

## Overview

org-asana transforms your Org-mode into a powerful Asana client. When you run `org-asana-sync`, the package fetches all your assigned tasks from Asana and creates a structured Org file with proper hierarchy (Projects → Sections → Tasks). Changes made in either Asana or Org-mode are synchronized bidirectionally - mark a task as DONE in Org-mode and it's marked complete in Asana; add a comment in Org-mode and it appears as a story in Asana.

The package is designed with a modular architecture, allowing you to enable only the features you need. Whether you just want basic task sync or need advanced features like custom fields, subtasks, and real-time webhooks, org-asana scales to meet your needs.

## Installation

### Using use-package (Emacs 30.1+)

```elisp
(use-package org-asana
  :vc (:url "https://github.com/theesfeld/org-asana")
  :custom
  ;; Required: Your Asana Personal Access Token
  (org-asana-token "your-personal-access-token")
  ;; Required: Path to your Asana org file
  (org-asana-org-file "~/org/asana.org")
  ;; Optional: Enable the modules you want
  (org-asana-modules '(org-asana-cache
                      org-asana-custom-fields
                      org-asana-subtasks
                      org-asana-attachments
                      org-asana-users
                      org-asana-types
                      org-asana-browser))
  :config
  ;; Optional: Set up key bindings
  (define-key org-mode-map (kbd "C-c a s") #'org-asana-sync)
  (define-key org-mode-map (kbd "C-c a c") #'org-asana-add-comment)
  (define-key org-mode-map (kbd "C-c a b") #'org-asana-browse))
```

### Manual Installation

```elisp
(add-to-list 'load-path "/path/to/org-asana")
(require 'org-asana)
(setq org-asana-token "your-personal-access-token")
(setq org-asana-org-file "~/org/asana.org")
```

## Getting Your Asana Token

1. Log into Asana in your web browser
2. Go to "My Settings" → "Apps" → "Manage Developer Apps"
3. Click "Create New Personal Access Token"
4. Give it a descriptive name and copy the token
5. Add the token to your configuration

Alternatively, you can store your token in authinfo:
```
machine app.asana.com login apikey password your-token-here
```

## Configuration Options

### Core Settings

```elisp
;; Authentication
(setq org-asana-token nil)                          ; Your PAT or nil to use authinfo
(setq org-asana-authinfo-machine "app.asana.com")   ; Machine name in authinfo (default)

;; File location
(setq org-asana-org-file "~/org/asana.org")         ; Where to store Asana tasks

;; Sync behavior
(setq org-asana-fetch-metadata t)                   ; Fetch comments/attachments (default: t)
(setq org-asana-show-progress-indicators t)         ; Show [done/total] counts (default: t)
(setq org-asana-apply-faces t)                      ; Apply visual faces (default: t)
(setq org-asana-enable-save-hook nil)               ; Auto-sync on save (default: nil)
(setq org-asana-enable-todo-hook nil)               ; Auto-sync on TODO change (default: nil)
(setq org-asana-enable-agenda-integration t)        ; Add to org-agenda (default: t)

;; Performance
(setq org-asana-rate-limit-delay 0.4)               ; Delay between API calls (default: 0.4)
(setq org-asana-debug nil)                          ; Enable debug messages (default: nil)

;; Modules to load
(setq org-asana-modules                             ; Default: all modules
      '(org-asana-cache
        org-asana-custom-fields
        org-asana-dependencies
        org-asana-subtasks
        org-asana-webhooks
        org-asana-attachments
        org-asana-users
        org-asana-types
        org-asana-browser))
```

### Module-Specific Settings

#### Cache Module
```elisp
(setq org-asana-enable-cache t)                                        ; Enable caching (default: t)
(setq org-asana-cache-directory                                       ; Cache storage location
      (expand-file-name "org-asana-cache" user-emacs-directory))      ; (default: ~/.emacs.d/org-asana-cache)
```

#### Attachments Module
```elisp
(setq org-asana-attachment-directory "~/org/asana-attachments/")       ; Download location (default)
(setq org-asana-auto-download-attachments nil)                        ; Auto-download on sync (default: nil)
```

#### Task Types Module
```elisp
(setq org-asana-show-type-indicators t)                               ; Show type prefixes (default: t)
(setq org-asana-milestone-prefix "[MILESTONE] ")                      ; Milestone prefix (default)
(setq org-asana-approval-prefix "[APPROVAL] ")                        ; Approval prefix (default)
(setq org-asana-separator-prefix "────── ")                           ; Separator prefix (default)
```

#### Browser Module
```elisp
(setq org-asana-browser-page-size 50)                                 ; Tasks per page (default: 50)
(setq org-asana-browser-show-completed t)                             ; Show completed tasks (default: t)
```

#### Custom Fields Module
```elisp
(setq org-asana-custom-field-prefix "ASANA-CF-")                      ; Property prefix (default)
```

#### User Management Module
```elisp
(setq org-asana-assignee-tags nil)                                    ; Tag→user mapping (default: nil)
;; Example: '(("john" . "1234567890") ("jane" . "0987654321"))
```

#### Webhooks Module
```elisp
(setq org-asana-webhook-endpoint nil)                                 ; Your webhook URL (default: nil)
(setq org-asana-webhook-server-port 8080)                            ; Local server port (default: 8080)
(setq org-asana-webhook-filters                                       ; Event filters (default below)
      '(((resource_type . "task")
         (action . "changed")
         (fields . ("completed" "name" "due_on" "assignee")))))
```

## Usage Guide

### Initial Setup and First Sync

After configuring your token and org file path, run your first sync:

```
M-x org-asana-sync
```

This will:
1. Connect to Asana and fetch all incomplete tasks assigned to you
2. Create or update your org-asana file with the proper structure
3. Apply visual faces to highlight priorities and deadlines
4. Add the file to your org-agenda if enabled

The resulting Org file structure looks like:
```org
#+TITLE: Asana Tasks
#+STARTUP: overview

* Project Name
** Section Name
*** TODO Task Title
    :PROPERTIES:
    :ASANA-TASK-GID: 1234567890
    :ASANA-PROJECT: Project Name
    :ASANA-SECTION: Section Name
    :ASANA-DUE-DATE: 2025-07-31
    :ASANA-ASSIGNEE: Your Name
    :ASANA-MODIFIED-AT: 2025-07-18T10:30:00.000Z
    :ASANA-PERMALINK: https://app.asana.com/0/0/1234567890/f
    :END:
    
    Task description here...
    
    :COMMENTS:
    [2025-07-18 10:30] John Doe:
    This is a comment from Asana
    :END:
```

### Daily Workflow

#### Managing Tasks

**Marking tasks complete**: Simply change the TODO state to DONE:
```
C-c C-t → DONE
```
On the next sync, this will mark the task as complete in Asana.

**Adding new tasks**: Create a new heading with TODO state and add the special property:
```org
*** TODO New Task Title
    :PROPERTIES:
    :ASANA-TASK-GID: new
    :END:
```
On sync, this will create the task in Asana and update the GID.

**Adding comments**: Use the comment command while on a task:
```
C-c a c → Enter your comment
```
This adds the comment to the :COMMENTS: drawer and syncs to Asana.

#### Using the Task Browser

The browser module provides an interactive interface without full sync:
```
M-x org-asana-browse
```

In the browser:
- `RET` - Open task in org file
- `o` - Open in web browser
- `c` - Toggle completion
- `a` - Assign to user
- `d` - Set due date
- `s` - Search tasks
- `f` - Filter tasks
- `r`/`g` - Refresh
- `q` - Quit

#### Working with Attachments

If the attachments module is enabled:
```
M-x org-asana-attach → Select file
```
This uploads the file to Asana (max 100MB). Attachments appear as links in the task.

To download attachments:
```
M-x org-asana-download-attachment    ; At point
M-x org-asana-download-all-attachments ; For whole task
```

#### Managing Dependencies

With the dependencies module:
```
M-x org-asana-add-dependency
M-x org-asana-show-dependencies
```
Dependencies are stored in the BLOCKER property and synced with Asana.

#### Custom Fields (Premium)

If your Asana workspace has custom fields, they automatically sync as properties:
```org
:ASANA-CF-Priority: High
:ASANA-CF-Status: In Progress
:ASANA-CF-Estimated-Hours: 5
```

### Org-Agenda Integration

org-asana automatically adds custom agenda commands:
- `C-c a A a` - All Asana tasks
- `C-c a A t` - Today's Asana tasks  
- `C-c a A w` - This week's Asana tasks
- `C-c a A d` - Tasks with upcoming deadlines

You can also use standard agenda queries:
```elisp
;; Tasks due within 7 days
(org-tags-view nil "ASANA-DUE-DATE<=\"<+7d>\"")

;; High priority tasks (if using custom fields)
(org-tags-view nil "ASANA-CF-Priority=\"High\"")
```

### Capture Templates

Add Asana tasks via org-capture:
```elisp
(setq org-capture-templates
      '(("a" "Asana Task" entry 
         (file org-asana-org-file)
         "* TODO %?\n:PROPERTIES:\n:ASANA-TASK-GID: new\n:END:\n"
         :empty-lines 1)))
```

### Advanced Features

#### Subtasks

With the subtasks module, nested tasks sync as Org hierarchy:
```org
*** TODO Parent Task
**** TODO Subtask 1
**** DONE Subtask 2
***** TODO Sub-subtask
```

#### Task Types

Different task types get special indicators and faces:
```org
*** TODO [MILESTONE] Project Launch
*** TODO [APPROVAL] Review Document
*** ────── Section Separator ──────
```

#### Real-time Updates (Webhooks)

For instant updates without polling:
1. Set up a public HTTPS endpoint
2. Configure: `(setq org-asana-webhook-endpoint "https://your-domain.com/webhook")`
3. Register: `M-x org-asana-register-webhook`

## Troubleshooting

### Connection Issues
```
M-x org-asana-test-connection
```
This verifies your token and API access.

### Debug Mode
```elisp
(setq org-asana-debug t)
```
Enables detailed logging of API calls and sync operations.

### Cache Issues
```
M-x org-asana-cache-status    ; View cache statistics
M-x org-asana-clear-cache      ; Clear cache if needed
```

### Common Problems

**"Authentication failed"**: Check your token is valid and not expired.

**"Rate limit exceeded"**: The package handles this automatically with exponential backoff. If persistent, increase `org-asana-rate-limit-delay`.

**Tasks not syncing**: Ensure tasks are assigned to you in Asana. The package only syncs tasks where you are the assignee.

**Conflicts**: Local changes take precedence. If a task was modified in both places, your Org changes win.

## Module System

org-asana uses a modular architecture. Each module adds specific functionality:

- **org-asana-cache**: Reduces API calls through intelligent caching
- **org-asana-custom-fields**: Syncs Asana Premium custom fields
- **org-asana-dependencies**: Manages task dependencies and blockers
- **org-asana-subtasks**: Recursive subtask synchronization (up to 5 levels)
- **org-asana-webhooks**: Real-time updates via webhooks
- **org-asana-attachments**: File upload/download support
- **org-asana-users**: Assignee management and user operations
- **org-asana-types**: Task type indicators (milestones, approvals)
- **org-asana-browser**: Interactive task browser interface

Enable only what you need to keep things fast and simple.

## Performance Tips

1. **Use caching**: The cache module significantly reduces API calls
2. **Disable metadata fetch**: Set `org-asana-fetch-metadata nil` if you don't need comments
3. **Increase rate limit delay**: For large workspaces, increase `org-asana-rate-limit-delay`
4. **Use the browser**: For quick lookups, the browser is faster than full sync
5. **Limit modules**: Only load modules you actively use

## Contributing

This package follows strict coding standards:
- GNU Emacs Lisp conventions
- Functional programming principles  
- One function, one task philosophy
- Comprehensive docstrings
- No classes or complex data structures

Pull requests welcome! Please ensure all functions are small, pure where possible, and well-documented.

## License

Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Changelog

### Version 2.0.0
- Complete rewrite with modular architecture
- Added bidirectional sync with TODO states
- Added comment synchronization
- Added 10 optional feature modules
- Enhanced error handling and rate limiting
- Improved performance with caching support