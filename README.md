# org-asana

Advanced two-way synchronization between Emacs Org-mode and Asana tasks.

## Features

### Core Functionality
- **Bidirectional Sync**: Full two-way synchronization between Org-mode and Asana
- **TODO States**: Maps Org TODO/DONE states to Asana completed status
- **Comments**: Sync comments between Org :COMMENTS: drawer and Asana stories
- **Rate Limiting**: Automatic rate limit protection with exponential backoff
- **Progress Indicators**: Shows [done/total] task counts
- **Visual Faces**: Custom faces for priorities and deadlines
- **Org Agenda Integration**: Seamless integration with org-agenda

### Optional Modules

The following modules can be enabled via `org-asana-modules`:

#### Cache Module (`org-asana-cache`)
- Reduces API calls through intelligent caching
- Configurable TTL for different data types
- Persistent cache storage
- Cache statistics and management

#### Custom Fields (`org-asana-custom-fields`)
- Support for Asana Premium custom fields
- Maps to Org properties with ASANA-CF- prefix
- Supports all field types: text, number, enum, multi_enum, date, people

#### Dependencies (`org-asana-dependencies`)
- Task dependency management
- Maps to Org BLOCKER property
- Visual dependency graph
- Enforces 30-item combined limit

#### Subtasks (`org-asana-subtasks`)
- Recursive subtask synchronization
- Supports up to 5 levels of nesting
- Maps to Org heading hierarchy
- Automatic parent-child relationship management

#### Webhooks (`org-asana-webhooks`)
- Real-time updates via webhooks
- HMAC-SHA256 signature validation
- Local development server support
- Event filtering and processing

#### Attachments (`org-asana-attachments`)
- File upload/download support
- 100MB file size limit
- Multipart/form-data uploads
- Automatic download options

#### User Management (`org-asana-users`)
- Assignee management with caching
- Email/name search
- Batch assignment operations
- Tag-based auto-assignment

#### Task Types (`org-asana-types`)
- Milestone, approval, and separator indicators
- Custom faces for different types
- Approval workflow management
- Interactive type operations

#### Task Browser (`org-asana-browser`)
- Interactive task browsing without full sync
- Tabulated list interface
- Search and filter capabilities
- Direct task editing

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/org-asana.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/org-asana")
(require 'org-asana)

;; Configure your Asana token
(setq org-asana-token "your-asana-personal-access-token")

;; Set your org file location
(setq org-asana-org-file "~/org/asana.org")

;; Enable desired modules
(setq org-asana-modules
      '(org-asana-cache
        org-asana-custom-fields
        org-asana-dependencies
        org-asana-subtasks
        org-asana-attachments
        org-asana-users
        org-asana-types
        org-asana-browser))
```

## Usage

### Basic Commands

- `M-x org-asana-sync` - Perform full bidirectional sync
- `M-x org-asana-test-connection` - Test your Asana connection
- `C-c a c` - Add a comment to the current task

### Module-Specific Commands

#### Task Browser
- `M-x org-asana-browse` - Open interactive task browser
- `M-x org-asana-browse-project` - Browse specific project
- `M-x org-asana-browse-my-tasks` - Browse your assigned tasks

#### Attachments
- `M-x org-asana-attach` - Attach file to current task
- `M-x org-asana-download-attachment` - Download attachment at point
- `M-x org-asana-download-all-attachments` - Download all task attachments

#### User Management
- `M-x org-asana-assign` - Assign task to a user
- `M-x org-asana-assign-to-me` - Assign task to yourself
- `M-x org-asana-batch-assign` - Assign multiple tasks

#### Dependencies
- `M-x org-asana-add-dependency` - Add task dependency
- `M-x org-asana-remove-dependency` - Remove dependency
- `M-x org-asana-show-dependencies` - View dependency graph

#### Task Types
- `M-x org-asana-approve-task` - Approve an approval task
- `M-x org-asana-reject-task` - Reject an approval task
- `M-x org-asana-convert-to-milestone` - Convert task to milestone

#### Cache Management
- `M-x org-asana-cache-status` - View cache statistics
- `M-x org-asana-clear-cache` - Clear cache
- `M-x org-asana-save-caches` - Save persistent caches

### Org-mode Integration

Tasks are synchronized with the following properties:
- `:ASANA-TASK-GID:` - Unique task identifier
- `:ASANA-PROJECT:` - Project name
- `:ASANA-SECTION:` - Section name
- `:ASANA-TAGS:` - Task tags
- `:ASANA-ASSIGNEE:` - Assigned user
- `:ASANA-DUE-DATE:` - Due date
- `:ASANA-MODIFIED-AT:` - Last modification time
- `:ASANA-PERMALINK:` - Direct link to task in Asana

Custom fields (if enabled) are mapped as:
- `:ASANA-CF-FieldName:` - Custom field values

### Capture Templates

Add to your capture templates:
```elisp
("a" "Asana Task" entry (file org-asana-org-file)
 "* TODO %?\n:PROPERTIES:\n:ASANA-TASK-GID: new\n:END:\n"
 :empty-lines 1)
```

### Agenda Integration

The package automatically adds custom agenda commands:
- `C-c a A a` - All Asana tasks
- `C-c a A t` - Today's Asana tasks
- `C-c a A w` - This week's Asana tasks
- `C-c a A d` - Tasks with deadlines

## Configuration

### Core Settings

```elisp
;; Enable automatic sync on save
(setq org-asana-enable-save-hook t)

;; Enable sync on TODO state changes
(setq org-asana-enable-todo-hook t)

;; Show progress indicators
(setq org-asana-show-progress-indicators t)

;; Apply visual task faces
(setq org-asana-apply-faces t)
```

### Module-Specific Settings

```elisp
;; Cache settings
(setq org-asana-enable-cache t)
(setq org-asana-cache-directory "~/.emacs.d/org-asana-cache/")

;; Attachment settings
(setq org-asana-attachment-directory "~/org/asana-attachments/")
(setq org-asana-auto-download-attachments t)

;; Task type indicators
(setq org-asana-show-type-indicators t)
(setq org-asana-milestone-prefix "[MILESTONE] ")
(setq org-asana-approval-prefix "[APPROVAL] ")

;; Browser settings
(setq org-asana-browser-page-size 50)
(setq org-asana-browser-show-completed t)
```

## Webhook Setup (Advanced)

For real-time updates:

1. Set up a public HTTPS endpoint
2. Configure the webhook endpoint:
```elisp
(setq org-asana-webhook-endpoint "https://your-domain.com/asana-webhook")
```
3. Register webhooks:
```elisp
(org-asana-register-webhook "project-gid")
```

## Troubleshooting

### Common Issues

1. **Authentication Failed**: Ensure your personal access token is valid
2. **Rate Limits**: The package handles rate limits automatically
3. **Sync Conflicts**: Local changes take precedence during sync

### Debug Mode

Enable debug messages:
```elisp
(setq org-asana-debug t)
```

## License

Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

## Contributing

Contributions are welcome! Please ensure all code follows:
- GNU Emacs Lisp coding standards
- Functional programming principles
- One function, one task philosophy
- Comprehensive docstrings