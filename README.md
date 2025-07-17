# org-asana

Advanced two-way synchronization between Emacs Org-mode and Asana tasks.

## Features

- **Two-way sync**: Sync tasks between Org-mode and Asana
- **Hierarchical structure**: Projects → Sections → Tasks with proper nesting
- **Smart completion handling**: Completed tasks preserved in COMPLETED section
- **Visual feedback**: Custom faces for priorities and deadlines
- **Progress indicators**: Track completion status [x/y] for projects/sections
- **Org-agenda integration**: Custom agenda views for Asana tasks
- **Auto-sync hooks**: Sync on save and TODO state changes
- **Rate limit protection**: Automatic retry with exponential backoff
- **Metadata support**: Sync comments, attachments, and activity history
- **Error handling**: Graceful degradation with specific error types

## Installation

### Using use-package with GitHub

```elisp
(use-package org-asana
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/org-asana"
  :init
  ;; Clone the repository first
  (unless (file-exists-p "~/.emacs.d/site-lisp/org-asana")
    (make-directory "~/.emacs.d/site-lisp" t)
    (shell-command 
     "git clone https://github.com/theesfeld/org-asana.git ~/.emacs.d/site-lisp/org-asana"))
  :config
  (setq org-asana-token "YOUR_PERSONAL_ACCESS_TOKEN"
        org-asana-org-file "~/org/asana.org"))
```

### Using straight.el

```elisp
(use-package org-asana
  :straight (:host github :repo "theesfeld/org-asana")
  :config
  ;; Use authinfo for secure token storage (recommended)
  (setq org-asana-token nil  ; Will read from authinfo
        org-asana-org-file "~/org/asana.org"
        
        ;; Core sync settings
        org-asana-conflict-resolution 'newest-wins  ; or 'asana-wins, 'local-wins
        org-asana-sync-tags t                       ; sync tags between org and Asana
        org-asana-sync-priority t                   ; sync priorities between org and Asana
        
        ;; Visual enhancements
        org-asana-show-progress-indicators t        ; show [x/y] progress
        org-asana-auto-apply-faces t               ; color-code tasks by priority/due date
        org-asana-collapse-on-open t                ; collapse drawers and headings
        
        ;; Metadata sync (disable if sync is too slow)
        org-asana-fetch-metadata t                  ; fetch comments, attachments, history
        org-asana-show-activity-history t           ; show activity timeline
        
        ;; Performance tuning
        org-asana-max-retries 3                     ; set to 1 to disable retries
        org-asana-debug nil                         ; enable for troubleshooting
        
        ;; Agenda settings
        org-asana-agenda-skip-completed t)          ; skip completed in agenda
        
  ;; Enable optional features
  (org-asana-enable-agenda-integration)
  (org-asana-enable-capture-templates))
```

### Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/theesfeld/org-asana.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/org-asana")
   (require 'org-asana)
   (setq org-asana-token "YOUR_PERSONAL_ACCESS_TOKEN"
         org-asana-org-file "~/org/asana.org")
   ```

## Getting Started

### 1. Get Your Asana Token

1. Go to https://app.asana.com
2. Click your profile photo → Settings → Apps
3. Navigate to "Developer apps"
4. Click "+ Create new token"
5. Name it (e.g., "Emacs Org-mode Sync")
6. Copy the token

### 2. Basic Configuration

```elisp
;; Option 1: Direct token (less secure)
(setq org-asana-token "YOUR_TOKEN_HERE"
      org-asana-org-file "~/org/asana.org")

;; Option 2: Use authinfo (recommended)
(setq org-asana-token nil  ; Will use authinfo
      org-asana-org-file "~/org/asana.org")
```

### Secure Token Storage with Authinfo

For better security, store your token in `~/.authinfo` or `~/.authinfo.gpg`:

```
# ~/.authinfo
machine app.asana.com password YOUR_PERSONAL_ACCESS_TOKEN

# Or use a custom machine name:
machine my-asana password YOUR_PERSONAL_ACCESS_TOKEN
```

Then configure org-asana to use it:

```elisp
(setq org-asana-token nil  ; Use authinfo instead of hardcoded token
      org-asana-authinfo-machine "my-asana")  ; Optional: custom machine name
```

The package will automatically retrieve the token from authinfo when needed.

### Configuration Options

#### Required Settings

| Variable | Default | Description |
|----------|---------|-------------|
| `org-asana-token` | `nil` | Your Asana Personal Access Token (if nil, uses authinfo) |
| `org-asana-authinfo-machine` | `"app.asana.com"` | Machine name for authinfo lookup |
| `org-asana-org-file` | `"~/org/asana.org"` | Org file path for sync |

#### Core Sync Settings

```
M-x org-asana-test-connection
```

### 4. Sync Tasks

```
M-x org-asana-sync
```

## Configuration Options

All configuration variables with their defaults:

```elisp
;; Required settings (no defaults)
(setq org-asana-token nil                          ; Your Asana Personal Access Token
      org-asana-org-file "~/org/asana.org")        ; Path to Org file for sync

;; Sync behavior
(setq org-asana-fetch-metadata t                   ; Fetch comments, attachments, activity
      org-asana-show-progress-indicators t         ; Show [x/y] progress indicators
      org-asana-rate-limit-delay 0.4               ; Delay between API requests (seconds)
      org-asana-debug nil)                         ; Enable debug messages

;; Visual features
(setq org-asana-apply-faces t)                     ; Apply custom faces to tasks

;; Hooks and automation
(setq org-asana-enable-save-hook t                 ; Auto-sync on buffer save
      org-asana-enable-todo-hook t                 ; Auto-sync on TODO state change
      org-asana-enable-agenda-integration t)       ; Add to org-agenda-files
```

## Usage

### Interactive Commands

- `M-x org-asana-sync` - Perform bidirectional sync
- `M-x org-asana-test-connection` - Test API connection
- `M-x org-asana-initialize` - Interactive setup wizard
- `M-x org-asana-enable-agenda-integration` - Add to agenda files
- `M-x org-asana-disable-agenda-integration` - Remove from agenda files
- `M-x org-asana-reset` - Reset state and hooks

### Org-Agenda Integration

When agenda integration is enabled, custom commands are available:

- `C-c a A t` - Asana tasks today
- `C-c a A w` - Asana tasks this week  
- `C-c a A d` - Asana tasks with deadlines (within 7 days)

## File Structure

After syncing, your org file will have this structure:

```org
* Project Name [2/5]
  :PROPERTIES:
  :ASANA-PROJECT-GID: 123456789
  :END:
** Section Name [1/3]
   :PROPERTIES:
   :ASANA-SECTION-GID: 987654321
   :END:
*** TODO [#A] Task Title
    DEADLINE: <2024-07-20>
    :PROPERTIES:
    :ASANA-ID: 456789123
    :ASANA-MODIFIED: 2024-07-15T10:00:00.000Z
    :ASANA-ASSIGNEE: John Doe
    :ASANA-TAGS: frontend, urgent
    :ASANA-PERMALINK: https://app.asana.com/0/123456789/456789123
    :END:
    
    Task description and notes here...
    
    ** Attachments
    - [[https://example.com/file.pdf][Design Document]]
    
    ** Comments
    - John Doe (2024-07-15): This looks good!
    - Jane Smith (2024-07-16): Approved
    
*** DONE Task 2
    :PROPERTIES:
    :ASANA-ID: 789456123
    :ASANA-COMPLETED-AT: 2024-07-14T15:00:00.000Z
    :END:
```

## Visual Features

### Custom Faces

Tasks are highlighted based on their status:

| Face | Description | Default Style |
|------|-------------|---------------|
| `org-asana-priority-high` | High priority tasks [#A] | Bold red |
| `org-asana-priority-medium` | Medium priority tasks [#B] | Orange |
| `org-asana-deadline-warning` | Tasks due within 3 days | Bold dark orange |
| `org-asana-deadline-overdue` | Overdue tasks | Bold red background |

Customize faces:
```elisp
(set-face-attribute 'org-asana-priority-high nil :foreground "red" :weight 'bold)
```

### Progress Indicators

Projects and sections show completion status:
```org
* Project Name [5/12]
** Section Name [2/4]
```

Update manually with `M-x org-asana-update-progress-indicators`.

## Advanced Features

### Automatic Sync Hooks

When enabled, org-asana automatically syncs:
- On buffer save (when `org-asana-enable-save-hook` is `t`)
- On TODO state changes (when `org-asana-enable-todo-hook` is `t`)

Disable temporarily:
```elisp
(setq org-asana-enable-save-hook nil)
```

### Rate Limiting

The package handles Asana's rate limits (150 requests/minute):
- Tracks remaining API calls
- Implements exponential backoff
- Automatic retry on 429 errors
- User feedback during rate limit waits

### Error Handling

Custom error types for better debugging:
- `org-asana-error` - Base error type
- `org-asana-auth-error` - Authentication failures
- `org-asana-rate-limit-error` - Rate limit exceeded
- `org-asana-api-error` - API errors
- `org-asana-sync-error` - Sync-specific errors

## Data Mapping

| Org-mode | Asana | Sync Direction |
|----------|-------|----------------|
| Heading text | Task name | ↔️ Bidirectional |
| TODO/DONE | Completed status | ↔️ Bidirectional |
| DEADLINE | Due date | → To Asana only |
| Body text | Task notes | → To Asana only |
| [#A]/[#B]/[#C] | Priority | ← From Asana only |
| :ASANA-ID: | Task GID | ← From Asana only |
| :ASANA-MODIFIED: | Modified time | ← From Asana only |
| :ASANA-TAGS: | Tags | ← From Asana only |
| Comments | Task stories | ← From Asana only |
| Attachments | File links | ← From Asana only |

## Performance Tips

For large workspaces:

1. **Disable metadata fetching** for faster sync:
   ```elisp
   (setq org-asana-fetch-metadata nil)
   ```

2. **Increase rate limit delay** if hitting limits:
   ```elisp
   (setq org-asana-rate-limit-delay 0.6)
   ```

3. **Disable auto-sync hooks** for manual control:
   ```elisp
   (setq org-asana-enable-save-hook nil
         org-asana-enable-todo-hook nil)
   ```

## Troubleshooting

### Enable debug mode
```elisp
(setq org-asana-debug t)
```

### Check rate limit status
```elisp
;; In *scratch* buffer
org-asana--rate-limit-remaining
org-asana--rate-limit-reset
```

### Reset everything
```elisp
M-x org-asana-reset
```

### Common Issues

**"No Asana token configured"**
- Set `org-asana-token` with your Personal Access Token

**"Authentication failed"**
- Verify token is valid and not expired
- Check token has required permissions

**Rate limit errors**
- Wait for reset (shown in error message)
- Increase `org-asana-rate-limit-delay`

**Sync seems slow**
- Disable metadata fetching: `(setq org-asana-fetch-metadata nil)`
- Check network connection
- Enable debug mode to see progress

## Contributing

Contributions welcome! Please:

1. Follow GNU Emacs Lisp coding standards
2. Maintain "one function, one task" principle (max 15 lines)
3. Add docstrings to all functions
4. Include tests for new features
5. Update documentation as needed

## License

Copyright (C) 2024 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.