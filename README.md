# org-asana

Advanced bidirectional sync between Emacs Org-mode and Asana with enhanced features.

## Features

### Core Functionality
- **Bidirectional sync**: Push org changes to Asana, pull Asana tasks to org
- **Hierarchical structure**: Projects → Sections → Tasks with TODO statistics
- **DONE task handling**: Completed tasks move to a COMPLETED section
- **Works from anywhere**: Syncs to a designated file from any buffer
- **Minimal configuration**: Just set your token and file path

### New Advanced Features
- **Rate Limiting Protection**: Automatic retry with exponential backoff
- **Pagination Support**: Handle workspaces with 100+ tasks
- **Visual Task Faces**: Custom highlighting for priorities and due dates
- **Org Agenda Integration**: Custom agenda views for Asana tasks
- **Progress Indicators**: [x/y] indicators for projects and sections
- **Capture Templates**: Create new Asana tasks using org-capture

## Installation

### Manual Installation

1. Clone this repository
2. Add to your init file:

```elisp
(add-to-list 'load-path "/path/to/org-asana")
(require 'org-asana)
```

### Using use-package

```elisp
(use-package org-asana
  :load-path "/path/to/org-asana"
  :config
  (setq org-asana-token "YOUR_PERSONAL_ACCESS_TOKEN"
        org-asana-org-file "~/org/asana.org"
        org-asana-conflict-resolution 'newest-wins  ; or 'asana-wins, 'local-wins
        org-asana-sync-tags t                       ; sync tags between org and Asana
        org-asana-sync-priority t                   ; sync priorities between org and Asana
        org-asana-show-progress-indicators t        ; show [x/y] progress
        org-asana-agenda-skip-completed t)          ; skip completed in agenda
  ;; Enable optional features
  (org-asana-enable-agenda-integration)
  (org-asana-enable-capture-templates))
```

## Configuration

### Getting Your Asana Token

1. Go to https://app.asana.com
2. Click your profile photo → Settings → Apps
3. Navigate to "Developer apps"
4. Click "+ Create new token"
5. Name it (e.g., "Emacs Org-mode Sync")
6. Copy the token

### Basic Configuration

```elisp
(setq org-asana-token "YOUR_TOKEN_HERE"
      org-asana-org-file "~/org/asana.org")
```

### Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `org-asana-token` | `nil` | **Required**: Your Asana Personal Access Token |
| `org-asana-org-file` | `nil` | **Required**: Org file path for sync |
| `org-asana-conflict-resolution` | `'newest-wins` | Conflict resolution: `'newest-wins`, `'asana-wins`, or `'local-wins` |
| `org-asana-sync-tags` | `t` | Whether to sync org tags with Asana tags |
| `org-asana-sync-priority` | `t` | Whether to sync org priority with Asana priority |
| `org-asana-show-progress-indicators` | `t` | Show [x/y] progress indicators for projects/sections |
| `org-asana-agenda-skip-completed` | `t` | Skip completed tasks in agenda views |
| `org-asana-default-project` | `nil` | Default project GID for new tasks via capture |

## Usage

### Basic Commands

- `M-x org-asana-test-connection` - Test your API connection
- `M-x org-asana-sync` - Sync tasks between Org and Asana

### Agenda Integration

After enabling agenda integration, use these custom agenda commands:

- `C-c a A a` - View all Asana tasks
- `C-c a A d` - View Asana tasks due today
- `C-c a A w` - View Asana tasks due this week
- `C-c a A p` - View Asana tasks sorted by priority

### Capture Templates

After enabling capture templates:

- `C-c c a` - Create a basic Asana task
- `C-c c A` - Create an Asana task with notes

Tasks created via capture are synced to Asana on the next `org-asana-sync`.

## File Structure

After syncing, your org file will look like:

```org
* Active Projects
** Project Name [2/5]
*** Section Name [1/3]
**** TODO [#A] Task 1 :work:urgent:
     DEADLINE: <2024-07-20>
     :PROPERTIES:
     :asana-id: 123456789
     :asana-modified: 2024-07-15T10:00:00.000Z
     :END:
     
     Task notes and description here...
     
**** DONE [#B] Task 2 :project:review:
**** TODO Task 3
*** Another Section [1/2]
**** TODO [#C] Task 4 :backend:
**** DONE Task 5

* COMPLETED
** Project Name
*** Section Name  
**** DONE [#A] Completed task :work:urgent:
     :PROPERTIES:
     :asana-id: 987654321
     :asana-completed-at: 2024-07-14T15:00:00.000Z
     :END:
```

## Visual Features

### Custom Faces

Tasks are highlighted based on their status:

- **Overdue tasks**: Bold red face
- **Due today**: Bold green face  
- **Due soon (within 7 days)**: Orange italic face
- **High priority [#A]**: Bold red face
- **Medium priority [#B]**: Bold yellow face
- **Low priority [#C]**: Bold blue face

### Progress Indicators

Projects and sections show completion progress:

```org
** Project Name [5/12]
*** Section Name [2/4]
```

## How It Works

1. **Sync from Asana**: Fetches all incomplete tasks assigned to you, organizing them by project and section
2. **Sync to Asana**: Updates task names, notes, completion status, due dates, priorities, and tags
3. **DONE tasks**: When you mark a task as DONE in org, it syncs to Asana and moves to the COMPLETED section
4. **New tasks**: Tasks created via capture templates are created in Asana during sync
5. **No duplicates**: Completed tasks in the COMPLETED section are never re-synced
6. **Conflict resolution**: When both org and Asana have changes, uses your configured strategy

## Advanced Features

### Rate Limiting

The package automatically handles Asana's rate limits:
- Tracks remaining API calls (150/minute limit)
- Implements exponential backoff on rate limit errors
- Retries failed requests automatically

### Pagination

Automatically fetches all tasks when you have more than 100:
- Follows pagination links transparently
- No configuration needed

### Org Agenda Integration

When enabled, adds your Asana file to `org-agenda-files` and provides:
- Custom agenda views for Asana tasks
- Filtering by due date and priority
- Skip completed tasks option

### Capture Templates

Create new Asana tasks directly from Emacs:
- Select project interactively or use default
- Set deadline and notes
- Tasks created on next sync

## Data Synchronization

### Supported Fields

| Org | Asana | Notes |
|-----|-------|-------|
| Heading | Task name | Bidirectional sync |
| TODO/DONE | Completed status | Bidirectional sync |
| DEADLINE | Due date | Bidirectional sync |
| [#A]/[#B]/[#C] | Priority (high/medium/low) | Optional, bidirectional |
| :tags: | Tags | Optional, bidirectional |
| Body text | Notes | Bidirectional sync |
| :asana-id: | Task GID | Auto-managed |
| :asana-modified: | Last modified | Auto-managed |

### Priority Mapping

| Org Priority | Asana Priority |
|--------------|----------------|
| [#A] | high |
| [#B] | medium |
| [#C] | low |
| (none) | (none) |

## Conflict Resolution

When the same task is modified in both org and Asana:

- **`'newest-wins`** (default): Compares timestamps, keeps most recent
- **`'asana-wins`**: Always prefers Asana data
- **`'local-wins`**: Always prefers org data

## Performance

- Efficient API usage with pagination
- Rate limit protection prevents API errors
- Progress indicators update only changed sections
- Visual faces applied after sync completion

## Limitations

- Only syncs tasks assigned to you
- Tags must exist in Asana workspace before syncing
- Custom fields and attachments are not synced
- Subtasks are not currently supported
- Comments are not synced

## Troubleshooting

### Connection Issues
```elisp
;; Test your connection
M-x org-asana-test-connection
```

### Rate Limit Errors
The package handles these automatically, but you can check:
```elisp
org-asana--rate-limit-remaining  ; Check remaining calls
org-asana--rate-limit-reset      ; Check reset time
```

### Agenda Not Showing Tasks
```elisp
;; Re-enable agenda integration
M-x org-asana-enable-agenda-integration
```

## Contributing

Contributions welcome! Please:
1. Follow GNU Elisp coding standards
2. Use small, single-purpose functions
3. Add docstrings to all functions
4. Test with large task lists

## License

Copyright (C) 2024 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

See LICENSE file for details.