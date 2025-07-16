# org-asana

Simple bidirectional sync between Emacs Org-mode and Asana.

## Features

- **Bidirectional sync**: Push org changes to Asana, pull Asana tasks to org
- **Hierarchical structure**: Projects → Sections → Tasks with TODO statistics
- **DONE task handling**: Completed tasks move to a COMPLETED section
- **Works from anywhere**: Syncs to a designated file from any buffer
- **Minimal configuration**: Just set your token and file path

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
        org-asana-sync-priority t))                 ; sync priorities between org and Asana
```

## Configuration

Get your Personal Access Token from Asana:
1. Go to https://app.asana.com
2. Click your profile photo → Settings → Apps
3. Navigate to "Developer apps"
4. Click "+ Create new token"
5. Name it (e.g., "Emacs Org-mode Sync")
6. Copy the token

Then configure org-asana:

```elisp
(setq org-asana-token "YOUR_TOKEN_HERE"
      org-asana-org-file "~/org/asana.org"
      org-asana-conflict-resolution 'newest-wins  ; How to resolve conflicts
      org-asana-sync-tags t                       ; Sync tags
      org-asana-sync-priority t)                  ; Sync priorities
```

### Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `org-asana-token` | `nil` | **Required**: Your Asana Personal Access Token |
| `org-asana-org-file` | `nil` | **Required**: Org file path for sync |
| `org-asana-conflict-resolution` | `'newest-wins` | Conflict resolution: `'newest-wins`, `'asana-wins`, or `'local-wins` |
| `org-asana-sync-tags` | `t` | Whether to sync org tags with Asana tags |
| `org-asana-sync-priority` | `t` | Whether to sync org priority with Asana priority |

## Usage

Only two commands:

- `M-x org-asana-test-connection` - Test your API connection
- `M-x org-asana-sync` - Sync tasks between Org and Asana

## File Structure

After syncing, your org file will look like:

```org
* Active Projects
** Project Name [2/5]
*** Section Name [1/3]
**** TODO [#A] Task 1 :work:urgent:
     DEADLINE: <2024-07-20>
     :PROPERTIES:
     :ASANA_TASK_ID: 123456789
     :ASANA_MODIFIED: 2024-07-15T10:00:00.000Z
     :END:
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
     :ASANA_TASK_ID: 987654321
     :ASANA_COMPLETED_AT: 2024-07-14T15:00:00.000Z
     :END:
```

## How It Works

1. **Sync from Asana**: Fetches all incomplete tasks assigned to you, organizing them by project and section
2. **Sync to Asana**: Updates task names, notes, completion status, due dates, priorities, and tags
3. **DONE tasks**: When you mark a task as DONE in org, it syncs to Asana and moves to the COMPLETED section
4. **No duplicates**: Completed tasks in the COMPLETED section are never re-synced
5. **Conflict resolution**: When both org and Asana have changes, uses your configured strategy

## Conflict Resolution

When the same task is modified in both org and Asana, the system resolves conflicts based on `org-asana-conflict-resolution`:

- **`'newest-wins`** (default): Compares modification timestamps, keeps the most recent changes
- **`'asana-wins`**: Always prefers Asana data over org data
- **`'local-wins`**: Always prefers org data over Asana data

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

### Priority Mapping

| Org Priority | Asana Priority |
|--------------|----------------|
| [#A] | high |
| [#B] | medium |
| [#C] | low |
| (none) | (none) |

## Limitations

- Only syncs tasks assigned to you
- No support for creating new tasks (use Asana web/app for that)
- One-way sync for completed tasks (org → Asana only)
- Tags are synced but not created in Asana (must exist in your workspace)
- Custom fields and attachments are not synced

## License

GNU GPL v3+