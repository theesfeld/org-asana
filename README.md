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
        org-asana-org-file "~/org/asana.org"))
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
      org-asana-org-file "~/org/asana.org")
```

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
**** TODO Task 1
     DEADLINE: <2024-07-20>
     :PROPERTIES:
     :ASANA_TASK_ID: 123456789
     :ASANA_MODIFIED: 2024-07-15T10:00:00.000Z
     :END:
**** DONE Task 2
**** TODO Task 3
*** Another Section [1/2]
**** TODO Task 4
**** DONE Task 5

* COMPLETED
** Project Name
*** Section Name  
**** DONE Completed task
     :PROPERTIES:
     :ASANA_TASK_ID: 987654321
     :ASANA_COMPLETED_AT: 2024-07-14T15:00:00.000Z
     :END:
```

## How It Works

1. **Sync from Asana**: Fetches all incomplete tasks assigned to you, organizing them by project and section
2. **Sync to Asana**: Updates task names, notes, completion status, and due dates
3. **DONE tasks**: When you mark a task as DONE in org, it syncs to Asana and moves to the COMPLETED section
4. **No duplicates**: Completed tasks in the COMPLETED section are never re-synced

## Limitations

- Only syncs tasks assigned to you
- No support for creating new tasks (use Asana web/app for that)
- Tags and priorities are not synced
- One-way sync for completed tasks (org → Asana only)

## License

GNU GPL v3+