# org-asana.el

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/org-asana-badge.svg)](https://melpa.org/#/org-asana)

Two-way synchronization between Emacs Org-mode and Asana tasks.

## Features

- **Bidirectional sync** between Org-mode entries and Asana tasks
- **Manual or periodic synchronization** (user configurable)
- **Intelligent conflict resolution** using newest-wins strategy with fallback
- **Comprehensive data mapping** (deadlines, priorities, tags, assignees, projects)
- **Org-agenda integration** with visual indicators and keybindings
- **Personal Access Token authentication**
- **Rate limiting and error handling**
- **Pure functional design** following GNU coding standards

## Installation

### Via MELPA (Recommended)

```elisp
(use-package org-asana
  :ensure t
  :after org
  :config
  (setq org-asana-token "your-personal-access-token")
  (org-asana-setup))
```

### Manual Installation

1. Download `org-asana.el` to your Emacs load path
2. Add to your init file:

```elisp
(require 'org-asana)
(setq org-asana-token "your-personal-access-token")
(org-asana-setup)
```

## Quick Start

1. **Get your Asana Personal Access Token**:
   - Go to [Asana Apps](https://app.asana.com/0/my-apps)
   - Click "Create New Personal Access Token"
   - Copy the token

2. **Configure org-asana**:
   ```elisp
   (setq org-asana-token "your-token-here")
   M-x org-asana-setup
   ```

3. **Start syncing**:
   ```elisp
   M-x org-asana-sync
   ```

## Configuration

### Complete use-package Example

```elisp
(use-package org-asana
  :ensure t
  :after org
  :config
  ;; Required: Your Asana Personal Access Token
  (setq org-asana-token "your-personal-access-token"
        
        ;; Sync method: 'manual or 'periodic (default: 'manual)
        org-asana-sync-method 'periodic
        
        ;; Sync interval in minutes for periodic sync (default: 15)
        org-asana-sync-interval 10
        
        ;; Conflict resolution: 'newest-wins or 'asana-wins (default: 'newest-wins)
        org-asana-conflict-resolution 'newest-wins
        
        ;; Default workspace GID (default: nil - auto-select first)
        org-asana-default-workspace nil
        
        ;; Default project GID for new tasks (default: nil - My Tasks only)
        org-asana-default-project nil
        
        ;; Whether to sync org tags with Asana tags (default: t)
        org-asana-sync-tags t
        
        ;; Whether to sync org priority with Asana priority (default: t)
        org-asana-sync-priority t
        
        ;; Specific org file for Asana tasks (default: nil - current buffer)
        org-asana-org-file "~/org/asana.org"
        
        ;; Org heading level for Asana tasks (default: 2)
        org-asana-heading-level 2)
  
  ;; Optional: Enable org-asana-mode in org buffers
  :hook (org-mode . org-asana-mode)
  
  ;; Optional: Enable agenda integration
  :hook (org-agenda-mode . org-asana-agenda-mode)
  
  ;; Keybindings (automatically set when org-asana-mode is enabled)
  :bind (:map org-mode-map
         ("C-c a s" . org-asana-sync)
         ("C-c a c" . org-asana-create-task-from-heading)
         ("C-c a i" . org-asana-import-my-tasks)
         ("C-c a o" . org-asana-open-in-asana)
         ("C-c a u" . org-asana-update-from-heading)
         ("C-c a d" . org-asana-delete-task)
         ("C-c a t" . org-asana-test-connection)))
```

### Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `org-asana-token` | `nil` | **Required**: Your Asana Personal Access Token |
| `org-asana-sync-method` | `'manual` | Sync method: `'manual` or `'periodic` |
| `org-asana-sync-interval` | `15` | Minutes between periodic syncs |
| `org-asana-conflict-resolution` | `'newest-wins` | Strategy: `'newest-wins` or `'asana-wins` |
| `org-asana-default-workspace` | `nil` | Default workspace GID (auto-select if nil) |
| `org-asana-default-project` | `nil` | Default project GID (My Tasks if nil) |
| `org-asana-sync-tags` | `t` | Sync org tags with Asana tags |
| `org-asana-sync-priority` | `t` | Sync org priority with Asana priority |
| `org-asana-org-file` | `nil` | Specific org file for tasks (current buffer if nil) |
| `org-asana-heading-level` | `2` | Heading level for imported tasks |

## Usage

### Interactive Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| `org-asana-sync` | `C-c a s` | Bidirectional synchronization |
| `org-asana-create-task-from-heading` | `C-c a c` | Create Asana task from org heading |
| `org-asana-import-my-tasks` | `C-c a i` | Import all "My Tasks" from Asana |
| `org-asana-open-in-asana` | `C-c a o` | Open task in Asana web interface |
| `org-asana-update-from-heading` | `C-c a u` | Update Asana task from org entry |
| `org-asana-delete-task` | `C-c a d` | Delete Asana task |
| `org-asana-test-connection` | `C-c a t` | Test API connection |
| `org-asana-setup` | - | Interactive setup wizard |

### Sync Methods

#### Manual Sync
```elisp
(setq org-asana-sync-method 'manual)
;; Sync when you want: M-x org-asana-sync
```

#### Periodic Sync
```elisp
(setq org-asana-sync-method 'periodic
      org-asana-sync-interval 15) ; Every 15 minutes
```

### Data Mapping

| Org Property | Asana Field | Description |
|--------------|-------------|-------------|
| Heading | `name` | Task title |
| `TODO`/`DONE` | `completed` | Completion status |
| `DEADLINE` | `due_date` | Due date |
| `[#A]`/`[#B]`/`[#C]` | `priority` | Priority level |
| Tags | `tags` | Task tags (if enabled) |
| Entry body | `notes` | Task description |
| `:ASANA_TASK_ID:` | `gid` | Unique task identifier |
| `:ASANA_MODIFIED:` | `modified_at` | Last modification time |
| `:ASANA_ASSIGNEE:` | `assignee.gid` | Assigned user |
| `:ASANA_PROJECTS:` | `projects` | Associated projects |

### Example Org Entry

```org
** TODO [#A] Complete project documentation :work:urgent:
DEADLINE: <2024-07-20 Sat>
:PROPERTIES:
:ASANA_TASK_ID: 1234567890123456
:ASANA_MODIFIED: 2024-07-15T10:30:00.000Z
:ASANA_ASSIGNEE: 9876543210987654
:ASANA_PROJECTS: 1111222233334444,5555666677778888
:END:
Write comprehensive documentation for the new feature including API references and examples.
```

## Org-Agenda Integration

Enable `org-asana-agenda-mode` for enhanced agenda functionality:

```elisp
(add-hook 'org-agenda-mode-hook #'org-asana-agenda-mode)
```

### Agenda Features
- Visual `[Asana]` indicators for synced tasks
- Direct sync from agenda view (`C-c a s`)
- Create tasks from agenda items (`C-c a c`)
- Open tasks in Asana (`C-c a o`)

## Conflict Resolution

When the same task is modified in both Org-mode and Asana:

### Newest Wins (Default)
```elisp
(setq org-asana-conflict-resolution 'newest-wins)
```
Compares modification timestamps and keeps the most recent changes.

### Asana Wins
```elisp
(setq org-asana-conflict-resolution 'asana-wins)
```
Always prefers Asana data over Org-mode data.

## Advanced Configuration

### Workspace Selection
```elisp
M-x org-asana-configure-workspace
```

### Reset Configuration
```elisp
M-x org-asana-reset-configuration
```

### Save Configuration to Init File
The setup wizard can automatically save your configuration:
```elisp
M-x org-asana-setup
;; Answer "yes" to "Save configuration to init file?"
```

## Troubleshooting

### Authentication Issues
1. Verify your token at [Asana Apps](https://app.asana.com/0/my-apps)
2. Test connection: `M-x org-asana-test-connection`
3. Check token permissions in Asana

### Sync Issues
1. Check network connectivity
2. Verify workspace/project access in Asana
3. Review conflict resolution settings
4. Check Emacs `*Messages*` buffer for errors

### Rate Limiting
org-asana respects Asana's rate limits (150 requests/minute). If you hit limits:
- Reduce sync frequency
- Sync smaller batches
- Wait for rate limit reset

## Requirements

- **Emacs 30.1+** (for improved JSON parsing)
- **Org 9.4+** (for modern org-agenda features)
- **Internet connection** for Asana API access
- **Asana Personal Access Token**

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes following GNU coding standards
4. Test your changes
5. Submit a pull request

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Acknowledgments

- Built following GNU Emacs coding standards
- Inspired by the Org-mode and Asana communities
- Follows functional programming principles

## Support

- **Issues**: [GitHub Issues](https://github.com/theesfeld/asana-emacs/issues)
- **Documentation**: This README and inline documentation
- **Community**: Emacs and Org-mode communities

---

*Made with ❤️ for the Emacs community*