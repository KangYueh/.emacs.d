# Zettelkasten Quick Reference Card

## Essential Keybindings

### Capture & Create
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n c`   | Capture new note (choose template)        |
| `C-c n f`   | Find or create note                       |
| `C-c n j`   | Daily note entry                          |
| `C-c c`     | Org capture menu (GTD)                    |

### Navigate & Search
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n f`   | Find note by title                        |
| `C-c n t`   | Filter notes by tag                       |
| `C-c n S`   | Full-text search (ripgrep)                |
| `C-c a`     | Open agenda                               |

### Link & Connect
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n i`   | Insert link to note                       |
| `C-c n I`   | Insert link (immediate, no open)          |
| `C-c n l`   | Toggle backlinks sidebar                  |
| `C-c n r`   | Extract subtree to new note               |

### Process & Review
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n D`   | Daily review dashboard                    |
| `C-c n P`   | Process fleeting notes                    |
| `C-c n p`   | Convert fleeting → permanent              |
| `C-c w p`   | Process inbox                             |
| `C-c w r`   | Weekly review                             |

### Quality Control
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n S`   | Show statistics                           |
| `C-c n O`   | Find orphan notes                         |
| `C-c n U`   | Find unlinked notes                       |
| `C-c n R`   | Review old notes (>30 days)               |
| `C-c n g`   | Open graph visualization                  |

### Export & Share
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c e h`   | Export to HTML and open                   |
| `C-c e m`   | Export to Markdown                        |
| `C-c e b`   | Export with backlinks                     |
| `C-c e c`   | Export note cluster                       |

### AI Features (Optional)
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c A s`   | Summarize region                          |
| `C-c A l`   | Suggest links                             |
| `C-c A e`   | Expand fleeting note                      |
| `C-c A q`   | Generate questions                        |
| `C-c A c`   | Critique note                             |
| `C-c A t`   | Suggest tags                              |

### Literature & Citations
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n k`   | Insert citation                           |
| `C-c n L`   | Open literature note                      |
| `C-c n b`   | Attach citekey to node                    |

### Utilities
| Key         | Action                                    |
|-------------|-------------------------------------------|
| `C-c n s`   | Take screenshot                           |
| `C-c n a`   | Add alias to note                         |
| `C-c w l`   | Convert heading to roam node              |

## Note Type Templates

### When to Use Each Type

| Type        | When to Use                               | Template Key |
|-------------|-------------------------------------------|--------------|
| Fleeting    | Quick capture, unprocessed idea           | `f`          |
| Permanent   | Refined, atomic concept                   | `p`          |
| Concept     | Definition with properties                | `c`          |
| Literature  | From papers, books, articles              | `l`          |
| Method      | Technique, process, how-to                | `m`          |
| Question    | Research question, open inquiry           | `q`          |
| Daily       | Journal, daily thoughts                   | (C-c n j)    |

## Daily Workflow (10 minutes)

### Morning (2 min)
```
1. C-c n j          → Open today's daily note
2. Write intentions
3. Review yesterday's fleeting notes
```

### During Day (ongoing)
```
1. Quick idea?      → C-c n c f (fleeting)
2. Reading paper?   → C-c c r (capture to inbox)
3. Found connection?→ C-c n i (link notes)
```

### Evening (8 min)
```
1. C-c n D          → Start daily review
2. C-c n P          → Process fleeting notes
3. For each fleeting:
   - Refine idea
   - Add links (C-c n i)
   - C-c n p (convert to permanent)
4. C-c n O          → Check orphans
5. Link or delete orphans
```

## Weekly Workflow (30 minutes)

### Review (15 min)
```
1. C-c w r          → Weekly review agenda
2. C-c n S          → Check statistics
3. C-c n R          → Review old notes
4. Update stale notes
```

### Maintain (15 min)
```
1. C-c n g          → View graph
2. Identify clusters
3. Create hub notes for major topics
4. C-c n U          → Find unlinked notes
5. Add connections
```

## Agenda Views

| Key       | View                                      |
|-----------|-------------------------------------------|
| `C-c a d` | Dashboard (today + next actions)          |
| `C-c a w` | Weekly review                             |
| `C-c a r` | Reading & research tasks                  |
| `C-c a p` | Projects overview                         |

## Capture Templates (C-c c)

| Key | Template                                  |
|-----|-------------------------------------------|
| `t` | TODO task                                 |
| `n` | Quick note                                |
| `p` | Project                                   |
| `m` | Meeting notes                             |
| `j` | Journal entry                             |
| `r` | Reading/research item                     |

## Common Patterns

### Creating a New Permanent Note
```
1. C-c n c p        → New permanent note
2. Write core idea
3. Add explanation
4. Add examples
5. C-c n i (3-5x)   → Link to related notes
6. Add tags
```

### Processing a Fleeting Note
```
1. C-c n P          → Open fleeting note
2. Read and refine
3. Is it worth keeping?
   Yes: C-c n p (convert to permanent)
   No:  Delete it
4. Add links (C-c n i)
5. Move to next fleeting
```

### Creating Literature Note
```
Method 1 (from citation):
1. C-c n k          → Insert citation
2. C-c n L          → Create literature note

Method 2 (manual):
1. C-c n c l        → New literature note
2. Fill in: Summary, Key Points, Quotes
3. Create permanent notes for key ideas
4. Link permanent notes to literature note
```

### Finding Related Notes
```
1. C-c n f          → Find note
2. C-c n l          → View backlinks
3. Click links to explore
4. C-c n g          → View in graph
```

### Exporting for Sharing
```
Single note:
1. Open note
2. C-c e h          → Export to HTML

With context:
1. Open note
2. C-c e c          → Export cluster (note + linked notes)
```

## Quality Checklist

### Good Permanent Note Has:
- ✅ One clear idea (atomic)
- ✅ Self-contained (understandable alone)
- ✅ 3-5 links to related notes
- ✅ Examples or evidence
- ✅ Written in your own words
- ✅ Meaningful tags (2-4)

### Warning Signs:
- ⚠️ No backlinks (orphan)
- ⚠️ No outgoing links (isolated)
- ⚠️ Too long (>500 words)
- ⚠️ Multiple ideas (not atomic)
- ⚠️ Unclear title
- ⚠️ Just copied text (not processed)

## Troubleshooting

### Can't Find a Note
```
1. C-c n S          → Full-text search
2. C-c n t          → Filter by tag
3. C-c a            → Check agenda (if has TODO)
```

### Too Many Orphans
```
1. C-c n O          → List orphans
2. For each:
   - Add 2-3 links, OR
   - Delete if not useful
3. Create hub notes for clusters
```

### Fleeting Notes Piling Up
```
1. Set daily reminder
2. C-c n D every evening
3. Process immediately (don't batch >10)
4. Delete freely (not everything is worth keeping)
```

### Graph Too Messy
```
1. Create hub/index notes
2. Link related notes to hubs
3. Use tags for filtering
4. Archive old/irrelevant notes
```

## Tips for Success

### Linking
- Link while writing (not later)
- Add context: "This relates to X because..."
- Aim for 3-5 links per note
- Link to both similar and contrasting ideas

### Tags
- Use sparingly (3-4 max per note)
- Prefer links over tags
- Use for: domains (ml, python), types (method, concept)
- Avoid: redundant tags (if already in title)

### Writing
- Write for future you (6 months from now)
- Use simple language
- One idea per note
- Include examples
- Cite sources

### Processing
- Daily: 5-10 minutes
- Weekly: 30 minutes
- Don't let fleeting notes age >3 days
- Delete freely (quality > quantity)

### Reviewing
- Check statistics weekly
- Review old notes monthly
- Update outdated information
- Strengthen weak connections

## Keyboard Shortcuts Summary

### Most Used (Learn These First)
```
C-c n c     Capture
C-c n f     Find
C-c n i     Insert link
C-c n l     Backlinks
C-c n D     Daily review
C-c n p     Convert to permanent
```

### Power User
```
C-c n S     Stats
C-c n O     Orphans
C-c n g     Graph
C-c n P     Process fleeting
C-c e h     Export HTML
```

### Advanced
```
C-c n C     Backlink context
C-c n E     Export with links
C-c n R     Review old
C-c n U     Unlinked
```

## File Locations

```
~/wdata/note/
├── roam/
│   ├── daily/          # Daily notes
│   ├── fleeting/       # Unprocessed
│   ├── permanent/      # Refined notes
│   ├── concepts/       # Definitions
│   ├── methods/        # How-tos
│   ├── questions/      # Research questions
│   ├── references/     # Literature notes
│   └── index.org       # Hub file
├── org/
│   ├── inbox.org       # GTD inbox
│   ├── gtd.org         # Next actions
│   ├── projects.org    # Projects
│   ├── someday.org     # Someday/maybe
│   └── archive.org     # Completed
├── exports/            # Exported files
└── img/                # Screenshots
```

## Getting Help

### In Emacs
```
C-h k <key>             What does this key do?
C-h f <function>        Function documentation
C-h v <variable>        Variable value
C-h m                   Current mode help
```

### Documentation
```
~/.emacs.d/ZETTELKASTEN_GUIDE.md    Full guide
~/.emacs.d/IMPROVEMENTS.md          What's new
~/.emacs.d/INSTALLATION.md          Setup guide
```

### Debugging
```
M-x org-roam-db-sync    Rebuild database
C-h e                   View messages
*Warnings* buffer       Check warnings
```

## Customization

### Change Paths
```elisp
;; In init.el before (require 'init-roam)
(setq org-roam-directory "~/your/path")
```

### Add Note Type
```elisp
;; In init-roam.el, add to org-roam-capture-templates
("x" "your-type" plain
 "Your template"
 :if-new (file+head "your-dir/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: your-tag\n")
 :unnarrowed t)
```

### Change Keybinding
```elisp
;; In init-roam.el or your init.el
(define-key org-roam-mode-map (kbd "C-c z") #'org-roam-node-find)
```

---

**Print this page and keep it handy while learning the system!**
