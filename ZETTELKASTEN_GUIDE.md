# Zettelkasten Workflow Guide

## Overview

Your Emacs configuration now includes an enhanced Zettelkasten system built on org-roam. This guide explains the workflow and keybindings.

## Directory Structure

```
~/wdata/note/roam/
├── daily/          # Daily notes and fleeting thoughts
├── fleeting/       # Quick captures to process later
├── permanent/      # Processed, atomic notes
├── concepts/       # Concept definitions
├── methods/        # Techniques and processes
├── questions/      # Open questions and research
├── references/     # Literature notes from papers/books
└── index.org       # Optional hub/index file
```

## Core Workflow

### 1. Capture (Input)

**Quick Capture:**
- `C-c n j` - Daily note entry (timestamped)
- `C-c n c` - Capture new note with template selection:
  - `f` - Fleeting note (quick idea)
  - `p` - Permanent note (processed thought)
  - `c` - Concept (definition + properties)
  - `l` - Literature note (from reading)
  - `m` - Method/technique
  - `q` - Question/research topic

**From Org Capture:**
- `C-c c` then:
  - `t` - TODO task
  - `n` - Quick note to inbox
  - `r` - Reading/research item
  - `j` - Journal entry

### 2. Process (Transform)

**Daily Processing:**
- `C-c n D` - Start daily review workflow
- `C-c n P` - Process fleeting notes
- `C-c n p` - Convert current fleeting → permanent
- `C-c w p` - Process inbox items

**Weekly Review:**
- `C-c w r` - Weekly review agenda

### 3. Link (Connect)

**Creating Links:**
- `C-c n i` - Insert link to existing/new note
- `C-c n I` - Insert link without opening note (faster)
- `C-c w l` - Convert current heading to roam node

**Viewing Links:**
- `C-c n l` - Toggle backlinks sidebar
- `C-c n C` - Show backlink context (detailed view)

### 4. Find (Navigate)

**Search & Filter:**
- `C-c n f` - Find/create any note
- `C-c n t` - Filter notes by tag
- `C-c n S` - Full-text search (ripgrep)

**Quality Control:**
- `C-c n O` - Find orphan notes (no backlinks)
- `C-c n U` - Find unlinked notes (no outgoing links)
- `C-c n R` - Review old notes (>30 days)

### 5. Analyze (Maintain)

**Statistics:**
- `C-c n S` - Show Zettelkasten statistics

**Visualization:**
- `C-c n g` - Open graph view (org-roam-ui)

**Export:**
- `C-c n E` - Export note with all linked notes

## Note Types Explained

### Fleeting Notes
- Quick captures, unprocessed thoughts
- Tagged: `fleeting`
- Should be processed within 1-2 days
- Convert to permanent when refined

### Permanent Notes
- Atomic ideas (one concept per note)
- Well-written, self-contained
- Tagged: `permanent`
- Heavily linked to other notes

### Literature Notes
- From papers, books, articles
- Tagged: `literature`
- Include source information
- Link to permanent notes for synthesis

### Concept Notes
- Definitions and explanations
- Tagged: `permanent concept`
- Include: definition, properties, examples, applications

### Daily Notes
- Journal-style entries
- Tagged: `daily`
- Can contain TODOs and fleeting thoughts
- Automatically organized by date

## Best Practices

### 1. Atomic Notes
- One idea per note
- Complete thoughts, not fragments
- Can stand alone without context

### 2. Link Liberally
- Link to related concepts
- Add context when linking (why the connection matters)
- Create bidirectional connections

### 3. Use Tags Wisely
- Keep tags minimal and meaningful
- Common tags: `fleeting`, `permanent`, `literature`, `concept`, `method`, `question`
- Add domain tags: `ml`, `python`, `research`, etc.

### 4. Regular Processing
- Daily: Process fleeting notes (5-10 min)
- Weekly: Review orphans and old notes (30 min)
- Monthly: Check statistics and graph structure

### 5. Progressive Summarization
- First pass: Capture (fleeting)
- Second pass: Process and link (permanent)
- Third pass: Synthesize and create hub notes

## Advanced Features

### Custom Templates

Insert templates in any note:
- Question template
- Comparison template (A vs B)
- Source/reference template

### Literature Workflow

1. Add paper to Zotero
2. `C-c n k` - Insert citation
3. `C-c n L` - Create literature note from citation
4. Process → create permanent notes
5. Link permanent notes together

### GTD Integration

- Capture tasks: `C-c c t`
- Daily notes can have TODOs
- Agenda shows TODOs from roam notes
- `C-c a` - View agenda
- Custom views: `d` (dashboard), `w` (weekly), `r` (reading), `p` (projects)

### Screenshot Integration

- `C-c n s` - Take screenshot and insert
- Uses scrot (Linux)
- Saves to `~/wdata/note/img/`

## Keybinding Reference

### Capture & Create
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n j   | Daily note entry                |
| C-c n c   | Capture with template           |
| C-c n f   | Find/create note                |
| C-c c     | Org capture menu                |

### Navigate & Search
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n f   | Find note                       |
| C-c n t   | Filter by tag                   |
| C-c n S   | Full-text search                |

### Link & Connect
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n i   | Insert link                     |
| C-c n I   | Insert link (immediate)         |
| C-c n l   | Toggle backlinks                |
| C-c n C   | Backlink context                |
| C-c n r   | Extract subtree to new note     |

### Process & Review
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n D   | Daily review                    |
| C-c n P   | Process fleeting notes          |
| C-c n p   | Convert to permanent            |
| C-c w p   | Process inbox                   |
| C-c w r   | Weekly review                   |

### Quality & Maintenance
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n S   | Show statistics                 |
| C-c n O   | Find orphans                    |
| C-c n U   | Find unlinked                   |
| C-c n R   | Review old notes                |
| C-c n g   | Open graph view                 |

### Literature & Citations
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n k   | Insert citation                 |
| C-c n L   | Open literature note            |
| C-c n b   | Attach citekey to node          |

### Utilities
| Key       | Action                          |
|-----------|---------------------------------|
| C-c n s   | Screenshot                      |
| C-c n E   | Export with links               |
| C-c n a   | Add alias                       |
| C-c w l   | Convert heading to node         |

## Example Workflow

### Morning Routine (5 min)
1. `C-c n j` - Open today's daily note
2. Write morning thoughts
3. Review yesterday's fleeting notes

### During Research (ongoing)
1. Reading paper → `C-c c r` - Capture to inbox
2. Important idea → `C-c n c f` - Fleeting note
3. Found connection → `C-c n i` - Link notes

### Evening Processing (10-15 min)
1. `C-c n D` - Start daily review
2. `C-c n P` - Process fleeting notes
3. For each fleeting:
   - Refine the idea
   - Add links to related notes
   - `C-c n p` - Convert to permanent
4. `C-c n O` - Check for orphans
5. Link orphans or delete if not useful

### Weekly Review (30 min)
1. `C-c w r` - Open weekly review
2. `C-c n S` - Check statistics
3. `C-c n R` - Review old notes
4. `C-c n g` - View graph, identify clusters
5. Create hub notes for major topics

## Tips for Success

1. **Start Small**: Begin with 5-10 notes, focus on linking
2. **Process Daily**: Don't let fleeting notes pile up
3. **Link First**: When creating a note, immediately link to 2-3 related notes
4. **Review Graph**: Weekly graph review reveals knowledge structure
5. **Embrace Imperfection**: Notes evolve over time
6. **Use Daily Notes**: Great for capturing context and progress
7. **Tag Sparingly**: Over-tagging defeats the purpose of links

## Troubleshooting

**Slow performance?**
- Run `M-x org-roam-db-sync` to rebuild database
- Check `org-roam.db` file size

**Missing backlinks?**
- Ensure `org-roam-db-autosync-mode` is enabled
- Save files after creating links

**Can't find notes?**
- Use `C-c n S` for full-text search
- Check if file is in `org-roam-directory`

**Graph not showing?**
- Ensure org-roam-ui is installed
- Run `M-x org-roam-ui-open`

## Resources

- Zettelkasten Method: https://zettelkasten.de/
- Org-roam Manual: https://www.orgroam.com/manual.html
- How to Take Smart Notes (book by Sönke Ahrens)
