# Zettelkasten System Architecture

## Visual Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                    YOUR ZETTELKASTEN SYSTEM                         │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                         INPUT LAYER                                 │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Quick Capture          Daily Notes           GTD Inbox            │
│  ┌──────────┐          ┌──────────┐          ┌──────────┐         │
│  │ C-c n c f│          │ C-c n j  │          │ C-c c    │         │
│  │ Fleeting │          │ Journal  │          │ Tasks    │         │
│  └────┬─────┘          └────┬─────┘          └────┬─────┘         │
│       │                     │                     │                │
│       └─────────────────────┼─────────────────────┘                │
│                             ▼                                       │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                      PROCESSING LAYER                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Daily Review (C-c n D)                                            │
│  ┌───────────────────────────────────────────────────────────┐    │
│  │  1. View fleeting notes                                    │    │
│  │  2. Refine ideas                                           │    │
│  │  3. Add links (C-c n i)                                    │    │
│  │  4. Convert to permanent (C-c n p)                         │    │
│  │  5. Check orphans (C-c n O)                                │    │
│  └───────────────────────────────────────────────────────────┘    │
│                             ▼                                       │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                       KNOWLEDGE BASE                                │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐            │
│  │  Permanent   │  │   Concept    │  │  Literature  │            │
│  │    Notes     │  │    Notes     │  │    Notes     │            │
│  │              │  │              │  │              │            │
│  │  • Atomic    │  │  • Defs      │  │  • Papers    │            │
│  │  • Linked    │  │  • Props     │  │  • Books     │            │
│  │  • Refined   │  │  • Examples  │  │  • Articles  │            │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘            │
│         │                 │                 │                     │
│         └─────────────────┼─────────────────┘                     │
│                           ▼                                        │
│                  ┌─────────────────┐                              │
│                  │  Knowledge Graph │                              │
│                  │   (C-c n g)     │                              │
│                  └─────────────────┘                              │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                      QUALITY CONTROL                                │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Statistics        Orphan Detection    Staleness Check             │
│  ┌──────────┐     ┌──────────┐        ┌──────────┐               │
│  │ C-c n S  │     │ C-c n O  │        │ C-c n R  │               │
│  │ Overview │     │ No links │        │ Old notes│               │
│  └──────────┘     └──────────┘        └──────────┘               │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                       OUTPUT LAYER                                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Export HTML       Export Markdown     Share Clusters              │
│  ┌──────────┐     ┌──────────┐        ┌──────────┐               │
│  │ C-c e h  │     │ C-c e m  │        │ C-c e c  │               │
│  │ Beautiful│     │ GitHub   │        │ With     │               │
│  │ styling  │     │ format   │        │ links    │               │
│  └──────────┘     └──────────┘        └──────────┘               │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Note Type Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                      NOTE LIFECYCLE                                 │
└─────────────────────────────────────────────────────────────────────┘

1. CAPTURE (Fleeting)
   ┌─────────────────┐
   │ Quick idea      │
   │ Unprocessed     │
   │ Raw thoughts    │
   └────────┬────────┘
            │
            │ Process within 1-2 days
            ▼
2. REFINE (Processing)
   ┌─────────────────┐
   │ Clarify idea    │
   │ Add context     │
   │ Find links      │
   └────────┬────────┘
            │
            │ Convert (C-c n p)
            ▼
3. PERMANENT (Knowledge Base)
   ┌─────────────────┐
   │ Atomic note     │
   │ Well-linked     │
   │ Self-contained  │
   └────────┬────────┘
            │
            │ Continuous improvement
            ▼
4. CONNECTED (Network)
   ┌─────────────────┐
   │ Part of graph   │
   │ Multiple links  │
   │ Discoverable    │
   └─────────────────┘
```

## Directory Structure

```
~/wdata/note/
│
├── roam/                          # Org-roam notes
│   ├── daily/                     # Daily journal entries
│   │   ├── 2026-03-01.org
│   │   ├── 2026-03-02.org
│   │   └── ...
│   │
│   ├── fleeting/                  # Unprocessed quick captures
│   │   ├── 20260302120000-quick-idea.org
│   │   └── ...
│   │
│   ├── permanent/                 # Refined atomic notes
│   │   ├── 20260301150000-spaced-repetition.org
│   │   ├── 20260302100000-neural-networks.org
│   │   └── ...
│   │
│   ├── concepts/                  # Concept definitions
│   │   ├── 20260301140000-backpropagation.org
│   │   └── ...
│   │
│   ├── methods/                   # Techniques and processes
│   │   ├── 20260302110000-pomodoro-technique.org
│   │   └── ...
│   │
│   ├── questions/                 # Research questions
│   │   ├── 20260301160000-how-does-attention-work.org
│   │   └── ...
│   │
│   ├── references/                # Literature notes
│   │   ├── 20260301130000-attention-is-all-you-need.org
│   │   └── ...
│   │
│   └── index.org                  # Hub/index file
│
├── org/                           # GTD files
│   ├── inbox.org                  # Capture inbox
│   ├── gtd.org                    # Next actions
│   ├── projects.org               # Active projects
│   ├── someday.org                # Someday/maybe
│   ├── archive.org                # Completed items
│   └── journal.org                # Journal entries
│
├── exports/                       # Exported files
│   ├── 20260302-my-note.html
│   ├── 20260302-cluster/
│   └── ...
│
└── img/                           # Screenshots and images
    ├── screenshot_20260302_120000.png
    └── ...
```

## Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                      DAILY WORKFLOW                                 │
└─────────────────────────────────────────────────────────────────────┘

MORNING (2 min)
┌──────────────┐
│ C-c n j      │ → Open daily note
│ Write goals  │
│ Review plan  │
└──────────────┘

DURING DAY (ongoing)
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│ Quick idea?  │ →   │ C-c n c f    │ →   │ Fleeting     │
│              │     │ Capture      │     │ note created │
└──────────────┘     └──────────────┘     └──────────────┘

┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│ Reading?     │ →   │ C-c c r      │ →   │ Added to     │
│              │     │ Capture      │     │ inbox        │
└──────────────┘     └──────────────┘     └──────────────┘

┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│ Connection?  │ →   │ C-c n i      │ →   │ Link added   │
│              │     │ Insert link  │     │              │
└──────────────┘     └──────────────┘     └──────────────┘

EVENING (8 min)
┌──────────────┐
│ C-c n D      │ → Daily review dashboard
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ C-c n P      │ → Process fleeting notes
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────┐
│ For each fleeting note:          │
│ 1. Read and refine               │
│ 2. Add links (C-c n i)           │
│ 3. Convert (C-c n p)             │
│ 4. Or delete if not useful       │
└──────┬───────────────────────────┘
       │
       ▼
┌──────────────┐
│ C-c n O      │ → Check orphans
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Link or      │
│ delete       │
└──────────────┘
```

## Weekly Review Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                     WEEKLY REVIEW (30 min)                          │
└─────────────────────────────────────────────────────────────────────┘

REVIEW (15 min)
┌──────────────┐
│ C-c w r      │ → Weekly review agenda
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ C-c n S      │ → Check statistics
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ C-c n R      │ → Review old notes (>30 days)
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Update stale │
│ notes        │
└──────────────┘

MAINTAIN (15 min)
┌──────────────┐
│ C-c n g      │ → View graph
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Identify     │
│ clusters     │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Create hub   │
│ notes        │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ C-c n U      │ → Find unlinked notes
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Add          │
│ connections  │
└──────────────┘
```

## Keybinding Map

```
┌─────────────────────────────────────────────────────────────────────┐
│                    KEYBINDING CATEGORIES                            │
└─────────────────────────────────────────────────────────────────────┘

C-c n *  → Org-roam / Zettelkasten commands
C-c c    → Org capture (GTD)
C-c a    → Org agenda
C-c e *  → Export commands
C-c w *  → Workflow commands
C-c A *  → AI commands (optional)

MOST USED (Learn First)
┌─────────────────────────────────────┐
│ C-c n c   Capture new note          │
│ C-c n f   Find note                 │
│ C-c n i   Insert link               │
│ C-c n l   Toggle backlinks          │
│ C-c n j   Daily note                │
└─────────────────────────────────────┘

PROCESSING
┌─────────────────────────────────────┐
│ C-c n D   Daily review              │
│ C-c n P   Process fleeting          │
│ C-c n p   Convert to permanent      │
│ C-c w p   Process inbox             │
│ C-c w r   Weekly review             │
└─────────────────────────────────────┘

QUALITY CONTROL
┌─────────────────────────────────────┐
│ C-c n S   Statistics                │
│ C-c n O   Find orphans              │
│ C-c n U   Find unlinked             │
│ C-c n R   Review old notes          │
│ C-c n g   Graph view                │
└─────────────────────────────────────┘

EXPORT
┌─────────────────────────────────────┐
│ C-c e h   Export to HTML            │
│ C-c e m   Export to Markdown        │
│ C-c e b   Export with backlinks     │
│ C-c e c   Export cluster            │
└─────────────────────────────────────┘
```

## Feature Modules

```
┌─────────────────────────────────────────────────────────────────────┐
│                      MODULE ARCHITECTURE                            │
└─────────────────────────────────────────────────────────────────────┘

init.el
  │
  ├─→ init-org.el                    [Core org-mode]
  │     • GTD workflow
  │     • Capture templates
  │     • Agenda views
  │     • Helper functions
  │
  ├─→ init-org-appearance.el         [Optional: Beautiful UI]
  │     • org-modern
  │     • org-superstar
  │     • mixed-pitch fonts
  │     • visual-fill-column
  │     • Custom faces
  │
  ├─→ init-org-export.el             [Optional: Export]
  │     • HTML with CSS
  │     • Markdown (GFM)
  │     • PDF via Pandoc
  │     • Export with backlinks
  │
  ├─→ init-roam.el                   [Core Zettelkasten]
  │     • 6 note types
  │     • Capture templates
  │     • Helper functions
  │     • Keybindings
  │
  ├─→ init-zettelkasten.el           [Maintenance toolkit]
  │     • Statistics
  │     • Orphan detection
  │     • Processing workflow
  │     • Quality control
  │     • Templates
  │
  └─→ init-org-ai.el                 [Optional: AI assistance]
        • Summarization
        • Link suggestions
        • Note expansion
        • Critique
        • Tag suggestions
```

## Configuration Levels

```
┌─────────────────────────────────────────────────────────────────────┐
│                   CONFIGURATION OPTIONS                             │
└─────────────────────────────────────────────────────────────────────┘

LEVEL 1: MINIMAL (Core only)
┌────────────────────────────────┐
│ (require 'init-org)            │
│ (require 'init-roam)           │
│ (require 'init-zettelkasten)   │
└────────────────────────────────┘
Features: Core Zettelkasten, 6 note types, basic workflow
Size: ~500 lines of code
Startup: Fast

LEVEL 2: RECOMMENDED (Core + Appearance)
┌────────────────────────────────┐
│ (require 'init-org)            │
│ (require 'init-org-appearance) │
│ (require 'init-roam)           │
│ (require 'init-zettelkasten)   │
└────────────────────────────────┘
Features: + Beautiful UI, modern styling
Size: ~800 lines of code
Startup: Medium

LEVEL 3: FULL (Everything)
┌────────────────────────────────┐
│ (require 'init-org)            │
│ (require 'init-org-appearance) │
│ (require 'init-org-export)     │
│ (require 'init-roam)           │
│ (require 'init-zettelkasten)   │
│ (require 'init-org-ai)         │
└────────────────────────────────┘
Features: + Export, AI assistance
Size: ~1200 lines of code
Startup: Slower (but still reasonable)
Requires: OpenAI API key for AI
```

## Growth Timeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                  ZETTELKASTEN GROWTH CURVE                          │
└─────────────────────────────────────────────────────────────────────┘

Week 1: Learning Phase
┌────────────────────────┐
│ • 5-10 notes           │
│ • Learn keybindings    │
│ • Practice linking     │
│ • Daily review habit   │
└────────────────────────┘

Month 1: Building Phase
┌────────────────────────┐
│ • 50+ notes            │
│ • Established workflow │
│ • First clusters       │
│ • Weekly reviews       │
└────────────────────────┘

Month 3: Network Phase
┌────────────────────────┐
│ • 150+ notes           │
│ • Strong connections   │
│ • Hub notes emerging   │
│ • Insights appearing   │
└────────────────────────┘

Month 6: Mature Phase
┌────────────────────────┐
│ • 300+ notes           │
│ • Dense network        │
│ • Serendipitous finds  │
│ • High value output    │
└────────────────────────┘

Year 1: Expert Phase
┌────────────────────────┐
│ • 500-1000+ notes      │
│ • Second brain         │
│ • Effortless recall    │
│ • Creative synthesis   │
└────────────────────────┘
```

## Quality Metrics

```
┌─────────────────────────────────────────────────────────────────────┐
│                    HEALTH INDICATORS                                │
└─────────────────────────────────────────────────────────────────────┘

GOOD SIGNS ✓
┌────────────────────────────────────┐
│ • Orphan rate < 10%                │
│ • Average links per note: 3-5      │
│ • Fleeting notes < 10              │
│ • Daily review streak > 7 days     │
│ • Graph has clusters               │
│ • Notes are atomic (focused)       │
└────────────────────────────────────┘

WARNING SIGNS ⚠
┌────────────────────────────────────┐
│ • Orphan rate > 20%                │
│ • Average links < 2                │
│ • Fleeting notes > 20              │
│ • No review in > 3 days            │
│ • Graph is disconnected            │
│ • Notes are too long (>500 words) │
└────────────────────────────────────┘

Check with: C-c n S (Statistics)
```

## Integration Points

```
┌─────────────────────────────────────────────────────────────────────┐
│                    SYSTEM INTEGRATIONS                              │
└─────────────────────────────────────────────────────────────────────┘

Zotero (Bibliography)
┌────────────────────────────────────┐
│ ~/wdata/zotero/all.bib             │
│         ↓                          │
│ Citar (C-c n k)                    │
│         ↓                          │
│ Literature notes                   │
└────────────────────────────────────┘

Git (Version Control)
┌────────────────────────────────────┐
│ All notes in git                   │
│         ↓                          │
│ Automatic backups                  │
│         ↓                          │
│ Sync across machines               │
└────────────────────────────────────┘

Org Agenda (GTD)
┌────────────────────────────────────┐
│ TODOs in notes                     │
│         ↓                          │
│ Appear in agenda                   │
│         ↓                          │
│ Unified task management            │
└────────────────────────────────────┘

Export (Sharing)
┌────────────────────────────────────┐
│ Notes                              │
│         ↓                          │
│ HTML/Markdown/PDF                  │
│         ↓                          │
│ Share with others                  │
└────────────────────────────────────┘
```

---

This diagram shows the complete architecture of your Zettelkasten system!
