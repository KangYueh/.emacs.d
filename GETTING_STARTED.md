# Getting Started in 5 Minutes

## Step 1: Install (2 minutes)

```bash
# 1. Backup your current config
cp -r ~/.emacs.d ~/.emacs.d.backup

# 2. Pull the new config (if not already done)
cd ~/.emacs.d
git pull origin master

# 3. Create directories
mkdir -p ~/wdata/note/roam/{daily,fleeting,permanent,concepts,methods,questions,references}
mkdir -p ~/wdata/note/{org,exports,img}
touch ~/wdata/note/org/{inbox,gtd,projects,someday,archive,journal}.org

# 4. Restart Emacs
```

## Step 2: Initialize (1 minute)

In Emacs:
```elisp
M-x org-roam-db-sync
```

Wait for it to complete. You'll see "Processed X files" in the minibuffer.

## Step 3: Create Your First Note (2 minutes)

### Try This Right Now:

1. **Press:** `C-c n c` (Capture new note)
2. **Choose:** `p` (for permanent note)
3. **Type title:** "My First Zettelkasten Note"
4. **Press:** `Enter`

You'll see a template like this:
```org
#+title: My First Zettelkasten Note
#+filetags: permanent
#+date: 2026-03-02
#+created: [2026-03-02 Sun 11:30]

* Core Idea


* Explanation


* Examples


* Connections


* References
```

5. **Fill it in:**
```org
* Core Idea
Zettelkasten is a note-taking method that focuses on atomic notes and connections.

* Explanation
Each note should contain one idea. Notes are linked together to form a knowledge network.

* Examples
- This note is an example of a permanent note
- It has a clear structure
- It will link to other notes

* Connections
(We'll add links in the next step)

* References
- Zettelkasten.de
```

6. **Save:** `C-x C-s`

## Step 4: Create a Second Note and Link Them

1. **Press:** `C-c n c p` (Create another permanent note)
2. **Type title:** "Atomic Notes"
3. **Fill in:**
```org
* Core Idea
An atomic note contains exactly one idea or concept.

* Explanation
Atomic means indivisible. Each note should be self-contained and focused on a single concept.

* Examples
- One concept per note
- Can be understood without reading other notes
- Easy to link and reuse

* Connections
This relates to [[My First Zettelkasten Note]] because atomicity is a core principle.
```

4. **To insert the link:**
   - Type `[[`
   - Start typing "My First"
   - Press `Tab` to autocomplete
   - Select the note
   - Press `Enter`

5. **Save:** `C-x C-s`

## Step 5: View Your Knowledge Graph

**Press:** `C-c n g`

A browser window will open showing your notes as a graph! You'll see:
- Two nodes (your notes)
- A connection between them
- Interactive visualization

Click nodes to navigate, drag to rearrange.

## 🎉 Congratulations!

You've just:
- ✅ Created your first permanent notes
- ✅ Linked notes together
- ✅ Viewed your knowledge graph

## What's Next?

### Today (10 minutes)
1. Create 3-5 more notes on topics you're learning
2. Link them together using `C-c n i`
3. Try a daily note: `C-c n j`

### This Week
1. **Read:** `QUICK_REFERENCE.md` (print it!)
2. **Practice:** Daily review workflow
   - Morning: `C-c n j` (daily note)
   - Evening: `C-c n D` (daily review)
3. **Explore:** Different note types
   - `C-c n c f` - Fleeting (quick capture)
   - `C-c n c c` - Concept (definition)
   - `C-c n c l` - Literature (from reading)

### This Month
1. **Read:** Full `ZETTELKASTEN_GUIDE.md`
2. **Build:** 50+ notes
3. **Establish:** Daily habit
4. **Customize:** To your workflow

## Essential Keybindings (Memorize These 10)

| Key       | Action                    | When to Use                |
|-----------|---------------------------|----------------------------|
| `C-c n c` | Capture new note          | Creating any note          |
| `C-c n f` | Find note                 | Searching for a note       |
| `C-c n i` | Insert link               | Linking notes together     |
| `C-c n l` | Toggle backlinks          | See what links to this     |
| `C-c n j` | Daily note                | Morning/evening journal    |
| `C-c n D` | Daily review              | Evening processing         |
| `C-c n P` | Process fleeting notes    | Converting quick captures  |
| `C-c n g` | Graph view                | Visualizing connections    |
| `C-c n S` | Statistics                | Checking your progress     |
| `C-c a`   | Agenda                    | Viewing tasks              |

## Daily Workflow (10 minutes/day)

### Morning (2 min)
```
1. C-c n j          → Open today's daily note
2. Write: What I want to accomplish today
3. Review: Yesterday's notes
```

### During Day (ongoing)
```
Quick idea?         → C-c n c f (fleeting note)
Reading something?  → C-c c r (capture to inbox)
Found connection?   → C-c n i (link notes)
```

### Evening (8 min)
```
1. C-c n D          → Start daily review
2. C-c n P          → Process fleeting notes
3. For each fleeting:
   - Refine the idea
   - Add links (C-c n i)
   - C-c n p (convert to permanent)
4. C-c n O          → Check for orphans
5. Link or delete orphans
```

## Common Tasks

### Creating Different Note Types

**Fleeting Note** (Quick capture):
```
C-c n c f
Title: "Interesting idea from podcast"
Write quick thoughts, process later
```

**Permanent Note** (Refined idea):
```
C-c n c p
Title: "Spaced Repetition Improves Memory"
Fill in: Core Idea, Explanation, Examples, Connections
```

**Concept Note** (Definition):
```
C-c n c c
Title: "Spaced Repetition"
Fill in: Definition, Properties, Related Concepts, Applications
```

**Literature Note** (From reading):
```
C-c n c l
Title: "Make It Stick - Chapter 2"
Fill in: Summary, Key Points, Quotes, Personal Notes
```

### Finding Notes

**By title:**
```
C-c n f
Type part of title
Select from list
```

**By tag:**
```
C-c n t
Choose tag
Browse filtered notes
```

**Full-text search:**
```
C-c n S
Type search term
Browse results
```

### Linking Notes

**While writing:**
```
Type: [[
Start typing note title
Press Tab
Select note
Press Enter
```

**Quick link:**
```
C-c n i
Search for note
Select
Link inserted
```

**Link without opening:**
```
C-c n I
(Capital I)
Faster for multiple links
```

### Viewing Connections

**Backlinks sidebar:**
```
C-c n l
Shows: What links to this note
Click to navigate
```

**Graph view:**
```
C-c n g
Interactive visualization
Click nodes to explore
```

**Backlink context:**
```
C-c n C
Shows: Full context around each backlink
Useful for understanding connections
```

## Troubleshooting

### "Can't find my note"
```
Try: C-c n S (full-text search)
Or: C-c n t (filter by tag)
Or: C-c n f (browse all notes)
```

### "Backlinks not showing"
```
1. Save the file: C-x C-s
2. Rebuild database: M-x org-roam-db-sync
3. Toggle backlinks: C-c n l
```

### "Graph is empty"
```
1. Create at least 2 notes
2. Link them together
3. Save both files
4. Refresh graph: C-c n g
```

### "Packages not loading"
```
1. Check *Messages* buffer: C-h e
2. Rebuild packages: M-x straight-rebuild-all
3. Restart Emacs
```

## Tips for Success

### 1. Start Small
- Create 5-10 notes first
- Focus on linking
- Don't worry about perfection

### 2. Link While Writing
- Don't defer linking to later
- Add 3-5 links per note
- Explain why you're linking

### 3. Process Daily
- Don't let fleeting notes pile up
- 10 minutes every evening
- Delete freely (quality > quantity)

### 4. Use Daily Notes
- Great for context and progress
- Capture quick thoughts
- Review what you learned

### 5. Review Weekly
- Check statistics: `C-c n S`
- View graph: `C-c n g`
- Find orphans: `C-c n O`
- Strengthen connections

## Example Session

Here's a real example of a 10-minute session:

```
[Morning - 2 min]
C-c n j
Write: "Today: Learn about neural networks"

[During day - ongoing]
Reading article about backpropagation...
C-c n c f
Title: "Backpropagation uses chain rule"
Content: "Quick note: backprop is just chain rule applied to neural nets"

[Evening - 8 min]
C-c n D                    # Start review
C-c n P                    # Process fleeting notes

Open: "Backpropagation uses chain rule"
Refine to:
  Core Idea: Backpropagation is the application of the chain rule...
  Explanation: [detailed explanation]
  Examples: [math example]

C-c n i                    # Link to "Chain Rule" note
C-c n i                    # Link to "Neural Networks" note
C-c n p                    # Convert to permanent

C-c n O                    # Check orphans
Found: "Old note about calculus"
C-c n i                    # Link it to "Chain Rule"

C-c n S                    # Check stats
"Total: 47 notes, Fleeting: 2, Permanent: 38"

Done! 🎉
```

## Next Steps

### Immediate
- [ ] Create 5 notes on topics you know
- [ ] Link them together
- [ ] View the graph

### This Week
- [ ] Print QUICK_REFERENCE.md
- [ ] Practice daily workflow
- [ ] Read full ZETTELKASTEN_GUIDE.md

### This Month
- [ ] Build to 50+ notes
- [ ] Establish daily habit
- [ ] Customize to your needs

## Get Help

### Documentation
- `QUICK_REFERENCE.md` - Cheat sheet
- `ZETTELKASTEN_GUIDE.md` - Complete guide
- `INSTALLATION.md` - Setup help

### In Emacs
```
C-h k <key>     What does this key do?
C-h m           Current mode help
C-h e           View messages
```

### Online
- Zettelkasten.de - Method explanation
- org-roam.com - Software documentation
- GitHub Issues - Report problems

## You're Ready!

You now know enough to start building your Zettelkasten. Remember:

1. **Start simple** - A few notes is fine
2. **Link liberally** - Connections are key
3. **Process daily** - 10 minutes/day
4. **Review weekly** - Check your progress
5. **Be patient** - Value grows over time

The system becomes more valuable as you add more notes and connections. Start today!

---

**Questions?** Check the other documentation files or create an issue on GitHub.

**Happy note-taking!** 📝✨
