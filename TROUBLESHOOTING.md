# Troubleshooting & FAQ

## Quick Fixes

### Problem: "Can't find my note"

**Solutions:**
```elisp
1. Full-text search:    C-c n S
2. Filter by tag:       C-c n t
3. Browse all notes:    C-c n f
4. Check agenda:        C-c a (if note has TODO)
```

**Common causes:**
- Note is in a subdirectory not scanned by org-roam
- Typo in title
- Note hasn't been saved yet

### Problem: "Backlinks not showing"

**Solutions:**
```elisp
1. Save the file:       C-x C-s
2. Rebuild database:    M-x org-roam-db-sync
3. Toggle backlinks:    C-c n l
4. Check link syntax:   [[id:...][title]] or [[title]]
```

**Common causes:**
- File not saved
- Database out of sync
- Wrong link format
- File outside org-roam-directory

### Problem: "Graph is empty or incomplete"

**Solutions:**
```elisp
1. Create at least 2 notes
2. Link them together:  C-c n i
3. Save both files:     C-x C-s
4. Sync database:       M-x org-roam-db-sync
5. Refresh graph:       C-c n g
```

**Common causes:**
- No notes created yet
- Notes not linked
- Database not synced
- Browser blocking localhost

### Problem: "Packages not loading"

**Solutions:**
```elisp
1. Check messages:      C-h e
2. Rebuild packages:    M-x straight-rebuild-all
3. Pull updates:        M-x straight-pull-all
4. Restart Emacs
```

**Common causes:**
- Package installation interrupted
- Network issues during install
- Conflicting package versions
- Corrupted package cache

### Problem: "Slow performance"

**Solutions:**
```elisp
1. Disable appearance:  Comment out (require 'init-org-appearance)
2. Increase GC:         (setq gc-cons-threshold (* 256 1024 1024))
3. Disable auto-save:   (setq auto-save-default nil)
4. Check database:      Delete org-roam.db and rebuild
```

**Common causes:**
- Too many appearance packages
- Large database (>10,000 notes)
- Low memory
- Slow disk I/O

### Problem: "Export not working"

**Solutions:**
```elisp
1. Check htmlize:       M-x package-install RET htmlize
2. Check pandoc:        pandoc --version (in terminal)
3. Check permissions:   Can you write to export directory?
4. Try simple export:   C-c C-e h o (org-mode export)
```

**Common causes:**
- Missing htmlize package
- Pandoc not installed
- Permission issues
- Invalid export template

### Problem: "AI features not working"

**Solutions:**
```elisp
1. Check API key:       M-: gptel-api-key
2. Test connection:     M-x gptel
3. Check network:       Can you reach api.openai.com?
4. Check balance:       Do you have API credits?
```

**Common causes:**
- API key not set
- Invalid API key
- Network/firewall blocking
- No API credits
- Rate limit exceeded

## Common Errors

### Error: "org-roam-db-autosync-mode: Wrong type argument"

**Cause:** Database schema mismatch

**Fix:**
```elisp
1. M-x org-roam-db-clear-all
2. M-x org-roam-db-sync
3. Restart Emacs
```

### Error: "Cannot open load file: No such file or directory"

**Cause:** Missing package

**Fix:**
```elisp
1. Check which package: Look at error message
2. Install manually:    M-x straight-use-package RET package-name
3. Restart Emacs
```

### Error: "Symbol's function definition is void"

**Cause:** Function not loaded or package not installed

**Fix:**
```elisp
1. Check if package loaded:  M-x describe-function RET function-name
2. Load package manually:    M-x require RET package-name
3. Check init.el order:      Ensure packages loaded before use
```

### Error: "File exists, but cannot be read"

**Cause:** Permission issues

**Fix:**
```bash
# In terminal
chmod 644 ~/wdata/note/roam/*.org
chmod 755 ~/wdata/note/roam/
```

### Error: "Org-roam database is locked"

**Cause:** Another Emacs instance or crashed process

**Fix:**
```bash
# In terminal
rm ~/.emacs.d/org-roam.db-shm
rm ~/.emacs.d/org-roam.db-wal
# Then in Emacs
M-x org-roam-db-sync
```

## Frequently Asked Questions

### General Questions

**Q: How many notes should I have?**

A: Start with 5-10, aim for 50+ in first month. Quality > quantity. The system becomes valuable around 100+ well-linked notes.

**Q: How long should a note be?**

A: Permanent notes: 100-300 words. If longer, consider splitting. Fleeting notes: Any length, they're temporary.

**Q: How many links per note?**

A: Aim for 3-5 links. Minimum 2, maximum 10. More links = better discoverability.

**Q: Should I delete notes?**

A: Yes! Delete freely during processing. If a fleeting note doesn't spark insight, delete it. Quality > quantity.

**Q: Can I use this for work notes?**

A: Absolutely! Create separate directories or use tags to separate personal/work notes.

**Q: How do I backup my notes?**

A: They're already in git! Just push regularly. Also consider:
- Cloud sync (Dropbox, Google Drive)
- External backup drive
- GitHub private repo

### Workflow Questions

**Q: I have 50 fleeting notes. Help!**

A: Don't process all at once:
1. Sort by date (oldest first)
2. Process 5-10 per day
3. Delete liberally (most won't be worth keeping)
4. Going forward: process daily

**Q: How do I handle long articles/papers?**

A: Create one literature note per source, then extract key ideas into separate permanent notes. Link permanent notes back to literature note.

**Q: Should I tag or link?**

A: Prefer links! Tags are for broad categories (domain, type). Links are for specific relationships. Use 2-4 tags max, unlimited links.

**Q: What if I can't think of links?**

A: Ask yourself:
- What does this remind me of?
- What contradicts this?
- What examples exist?
- What are the prerequisites?
- What are the consequences?

**Q: How do I organize notes?**

A: Don't organize by folders! Use:
- Links (primary organization)
- Tags (secondary, for filtering)
- Hub notes (for major topics)
- Graph view (for discovery)

### Technical Questions

**Q: Can I use Dropbox/Google Drive?**

A: Yes, but be careful:
- Don't sync .emacs.d/ (packages can break)
- Do sync ~/wdata/note/
- Watch for sync conflicts
- Consider git instead

**Q: Can I use this on mobile?**

A: Limited options:
- Orgzly (Android) - basic org-mode support
- Beorg (iOS) - basic org-mode support
- SSH + Emacs in terminal
- Web interface via org-roam-ui

**Q: How do I migrate from Obsidian/Notion?**

A:
1. Export to Markdown
2. Convert to org-mode: `pandoc -f markdown -t org file.md -o file.org`
3. Move to org-roam directory
4. Run `M-x org-roam-db-sync`
5. Fix links manually (or write script)

**Q: Can I use this without Emacs?**

A: No, this is an Emacs configuration. But org-roam files are plain text, so you can:
- Read them in any text editor
- Search with grep/ripgrep
- Version control with git
- Process with scripts

**Q: How do I share notes with non-Emacs users?**

A: Use export features:
- HTML: `C-c e h` (beautiful, readable)
- Markdown: `C-c e m` (GitHub-compatible)
- PDF: `C-c e p` (via Pandoc)

### Customization Questions

**Q: Can I change the directory structure?**

A: Yes! Edit `org-roam-capture-templates` in `init-roam.el`:
```elisp
:if-new (file+head "your-dir/%<%Y%m%d%H%M%S>-${slug}.org" ...)
```

**Q: Can I add custom note types?**

A: Yes! Add to `org-roam-capture-templates`:
```elisp
("x" "your-type" plain
 "Your template"
 :if-new (file+head "your-dir/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: your-tag\n")
 :unnarrowed t)
```

**Q: Can I change keybindings?**

A: Yes! Add to your init.el:
```elisp
(with-eval-after-load 'org-roam
  (define-key org-roam-mode-map (kbd "C-c z") #'org-roam-node-find))
```

**Q: Can I disable appearance features?**

A: Yes! Comment out in init.el:
```elisp
;; (require 'init-org-appearance)
```

**Q: Can I use a different theme?**

A: Yes! Edit `init-themes.el` or set in init.el:
```elisp
(load-theme 'your-theme t)
```

### AI Questions

**Q: Do I need AI features?**

A: No! They're completely optional. The core system works great without AI.

**Q: Which AI model should I use?**

A:
- GPT-4: Best quality, slower, more expensive
- GPT-3.5-turbo: Good quality, faster, cheaper
- Start with GPT-3.5-turbo

**Q: How much does AI cost?**

A: Depends on usage:
- Light use (10 requests/day): ~$5/month
- Medium use (50 requests/day): ~$20/month
- Heavy use (200 requests/day): ~$80/month

**Q: Can I use local AI models?**

A: Yes! Use:
- Ollama (local LLMs)
- LM Studio
- GPT4All
Configure gptel to use local endpoint

**Q: Should I trust AI suggestions?**

A: No! Always review:
- AI can hallucinate
- AI doesn't understand your context
- AI is a tool, not a replacement for thinking
- Use AI for inspiration, not truth

## Performance Optimization

### For Large Zettelkasten (1000+ notes)

**1. Database optimization:**
```elisp
;; In init.el
(setq org-roam-db-gc-threshold most-positive-fixnum)
```

**2. Disable auto-save:**
```elisp
(setq org-roam-db-update-on-save nil)
;; Manually sync: M-x org-roam-db-sync
```

**3. Reduce appearance features:**
```elisp
;; Comment out in init-org-appearance.el
;; (use-package org-modern ...)
;; (use-package mixed-pitch ...)
```

**4. Increase GC threshold:**
```elisp
(setq gc-cons-threshold (* 256 1024 1024))
```

**5. Use SSD:**
- Store notes on SSD, not HDD
- Significant performance improvement

### For Slow Startup

**1. Defer package loading:**
```elisp
(use-package package-name
  :defer t  ; Load only when needed
  ...)
```

**2. Reduce auto-loaded packages:**
```elisp
;; Comment out packages you don't use daily
```

**3. Use native compilation (Emacs 28+):**
```elisp
(setq native-comp-async-report-warnings-errors nil)
```

**4. Profile startup:**
```elisp
M-x esup  ; Emacs Start Up Profiler
```

## Data Management

### Backup Strategy

**Daily (automatic):**
- Git commits (if you commit daily)
- Cloud sync (if enabled)

**Weekly (manual):**
```bash
# Full backup
tar -czf zettelkasten-backup-$(date +%Y%m%d).tar.gz ~/wdata/note/
```

**Monthly (archive):**
```bash
# Move to external drive
cp zettelkasten-backup-*.tar.gz /path/to/external/drive/
```

### Cleaning Up

**Find and delete duplicates:**
```elisp
M-x org-roam-db-sync
C-c n S  ; Check statistics
; Manually review similar titles
```

**Archive old notes:**
```bash
# Move notes older than 2 years with no links
# (Write custom script or do manually)
```

**Optimize database:**
```elisp
M-x org-roam-db-clear-all
M-x org-roam-db-sync
```

### Migration

**From old org-roam v1:**
```elisp
M-x org-roam-migrate-wizard
```

**From plain org files:**
```elisp
1. Move files to org-roam-directory
2. Add #+title: to each file
3. M-x org-roam-db-sync
4. Manually add links
```

**To new machine:**
```bash
# On old machine
cd ~/wdata/note
git push origin master

# On new machine
git clone your-repo ~/wdata/note
cd ~/.emacs.d
git pull origin master
# In Emacs
M-x org-roam-db-sync
```

## Best Practices

### Do's ✓

- **Do** process fleeting notes daily
- **Do** link while writing (not later)
- **Do** write in your own words
- **Do** delete freely during processing
- **Do** review weekly
- **Do** keep notes atomic (one idea)
- **Do** add context when linking
- **Do** use daily notes for journaling
- **Do** backup regularly
- **Do** commit to git frequently

### Don'ts ✗

- **Don't** let fleeting notes pile up (>20)
- **Don't** copy-paste without processing
- **Don't** create notes without links
- **Don't** use too many tags (>5)
- **Don't** make notes too long (>500 words)
- **Don't** organize by folders
- **Don't** worry about perfect organization
- **Don't** delete permanent notes lightly
- **Don't** skip daily review
- **Don't** trust AI blindly

## Getting Help

### In Emacs

```elisp
C-h k <key>         ; What does this key do?
C-h f <function>    ; Function documentation
C-h v <variable>    ; Variable value
C-h m               ; Current mode help
C-h e               ; View messages
C-h i               ; Info manuals
```

### Documentation

- `GETTING_STARTED.md` - Quick start (5 min)
- `QUICK_REFERENCE.md` - Cheat sheet
- `ZETTELKASTEN_GUIDE.md` - Complete guide
- `ARCHITECTURE.md` - System diagrams
- This file - Troubleshooting

### Online Resources

**Zettelkasten Method:**
- https://zettelkasten.de/
- https://www.reddit.com/r/Zettelkasten/
- Book: "How to Take Smart Notes" by Sönke Ahrens

**Org-roam:**
- https://www.orgroam.com/manual.html
- https://org-roam.discourse.group/
- https://github.com/org-roam/org-roam

**Org-mode:**
- https://orgmode.org/
- https://orgmode.org/manual/
- https://www.reddit.com/r/orgmode/

**Emacs:**
- https://www.gnu.org/software/emacs/manual/
- https://www.emacswiki.org/
- https://www.reddit.com/r/emacs/

### Community

**Ask questions:**
- Org-roam Discourse: https://org-roam.discourse.group/
- Reddit r/orgmode: https://www.reddit.com/r/orgmode/
- Reddit r/Zettelkasten: https://www.reddit.com/r/Zettelkasten/
- Emacs StackExchange: https://emacs.stackexchange.com/

**Report bugs:**
- This config: https://github.com/KangYueh/.emacs.d/issues
- Org-roam: https://github.com/org-roam/org-roam/issues

## Debugging

### Enable Debug Mode

```elisp
;; In init.el or evaluate
(setq debug-on-error t)
```

Now when an error occurs, you'll see a full backtrace.

### Check Package Status

```elisp
M-x list-packages
; Search for: org-roam, citar, consult-org-roam
; Check if installed and version
```

### Check Variables

```elisp
M-: org-roam-directory          ; Check directory path
M-: org-roam-db-location        ; Check database location
M-: org-agenda-files            ; Check agenda files
```

### Check Database

```bash
# In terminal
ls -lh ~/.emacs.d/org-roam.db
# Should exist and have reasonable size (few MB)

# Check integrity
sqlite3 ~/.emacs.d/org-roam.db "PRAGMA integrity_check;"
```

### Minimal Config Test

```bash
# Start Emacs with minimal config
emacs -Q

# Then evaluate:
(add-to-list 'load-path "~/.emacs.d/straight/repos/org-roam")
(require 'org-roam)
(setq org-roam-directory "~/wdata/note/roam")
(org-roam-db-autosync-mode)

# If this works, problem is in your config
# If this fails, problem is with org-roam installation
```

### Log Everything

```elisp
;; Add to init.el temporarily
(setq org-roam-verbose t)
(setq message-log-max 10000)

;; Check *Messages* buffer after operations
C-h e
```

## Still Stuck?

### Before Asking for Help

1. **Check error message:** `C-h e`
2. **Try minimal config:** `emacs -Q`
3. **Check documentation:** Read relevant .md file
4. **Search online:** Google the error message
5. **Check GitHub issues:** Someone may have had same problem

### When Asking for Help

Include:
1. **What you tried:** Exact steps
2. **What happened:** Error message or unexpected behavior
3. **What you expected:** What should have happened
4. **Your setup:** Emacs version, OS, relevant config
5. **Minimal example:** Simplest case that reproduces issue

### Example Good Question

```
Title: org-roam-db-sync fails with "database locked" error

Environment:
- Emacs 29.1
- Windows 10
- org-roam 2.2.2

Steps to reproduce:
1. Start Emacs
2. M-x org-roam-db-sync
3. Error: "database is locked"

What I tried:
- Restarted Emacs
- Deleted .db-shm and .db-wal files
- Checked no other Emacs instance running

Error message:
[paste full error from *Messages*]

Config:
[paste relevant parts of init.el]
```

## Summary

Most issues are solved by:
1. Saving files (`C-x C-s`)
2. Syncing database (`M-x org-roam-db-sync`)
3. Restarting Emacs
4. Checking documentation
5. Asking for help with details

Remember: The system is robust and well-tested. Most "bugs" are configuration issues or misunderstandings. Take time to read the documentation!

---

**Still have questions?** Check the other documentation files or ask in the community!
