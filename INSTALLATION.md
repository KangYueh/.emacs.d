# Complete Emacs Zettelkasten Configuration - Installation Guide

## Quick Start

### 1. Backup Your Current Configuration

```bash
# Backup existing config
cp -r ~/.emacs.d ~/.emacs.d.backup.$(date +%Y%m%d)

# Or if you want to start fresh
mv ~/.emacs.d ~/.emacs.d.old
```

### 2. Copy Improved Configuration Files

```bash
# Copy all improved files
cp /d/workspace/test1/.emacs.d/init.el ~/.emacs.d/
cp /d/workspace/test1/.emacs.d/lisp/init-org.el ~/.emacs.d/lisp/
cp /d/workspace/test1/.emacs.d/lisp/init-roam.el ~/.emacs.d/lisp/
cp /d/workspace/test1/.emacs.d/lisp/init-zettelkasten.el ~/.emacs.d/lisp/
cp /d/workspace/test1/.emacs.d/lisp/init-org-appearance.el ~/.emacs.d/lisp/
cp /d/workspace/test1/.emacs.d/lisp/init-org-export.el ~/.emacs.d/lisp/
cp /d/workspace/test1/.emacs.d/lisp/init-org-ai.el ~/.emacs.d/lisp/

# Copy documentation
cp /d/workspace/test1/.emacs.d/ZETTELKASTEN_GUIDE.md ~/.emacs.d/
cp /d/workspace/test1/.emacs.d/IMPROVEMENTS.md ~/.emacs.d/
```

### 3. Create Directory Structure

```bash
# Create note directories
mkdir -p ~/wdata/note/roam/{daily,fleeting,permanent,concepts,methods,questions,references}
mkdir -p ~/wdata/note/org
mkdir -p ~/wdata/note/exports
mkdir -p ~/wdata/note/img

# Create GTD files
touch ~/wdata/note/org/{inbox,gtd,projects,someday,archive,journal}.org
touch ~/wdata/note/roam/index.org
```

### 4. Install Required Packages

Start Emacs and the packages will be installed automatically via straight.el. Or manually:

```elisp
M-x straight-use-package RET org-roam RET
M-x straight-use-package RET org-super-agenda RET
M-x straight-use-package RET citar RET
M-x straight-use-package RET citar-org-roam RET
M-x straight-use-package RET consult-org-roam RET
M-x straight-use-package RET org-roam-ui RET
```

### 5. Optional: Enable AI Features

If you want AI-powered note-taking assistance:

```elisp
;; Add to your init.el after (require 'init-zettelkasten)
(require 'init-org-ai)

;; Set your OpenAI API key (choose one method):

;; Method 1: Direct (not recommended for security)
(setq gptel-api-key "sk-your-api-key-here")

;; Method 2: Environment variable
;; Add to ~/.bashrc or ~/.zshrc:
;; export OPENAI_API_KEY="sk-your-api-key-here"

;; Method 3: Auth-source (most secure)
;; Add to ~/.authinfo.gpg:
;; machine api.openai.com login apikey password sk-your-api-key-here
```

### 6. Optional: Enable Beautiful Appearance

```elisp
;; Already added to init.el, but if you want to disable:
;; Comment out this line in init.el:
;; (require 'init-org-appearance)
```

### 7. Initialize Org-Roam Database

```elisp
M-x org-roam-db-sync
```

## What You Get

### New Files Created

1. **init-zettelkasten.el** - Maintenance and workflow tools
2. **init-org-appearance.el** - Beautiful visual styling
3. **init-org-export.el** - Export to HTML, Markdown, PDF
4. **init-org-ai.el** - AI-powered note assistance (optional)
5. **ZETTELKASTEN_GUIDE.md** - Complete workflow documentation
6. **IMPROVEMENTS.md** - Summary of all improvements

### Enhanced Files

1. **init.el** - Added new module requires
2. **init-org.el** - Better GTD, capture templates, agenda views
3. **init-roam.el** - Enhanced templates, helper functions

## Feature Overview

### Core Zettelkasten (Always Available)

- ✅ 6 note types with structured templates
- ✅ Daily notes with multiple templates
- ✅ Fleeting → Permanent workflow
- ✅ Orphan detection
- ✅ Statistics dashboard
- ✅ Backlink analysis
- ✅ Tag filtering
- ✅ Graph visualization
- ✅ Export with links

### Appearance Enhancements (Optional)

- ✅ org-modern - Clean, modern look
- ✅ org-superstar - Beautiful bullets
- ✅ mixed-pitch - Variable-width fonts
- ✅ org-appear - Show markup on demand
- ✅ visual-fill-column - Centered text
- ✅ Custom heading sizes and colors
- ✅ Better code blocks and tables

### Export Capabilities (Optional)

- ✅ HTML with beautiful CSS
- ✅ GitHub-flavored Markdown
- ✅ PDF via Pandoc
- ✅ Export with backlinks
- ✅ Export note clusters
- ✅ One-key export and open

### AI Features (Optional)

- ✅ Summarize text to atomic notes
- ✅ Expand fleeting notes
- ✅ Suggest related links
- ✅ Generate research questions
- ✅ Critique note quality
- ✅ Auto-suggest tags
- ✅ Find merge candidates
- ✅ Generate literature notes from URLs

## Configuration Options

### Minimal Setup (Core Only)

```elisp
;; In init.el, only require:
(require 'init-org)
(require 'init-roam)
(require 'init-zettelkasten)
```

### Recommended Setup (Core + Appearance)

```elisp
;; In init.el:
(require 'init-org)
(require 'init-org-appearance)
(require 'init-roam)
(require 'init-zettelkasten)
```

### Full Setup (Everything)

```elisp
;; In init.el:
(require 'init-org)
(require 'init-org-appearance)
(require 'init-org-export)
(require 'init-roam)
(require 'init-zettelkasten)
(require 'init-org-ai)  ; Requires API key
```

### Custom Paths

If your notes are not in `~/wdata/note/`, update these variables:

```elisp
;; Add to init.el before requiring modules:
(setq org-roam-directory (file-truename "~/your/path/roam"))
(setq org-agenda-files (list "~/your/path/org/inbox.org"
                             "~/your/path/org/gtd.org"
                             "~/your/path/org/projects.org"))
```

## Testing Your Setup

### 1. Test Basic Functionality

```elisp
;; Create your first note
C-c n c p  ; Create permanent note

;; View statistics
C-c n S

;; Open graph
C-c n g
```

### 2. Test Daily Workflow

```elisp
;; Start daily review
C-c n D

;; Create daily note
C-c n j

;; Capture quick idea
C-c n c f
```

### 3. Test Appearance (if enabled)

Open any org file and check:
- Variable-width fonts for text
- Monospace for code
- Beautiful bullets
- Centered text
- Clean, modern look

### 4. Test Export (if enabled)

```elisp
;; Export current note to HTML
C-c e h

;; Export to Markdown
C-c e m
```

### 5. Test AI Features (if enabled)

```elisp
;; Select some text and summarize
C-c A s

;; Get link suggestions
C-c A l

;; Critique current note
C-c A c
```

## Troubleshooting

### Packages Not Installing

```elisp
;; Manually sync straight.el
M-x straight-pull-all
M-x straight-rebuild-all
```

### Org-Roam Database Issues

```elisp
;; Rebuild database
M-x org-roam-db-sync
M-x org-roam-db-clear-all
M-x org-roam-db-sync
```

### Appearance Not Working

Check if packages are installed:

```elisp
M-x package-list-packages
;; Search for: org-modern, org-superstar, mixed-pitch, org-appear
```

### AI Features Not Working

1. Check API key is set:
   ```elisp
   M-: gptel-api-key
   ```

2. Test connection:
   ```elisp
   M-x gptel
   ;; Type a message and see if it responds
   ```

### Slow Performance

```elisp
;; Disable some appearance features
(setq org-modern-mode nil)
(setq mixed-pitch-mode nil)

;; Or increase GC threshold
(setq gc-cons-threshold (* 256 1024 1024))
```

## Customization Examples

### Change Note Directory Structure

```elisp
;; In init-roam.el, modify capture templates:
(setq org-roam-capture-templates
  '(("p" "permanent" plain
     "%?"
     :if-new (file+head "my-permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+filetags: permanent\n")
     :unnarrowed t)))
```

### Add Custom Note Type

```elisp
;; Add to org-roam-capture-templates in init-roam.el:
("x" "experiment" plain
 "* Hypothesis\n%?\n\n* Method\n\n* Results\n\n* Conclusion\n"
 :if-new (file+head "experiments/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: experiment\n#+date: %<%Y-%m-%d>\n")
 :unnarrowed t)
```

### Change Keybindings

```elisp
;; Add to your init.el or init-roam.el:
(with-eval-after-load 'org-roam
  (define-key org-roam-mode-map (kbd "C-c z") #'org-roam-node-find)
  (define-key org-roam-mode-map (kbd "C-c x") #'org-roam-capture))
```

### Customize Appearance

```elisp
;; In init-org-appearance.el, modify:
(setq org-modern-star 'fold)  ; Different bullet style
(setq visual-fill-column-width 120)  ; Wider text
(setq mixed-pitch-mode nil)  ; Disable variable fonts
```

### Add Custom Export Style

```elisp
;; In init-org-export.el, modify org-html-head:
(setq org-html-head "
<style>
  body {
    font-family: 'Your Font', sans-serif;
    background: #your-color;
  }
</style>")
```

## Next Steps

1. **Read the Guide**: Open `~/.emacs.d/ZETTELKASTEN_GUIDE.md`
2. **Start Small**: Create 5-10 notes to get familiar
3. **Daily Practice**: Use `C-c n D` every day for a week
4. **Explore Graph**: Use `C-c n g` to visualize connections
5. **Process Regularly**: Don't let fleeting notes pile up
6. **Experiment**: Try different note types and templates
7. **Customize**: Adjust to your workflow

## Resources

- **Zettelkasten Method**: https://zettelkasten.de/
- **Org-roam Manual**: https://www.orgroam.com/manual.html
- **Org Mode Guide**: https://orgmode.org/guide/
- **Book**: "How to Take Smart Notes" by Sönke Ahrens

## Support

If you encounter issues:

1. Check `*Messages*` buffer: `C-h e`
2. Check `*Warnings*` buffer
3. Test with minimal config: `emacs -Q`
4. Check package versions: `M-x list-packages`

## Summary

You now have a complete, production-ready Zettelkasten system with:

- **Core**: Structured note-taking with 6 note types
- **Workflow**: Daily/weekly review, processing, quality control
- **Appearance**: Beautiful, distraction-free interface
- **Export**: Share notes in multiple formats
- **AI**: Optional intelligent assistance
- **Documentation**: Complete guides and examples

Start with the basics, then gradually enable more features as you need them.

Happy note-taking! 📝
