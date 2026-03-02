;;; init-org-export.el --- Enhanced org export capabilities -*- lexical-binding: t -*-
;;; Commentary:
;; Export org notes to various formats with beautiful styling
;; Useful for sharing Zettelkasten notes

;;; Code:

;; ──────────────────────────────────────────────
;; 1. HTML Export with Modern Styling
;; ──────────────────────────────────────────────

(use-package htmlize
  :ensure t
  :defer t)

(with-eval-after-load 'ox-html
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-validation-link nil
        org-html-postamble nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-htmlize-output-type 'css
        org-html-head "
<style>
  body {
    max-width: 800px;
    margin: 40px auto;
    padding: 0 20px;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
    font-size: 16px;
    line-height: 1.6;
    color: #333;
    background: #fff;
  }
  h1, h2, h3, h4, h5, h6 {
    font-weight: 600;
    line-height: 1.3;
    margin-top: 1.5em;
    margin-bottom: 0.5em;
  }
  h1 { font-size: 2.2em; border-bottom: 2px solid #eee; padding-bottom: 0.3em; }
  h2 { font-size: 1.8em; border-bottom: 1px solid #eee; padding-bottom: 0.3em; }
  h3 { font-size: 1.5em; }
  h4 { font-size: 1.3em; }
  a { color: #0066cc; text-decoration: none; }
  a:hover { text-decoration: underline; }
  code {
    background: #f5f5f5;
    padding: 2px 6px;
    border-radius: 3px;
    font-family: 'SF Mono', Monaco, 'Cascadia Code', 'Roboto Mono', Consolas, monospace;
    font-size: 0.9em;
  }
  pre {
    background: #f5f5f5;
    padding: 16px;
    border-radius: 6px;
    overflow-x: auto;
    border-left: 3px solid #0066cc;
  }
  pre code {
    background: none;
    padding: 0;
  }
  blockquote {
    border-left: 4px solid #ddd;
    padding-left: 20px;
    margin-left: 0;
    color: #666;
    font-style: italic;
  }
  table {
    border-collapse: collapse;
    width: 100%;
    margin: 1em 0;
  }
  th, td {
    border: 1px solid #ddd;
    padding: 12px;
    text-align: left;
  }
  th {
    background: #f5f5f5;
    font-weight: 600;
  }
  img {
    max-width: 100%;
    height: auto;
    border-radius: 6px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }
  .tag {
    background: #e3f2fd;
    color: #1976d2;
    padding: 2px 8px;
    border-radius: 3px;
    font-size: 0.85em;
    margin-left: 8px;
  }
  .timestamp {
    color: #999;
    font-size: 0.9em;
  }
  .todo { color: #f44336; font-weight: 600; }
  .done { color: #4caf50; font-weight: 600; }
  .priority { color: #ff9800; font-weight: 600; }
  @media (prefers-color-scheme: dark) {
    body { background: #1e1e1e; color: #d4d4d4; }
    h1, h2 { border-color: #333; }
    code, pre { background: #2d2d2d; }
    pre { border-left-color: #4fc3f7; }
    blockquote { border-left-color: #555; color: #aaa; }
    th, td { border-color: #444; }
    th { background: #2d2d2d; }
    a { color: #4fc3f7; }
  }
</style>"))

;; ──────────────────────────────────────────────
;; 2. Markdown Export
;; ──────────────────────────────────────────────

(use-package ox-gfm
  :ensure t
  :after ox
  :config
  (require 'ox-gfm))

;; ──────────────────────────────────────────────
;; 3. Pandoc Integration
;; ──────────────────────────────────────────────

(use-package ox-pandoc
  :ensure t
  :after ox
  :config
  (setq org-pandoc-options-for-docx '((standalone . t)))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex"))))

;; ──────────────────────────────────────────────
;; 4. Export Dispatcher Enhancements
;; ──────────────────────────────────────────────

(with-eval-after-load 'ox
  (setq org-export-with-toc t
        org-export-with-tags t
        org-export-with-author t
        org-export-with-creator nil
        org-export-with-date t
        org-export-with-email nil
        org-export-with-timestamps t
        org-export-with-todo-keywords t
        org-export-with-broken-links 'mark
        org-export-headline-levels 4))

;; ──────────────────────────────────────────────
;; 5. Custom Export Functions
;; ──────────────────────────────────────────────

(defun my/org-export-to-html-and-open ()
  "Export current org file to HTML and open in browser."
  (interactive)
  (let ((output-file (org-html-export-to-html)))
    (browse-url output-file)
    (message "Exported to: %s" output-file)))

(defun my/org-export-subtree-to-html ()
  "Export current subtree to HTML file."
  (interactive)
  (org-narrow-to-subtree)
  (let ((output-file (org-html-export-to-html nil t)))
    (widen)
    (message "Exported subtree to: %s" output-file)))

(defun my/org-export-to-markdown ()
  "Export current org file to GitHub-flavored markdown."
  (interactive)
  (if (fboundp 'org-gfm-export-to-markdown)
      (let ((output-file (org-gfm-export-to-markdown)))
        (message "Exported to: %s" output-file))
    (message "ox-gfm not available. Install it first.")))

(defun my/org-export-roam-note-with-backlinks ()
  "Export current roam note with backlinks section."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (backlinks (org-roam-backlinks-get node))
           (export-file (concat (file-name-sans-extension (buffer-file-name))
                               "-export.org")))
      (with-temp-file export-file
        (insert-buffer-substring (current-buffer))
        (goto-char (point-max))
        (insert "\n\n* Backlinks\n\n")
        (if backlinks
            (dolist (backlink backlinks)
              (let* ((source-node (org-roam-backlink-source-node backlink))
                     (title (org-roam-node-title source-node))
                     (id (org-roam-node-id source-node)))
                (insert (format "- [[id:%s][%s]]\n" id title))))
          (insert "No backlinks.\n")))
      (find-file export-file)
      (message "Created export file with backlinks: %s" export-file))))

(defun my/org-export-zettelkasten-cluster ()
  "Export current note and all directly linked notes."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (title (org-roam-node-title node))
           (links (org-roam-node-links node))
           (backlinks (org-roam-backlinks-get node))
           (export-dir (concat "~/wdata/note/exports/"
                              (format-time-string "%Y%m%d-")
                              (org-roam-node-slug node)))
           (index-file (concat export-dir "/index.html")))

      ;; Create export directory
      (unless (file-directory-p export-dir)
        (make-directory export-dir t))

      ;; Export main note
      (let ((org-export-output-file-name (concat export-dir "/main.html")))
        (org-html-export-to-html))

      ;; Export linked notes
      (dolist (link links)
        (let* ((linked-node (org-roam-node-from-id (car link)))
               (linked-file (org-roam-node-file linked-node)))
          (when (and linked-node (file-exists-p linked-file))
            (with-current-buffer (find-file-noselect linked-file)
              (let ((org-export-output-file-name
                     (concat export-dir "/"
                            (org-roam-node-slug linked-node) ".html")))
                (org-html-export-to-html))))))

      ;; Create index
      (with-temp-file index-file
        (insert "<!DOCTYPE html>\n<html>\n<head>\n")
        (insert "<meta charset=\"utf-8\">\n")
        (insert (format "<title>%s - Cluster Export</title>\n" title))
        (insert "</head>\n<body>\n")
        (insert (format "<h1>%s</h1>\n" title))
        (insert "<h2>Main Note</h2>\n")
        (insert "<p><a href=\"main.html\">View Main Note</a></p>\n")
        (insert "<h2>Linked Notes</h2>\n<ul>\n")
        (dolist (link links)
          (let ((linked-node (org-roam-node-from-id (car link))))
            (when linked-node
              (insert (format "<li><a href=\"%s.html\">%s</a></li>\n"
                            (org-roam-node-slug linked-node)
                            (org-roam-node-title linked-node))))))
        (insert "</ul>\n</body>\n</html>\n"))

      (browse-url index-file)
      (message "Exported cluster to: %s" export-dir))))

;; ──────────────────────────────────────────────
;; 6. PDF Export Configuration
;; ──────────────────────────────────────────────

(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "xelatex"
        org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))

  ;; Add custom LaTeX classes
  (add-to-list 'org-latex-classes
               '("article-modern"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{geometry}
\\geometry{margin=1in}
\\usepackage{parskip}
\\setlength{\\parindent}{0pt}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; ──────────────────────────────────────────────
;; 7. Keybindings
;; ──────────────────────────────────────────────

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e h") #'my/org-export-to-html-and-open)
  (define-key org-mode-map (kbd "C-c e s") #'my/org-export-subtree-to-html)
  (define-key org-mode-map (kbd "C-c e m") #'my/org-export-to-markdown)
  (define-key org-mode-map (kbd "C-c e b") #'my/org-export-roam-note-with-backlinks)
  (define-key org-mode-map (kbd "C-c e c") #'my/org-export-zettelkasten-cluster))

(provide 'init-org-export)
;;; init-org-export.el ends here
