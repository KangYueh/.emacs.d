;;; init-zettelkasten.el --- Enhanced Zettelkasten workflow utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Additional utilities for Zettelkasten method beyond org-roam basics
;; Focuses on: linking, processing, reviewing, and maintaining notes

;;; Code:

;; ──────────────────────────────────────────────
;; 1. Note Statistics & Health
;; ──────────────────────────────────────────────

(defun my/zettel-stats ()
  "Show statistics about your Zettelkasten."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (total (length nodes))
         (fleeting (length (seq-filter
                           (lambda (n) (member "fleeting" (org-roam-node-tags n)))
                           nodes)))
         (permanent (length (seq-filter
                            (lambda (n) (member "permanent" (org-roam-node-tags n)))
                            nodes)))
         (literature (length (seq-filter
                             (lambda (n) (member "literature" (org-roam-node-tags n)))
                             nodes)))
         (orphans (length (seq-filter
                          (lambda (n) (= 0 (length (org-roam-backlinks-get n))))
                          nodes))))
    (message "Zettelkasten Stats:\nTotal: %d | Fleeting: %d | Permanent: %d | Literature: %d | Orphans: %d"
             total fleeting permanent literature orphans)))

(defun my/zettel-find-orphans ()
  "Find notes with no backlinks (potential orphans)."
  (interactive)
  (let ((orphans (seq-filter
                  (lambda (n)
                    (and (= 0 (length (org-roam-backlinks-get n)))
                         (not (member "daily" (org-roam-node-tags n)))))
                  (org-roam-node-list))))
    (if orphans
        (org-roam-node-find nil nil
                           (lambda (node)
                             (member node orphans)))
      (message "No orphan notes found!"))))

(defun my/zettel-find-unlinked ()
  "Find notes that don't link to any other notes."
  (interactive)
  (let ((unlinked (seq-filter
                   (lambda (n)
                     (with-current-buffer (find-file-noselect (org-roam-node-file n))
                       (goto-char (point-min))
                       (not (re-search-forward org-link-bracket-re nil t))))
                   (org-roam-node-list))))
    (if unlinked
        (org-roam-node-find nil nil
                           (lambda (node)
                             (member node unlinked)))
      (message "All notes have links!"))))

;; ──────────────────────────────────────────────
;; 2. Processing Workflow
;; ──────────────────────────────────────────────

(defun my/zettel-process-fleeting ()
  "Review fleeting notes and convert to permanent."
  (interactive)
  (let ((fleeting-notes (seq-filter
                         (lambda (n) (member "fleeting" (org-roam-node-tags n)))
                         (org-roam-node-list))))
    (if fleeting-notes
        (progn
          (org-roam-node-find nil nil
                             (lambda (node)
                               (member "fleeting" (org-roam-node-tags node))))
          (message "Processing fleeting notes. Use C-c n p to convert to permanent."))
      (message "No fleeting notes to process!"))))

(defun my/zettel-convert-to-permanent ()
  "Convert current fleeting note to permanent note."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (tags (org-roam-node-tags node)))
      (if (member "fleeting" tags)
          (progn
            (org-roam-tag-remove '("fleeting"))
            (org-roam-tag-add '("permanent"))
            (save-buffer)
            (message "Converted to permanent note!"))
        (message "This is not a fleeting note.")))))

(defun my/zettel-review-old-notes ()
  "Find notes older than 30 days that haven't been modified recently."
  (interactive)
  (let* ((cutoff-time (time-subtract (current-time) (days-to-time 30)))
         (old-notes (seq-filter
                     (lambda (n)
                       (let ((mtime (file-attribute-modification-time
                                    (file-attributes (org-roam-node-file n)))))
                         (time-less-p mtime cutoff-time)))
                     (org-roam-node-list))))
    (if old-notes
        (org-roam-node-find nil nil
                           (lambda (node)
                             (member node old-notes)))
      (message "No old notes found!"))))

;; ──────────────────────────────────────────────
;; 3. Smart Linking
;; ──────────────────────────────────────────────

(defun my/zettel-insert-link-with-context ()
  "Insert link and add context about why you're linking."
  (interactive)
  (org-roam-node-insert)
  (insert " - ")
  (save-excursion
    (insert "\n  Context: ")))

(defun my/zettel-backlink-context ()
  "Show context around backlinks for current note."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (backlinks (org-roam-backlinks-get node)))
      (if backlinks
          (progn
            (switch-to-buffer-other-window "*Backlink Context*")
            (erase-buffer)
            (org-mode)
            (insert (format "* Backlinks to: %s\n\n" (org-roam-node-title node)))
            (dolist (backlink backlinks)
              (let* ((source-node (org-roam-backlink-source-node backlink))
                     (point (org-roam-backlink-point backlink)))
                (insert (format "** From: [[id:%s][%s]]\n"
                              (org-roam-node-id source-node)
                              (org-roam-node-title source-node)))
                (insert "Context:\n#+begin_quote\n")
                (with-current-buffer (find-file-noselect (org-roam-node-file source-node))
                  (save-excursion
                    (goto-char point)
                    (let ((start (line-beginning-position))
                          (end (line-end-position)))
                      (insert (buffer-substring-no-properties start end)))))
                (insert "\n#+end_quote\n\n")))
            (goto-char (point-min)))
        (message "No backlinks found.")))))

;; ──────────────────────────────────────────────
;; 4. Templates & Snippets
;; ──────────────────────────────────────────────

(defun my/zettel-insert-question-template ()
  "Insert question template in current note."
  (interactive)
  (insert "* Question\n\n")
  (insert "* Current Understanding\n\n")
  (insert "* Research Notes\n\n")
  (insert "* Answer\n\n")
  (insert "* Related Questions\n"))

(defun my/zettel-insert-comparison-template ()
  "Insert comparison template for comparing concepts."
  (interactive)
  (let ((concept-a (read-string "Concept A: "))
        (concept-b (read-string "Concept B: ")))
    (insert (format "* Comparing: %s vs %s\n\n" concept-a concept-b))
    (insert (format "** %s\n\n" concept-a))
    (insert (format "** %s\n\n" concept-b))
    (insert "** Similarities\n\n")
    (insert "** Differences\n\n")
    (insert "** When to Use Each\n\n")))

(defun my/zettel-insert-source-template ()
  "Insert source/reference template."
  (interactive)
  (insert "* Source Information\n")
  (insert "- Author: \n")
  (insert "- Title: \n")
  (insert "- Year: \n")
  (insert "- URL: \n\n")
  (insert "* Key Takeaways\n\n")
  (insert "* Quotes\n\n")
  (insert "* My Thoughts\n\n"))

;; ──────────────────────────────────────────────
;; 5. Daily Review Workflow
;; ──────────────────────────────────────────────

(defun my/zettel-daily-review ()
  "Start daily Zettelkasten review workflow."
  (interactive)
  (let ((review-buffer (get-buffer-create "*Zettelkasten Daily Review*")))
    (switch-to-buffer review-buffer)
    (erase-buffer)
    (org-mode)
    (insert "* Zettelkasten Daily Review\n\n")
    (insert (format-time-string "Date: %Y-%m-%d %A\n\n"))

    ;; Stats
    (insert "** Statistics\n")
    (let* ((nodes (org-roam-node-list))
           (total (length nodes))
           (fleeting (length (seq-filter
                             (lambda (n) (member "fleeting" (org-roam-node-tags n)))
                             nodes))))
      (insert (format "- Total notes: %d\n" total))
      (insert (format "- Fleeting notes to process: %d\n\n" fleeting)))

    ;; Tasks
    (insert "** Today's Tasks\n")
    (insert "- [ ] Process fleeting notes (C-c n P)\n")
    (insert "- [ ] Review orphan notes (C-c n O)\n")
    (insert "- [ ] Add 1-3 new permanent notes\n")
    (insert "- [ ] Review and strengthen 2-3 existing notes\n\n")

    ;; Quick links
    (insert "** Quick Actions\n")
    (insert "- [[elisp:(my/zettel-process-fleeting)][Process Fleeting Notes]]\n")
    (insert "- [[elisp:(my/zettel-find-orphans)][Find Orphans]]\n")
    (insert "- [[elisp:(my/zettel-stats)][Show Stats]]\n")
    (insert "- [[elisp:(org-roam-node-find)][Find Note]]\n")
    (insert "- [[elisp:(org-roam-capture)][New Note]]\n\n")

    (goto-char (point-min))))

;; ──────────────────────────────────────────────
;; 6. Export & Sharing
;; ──────────────────────────────────────────────

(defun my/zettel-export-note-with-links ()
  "Export current note with all linked notes to a single file."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (title (org-roam-node-title node))
           (export-file (concat "~/wdata/note/exports/"
                               (format-time-string "%Y%m%d-")
                               (org-roam-node-slug node)
                               ".org"))
           (links (org-roam-node-links node)))
      (with-temp-file export-file
        (insert (format "#+title: %s (with linked notes)\n" title))
        (insert (format "#+date: %s\n\n" (format-time-string "%Y-%m-%d")))
        (insert "* Main Note\n\n")
        (insert-file-contents (org-roam-node-file node))
        (insert "\n\n* Linked Notes\n\n")
        (dolist (link links)
          (let ((linked-node (org-roam-node-from-id (car link))))
            (when linked-node
              (insert (format "** %s\n\n" (org-roam-node-title linked-node)))
              (insert-file-contents (org-roam-node-file linked-node))
              (insert "\n\n")))))
      (message "Exported to: %s" export-file)
      (find-file export-file))))

;; ──────────────────────────────────────────────
;; 7. Keybindings
;; ──────────────────────────────────────────────

(with-eval-after-load 'org-roam
  (define-key org-roam-mode-map (kbd "C-c n S") #'my/zettel-stats)
  (define-key org-roam-mode-map (kbd "C-c n O") #'my/zettel-find-orphans)
  (define-key org-roam-mode-map (kbd "C-c n U") #'my/zettel-find-unlinked)
  (define-key org-roam-mode-map (kbd "C-c n P") #'my/zettel-process-fleeting)
  (define-key org-roam-mode-map (kbd "C-c n p") #'my/zettel-convert-to-permanent)
  (define-key org-roam-mode-map (kbd "C-c n R") #'my/zettel-review-old-notes)
  (define-key org-roam-mode-map (kbd "C-c n C") #'my/zettel-backlink-context)
  (define-key org-roam-mode-map (kbd "C-c n D") #'my/zettel-daily-review)
  (define-key org-roam-mode-map (kbd "C-c n E") #'my/zettel-export-note-with-links))

(provide 'init-zettelkasten)
;;; init-zettelkasten.el ends here
