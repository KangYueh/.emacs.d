;;; init-org-ai.el --- AI-powered note-taking enhancements -*- lexical-binding: t -*-
;;; Commentary:
;; Optional AI integrations for Zettelkasten workflow
;; Helps with summarization, linking suggestions, and note generation

;;; Code:

;; ──────────────────────────────────────────────
;; 1. GPTel - ChatGPT integration
;; ──────────────────────────────────────────────

(use-package gptel
  :ensure t
  :defer t
  :config
  (setq gptel-model "gpt-4"
        gptel-default-mode 'org-mode)

  ;; Custom directives for Zettelkasten
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (zettel-summarize . "Summarize this text into a concise, atomic note suitable for a Zettelkasten. Focus on the core idea in 2-3 sentences.")
          (zettel-expand . "Expand this fleeting note into a well-structured permanent note. Include: core idea, explanation, examples, and potential connections.")
          (zettel-links . "Suggest 3-5 potential connections or related concepts for this note. Explain why each connection is relevant.")
          (zettel-questions . "Generate 3-5 thought-provoking questions based on this note that could lead to new insights or research directions.")
          (zettel-critique . "Critically analyze this note. Point out: unclear statements, missing context, weak arguments, or areas needing more evidence."))))

;; ──────────────────────────────────────────────
;; 2. Custom AI Helper Functions
;; ──────────────────────────────────────────────

(defun my/ai-summarize-region ()
  "Summarize selected region into Zettelkasten-style note."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gptel-request text
          :system "Summarize this into a concise, atomic note (2-3 sentences). Focus on the core idea."
          :callback
          (lambda (response info)
            (if response
                (progn
                  (goto-char (region-end))
                  (insert "\n\n** AI Summary\n" response "\n"))
              (message "AI request failed")))))
    (message "No region selected")))

(defun my/ai-suggest-links ()
  "Suggest potential links for current note."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (title (org-roam-node-title node))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (all-titles (mapcar #'org-roam-node-title (org-roam-node-list)))
           (prompt (format "Given this note titled '%s', suggest 5 related concepts from this list that would make good connections: %s\n\nNote content:\n%s"
                          title
                          (string-join all-titles ", ")
                          (substring content 0 (min 1000 (length content))))))
      (gptel-request prompt
        :callback
        (lambda (response info)
          (if response
              (with-current-buffer (get-buffer-create "*AI Link Suggestions*")
                (erase-buffer)
                (org-mode)
                (insert "* Link Suggestions for: " title "\n\n")
                (insert response)
                (goto-char (point-min))
                (display-buffer (current-buffer)))
            (message "AI request failed")))))))

(defun my/ai-expand-fleeting-note ()
  "Expand current fleeting note into permanent note with AI assistance."
  (interactive)
  (when (org-roam-buffer-p)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (gptel-request content
        :system "Expand this fleeting note into a well-structured permanent note. Include: 1) Core Idea (2-3 sentences), 2) Detailed Explanation, 3) Examples or Evidence, 4) Potential Connections to other concepts."
        :callback
        (lambda (response info)
          (if response
              (progn
                (goto-char (point-max))
                (insert "\n\n* AI Expansion\n" response "\n")
                (message "AI expansion added. Review and integrate."))
            (message "AI request failed")))))))

(defun my/ai-generate-questions ()
  "Generate research questions based on current note."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (gptel-request content
      :system "Generate 5 thought-provoking questions based on this note. Questions should: 1) Challenge assumptions, 2) Explore implications, 3) Suggest research directions, 4) Connect to broader concepts."
      :callback
      (lambda (response info)
        (if response
            (progn
              (goto-char (point-max))
              (insert "\n\n* Research Questions\n" response "\n")
              (message "Questions generated."))
          (message "AI request failed"))))))

(defun my/ai-critique-note ()
  "Get AI critique of current note quality."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (gptel-request content
      :system "Critically analyze this Zettelkasten note. Evaluate: 1) Clarity - Is the core idea clear? 2) Atomicity - Does it focus on one concept? 3) Completeness - Is context sufficient? 4) Connections - Are links/references adequate? Provide specific, actionable feedback."
      :callback
      (lambda (response info)
        (if response
            (with-current-buffer (get-buffer-create "*AI Note Critique*")
              (erase-buffer)
              (org-mode)
              (insert "* Note Critique\n\n")
              (insert response)
              (goto-char (point-min))
              (display-buffer (current-buffer)))
          (message "AI request failed"))))))

;; ──────────────────────────────────────────────
;; 3. Org-AI (Alternative AI integration)
;; ──────────────────────────────────────────────

(use-package org-ai
  :ensure t
  :defer t
  :commands (org-ai-mode org-ai-global-mode)
  :hook (org-mode . org-ai-mode)
  :config
  (setq org-ai-default-chat-model "gpt-4")
  (org-ai-global-mode))

;; ──────────────────────────────────────────────
;; 4. Automatic Tagging Suggestions
;; ──────────────────────────────────────────────

(defun my/ai-suggest-tags ()
  "Suggest tags for current note based on content."
  (interactive)
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-node-at-point))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (existing-tags (org-roam-node-tags node)))
      (gptel-request content
        :system (format "Suggest 3-5 relevant tags for this note. Current tags: %s. Suggest tags that are: specific, meaningful, and help with categorization. Return only tag names, comma-separated."
                       (if existing-tags (string-join existing-tags ", ") "none"))
        :callback
        (lambda (response info)
          (if response
              (let ((suggested-tags (split-string response "," t "[ \t\n]+")))
                (message "Suggested tags: %s" (string-join suggested-tags ", "))
                (when (y-or-n-p "Add these tags? ")
                  (org-roam-tag-add suggested-tags)
                  (save-buffer)))
            (message "AI request failed")))))))

;; ──────────────────────────────────────────────
;; 5. Smart Note Merging
;; ──────────────────────────────────────────────

(defun my/ai-suggest-merge-candidates ()
  "Find notes that might be duplicates or should be merged."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (titles (mapcar (lambda (n)
                          (cons (org-roam-node-title n)
                                (org-roam-node-id n)))
                        nodes))
         (title-list (mapcar #'car titles)))
    (gptel-request (string-join title-list "\n")
      :system "Analyze these note titles. Identify groups of 2-3 notes that seem to cover similar topics and might benefit from merging. For each group, explain why they're similar. Format: 'Group N: [title1, title2] - Reason'"
      :callback
      (lambda (response info)
        (if response
            (with-current-buffer (get-buffer-create "*AI Merge Suggestions*")
              (erase-buffer)
              (org-mode)
              (insert "* Potential Note Merges\n\n")
              (insert response)
              (insert "\n\n* Instructions\n")
              (insert "Review each suggestion. To merge:\n")
              (insert "1. Open both notes\n")
              (insert "2. Copy content from one to the other\n")
              (insert "3. Update links\n")
              (insert "4. Delete the redundant note\n")
              (goto-char (point-min))
              (display-buffer (current-buffer)))
          (message "AI request failed"))))))

;; ──────────────────────────────────────────────
;; 6. Literature Note Generation
;; ──────────────────────────────────────────────

(defun my/ai-generate-literature-note (url-or-text)
  "Generate literature note from URL or pasted text."
  (interactive "sEnter URL or paste text: ")
  (let ((prompt (if (string-match-p "^https?://" url-or-text)
                    (format "Fetch and summarize this article: %s. Create a literature note with: 1) Summary (3-4 sentences), 2) Key Points (bullet list), 3) Notable Quotes, 4) Personal Reflection prompts" url-or-text)
                  (format "Create a literature note from this text. Include: 1) Summary (3-4 sentences), 2) Key Points (bullet list), 3) Notable Quotes, 4) Personal Reflection prompts\n\nText:\n%s" url-or-text))))
    (gptel-request prompt
      :callback
      (lambda (response info)
        (if response
            (let ((title (read-string "Note title: ")))
              (org-roam-capture- :node (org-roam-node-create :title title)
                                :templates '(("l" "literature" plain
                                             "%?"
                                             :if-new (file+head "references/%<%Y%m%d%H%M%S>-${slug}.org"
                                                               "#+title: ${title}\n#+filetags: literature\n#+date: %<%Y-%m-%d>\n#+source: \n\n")
                                             :unnarrowed t)))
              (insert response)
              (message "Literature note created with AI assistance."))
          (message "AI request failed"))))))

;; ──────────────────────────────────────────────
;; 7. Keybindings
;; ──────────────────────────────────────────────

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c A s") #'my/ai-summarize-region)
  (define-key org-mode-map (kbd "C-c A l") #'my/ai-suggest-links)
  (define-key org-mode-map (kbd "C-c A e") #'my/ai-expand-fleeting-note)
  (define-key org-mode-map (kbd "C-c A q") #'my/ai-generate-questions)
  (define-key org-mode-map (kbd "C-c A c") #'my/ai-critique-note)
  (define-key org-mode-map (kbd "C-c A t") #'my/ai-suggest-tags)
  (define-key org-mode-map (kbd "C-c A m") #'my/ai-suggest-merge-candidates)
  (define-key org-mode-map (kbd "C-c A L") #'my/ai-generate-literature-note))

;; ──────────────────────────────────────────────
;; 8. Configuration Notes
;; ──────────────────────────────────────────────

;; To use these features, you need to:
;; 1. Install gptel: M-x package-install RET gptel RET
;; 2. Set your OpenAI API key:
;;    (setq gptel-api-key "your-api-key-here")
;;    Or better, use auth-source:
;;    Add to ~/.authinfo.gpg:
;;    machine api.openai.com login apikey password YOUR-API-KEY
;;
;; 3. Optionally install org-ai for inline AI chat:
;;    M-x package-install RET org-ai RET

(provide 'init-org-ai)
;;; init-org-ai.el ends here
