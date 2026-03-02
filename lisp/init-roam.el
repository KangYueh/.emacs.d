;;; init-roam.el --- Org-roam optimized for Zettelkasten
;;; Commentary:
;;  C-c n j  → daily fleeting entry
;;  C-c n f  → find/create any node
;;  C-c n L  → open literature note via Citar
;;  C-c n c  → capture (fleeting / concept / literature)
;;  C-c n i  → insert node link inline
;;  C-c n l  → toggle backlink buffer
;;  C-c n r  → extract subtree → new node
;;  C-c n k  → insert citation [cite:@key]
;;  C-c n b  → attach citekey to current node
;;  C-c n s  → full-text ripgrep search
;;  C-c n t  → add tag to current node
;;  C-c n a  → show node aliases
;;  C-c n g  → open org-roam-ui graph

;;; Code:

;; 提升到顶层，消除跨包变量引用的时序问题
(setq org-roam-directory (file-truename "~/wdata/note/roam"))

;; ──────────────────────────────────────────────
;; 0. Helper functions for Zettelkasten workflow
;; ──────────────────────────────────────────────

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Insert node without opening it. Useful for quick linking."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                   '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  "Filter org-roam nodes by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag)
  "List all notes with TAG."
  (interactive
   (list (completing-read "Tag: " (org-roam-tag-completions))))
  (org-roam-node-find nil nil (my/org-roam-filter-by-tag tag)))

(defun my/org-roam-refresh-agenda-list ()
  "Refresh org-agenda-files with all roam files containing TODO."
  (interactive)
  (setq org-agenda-files
        (append (list "~/wdata/note/org/inbox.org"
                      "~/wdata/note/org/gtd.org"
                      "~/wdata/note/org/projects.org")
                (directory-files-recursively org-roam-directory "\\.org$"))))

(defun my/org-roam-capture-inbox ()
  "Capture to inbox.org for quick processing."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?\n"
                                   :if-new (file+head "inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-copy-todo-to-today ()
  "Copy current TODO heading to today's daily note."
  (interactive)
  (let ((heading (org-get-heading t t t t)))
    (org-roam-dailies-capture-today nil "t")
    (insert heading)))

(defun my/org-roam-node-from-cite (keys-entries)
  "Create literature note from citation."
  (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
  (let ((title (citar-format--entry
                "${author} (${year}): ${title}"
                (cdr keys-entries))))
    (org-roam-capture- :node (org-roam-node-create :title title)
                       :info (list :ref (car keys-entries))
                       :templates '(("l" "literature" plain
                                     "%?"
                                     :if-new (file+head "references/${citekey}.org"
                                                        "#+title: ${title}\n#+filetags: literature\n#+date: %<%Y-%m-%d>\n#+ref: @${citekey}\n\n* Summary\n\n* Key Points\n\n* Personal Notes\n\n* Related Notes\n")
                                     :unnarrowed t)))))

;; ──────────────────────────────────────────────
;; 1. org-roam core
;; ──────────────────────────────────────────────
(use-package org-roam
  :ensure t
  :defer t
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t
        org-roam-v2-ack t
        org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

  (require 'org-roam-dailies)

  ;; ── Dailies ──
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* %<%H:%M> %?"
	   :if-new (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d %A>\n#+filetags: daily\n\n* Morning Review\n\n* Notes\n\n* Evening Reflection\n"))
          ("t" "task" entry
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: daily\n"))
          ("m" "meeting" entry
           "* %<%H:%M> Meeting: %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Attendees\n\n** Notes\n\n** Action Items\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: daily\n"))))

  ;; ── Capture templates (Enhanced Zettelkasten) ──
  (setq org-roam-capture-templates
	'(("f" "fleeting" plain
	   "* Quick Capture\n%?\n\n* Source\n%a\n\n* Tags\n"
	   :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: fleeting\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
	   :unnarrowed t)

	  ("p" "permanent" plain
	   "* Core Idea\n%?\n\n* Explanation\n\n* Examples\n\n* Connections\n\n* References\n"
	   :if-new (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: permanent\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
	   :unnarrowed t)

	  ("c" "concept" plain
	   "* Definition\n%?\n\n* Key Properties\n\n* Related Concepts\n\n* Applications\n\n* Questions\n"
	   :if-new (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: permanent concept\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
	   :unnarrowed t)

          ("l" "literature" plain
           "* Summary\n%?\n\n* Key Points\n\n* Personal Notes\n\n* Quotes\n\n* Related Notes\n"
           :if-new (file+head "references/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: literature\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
           :unnarrowed t)

          ("m" "method/technique" plain
           "* Overview\n%?\n\n* Steps/Process\n\n* When to Use\n\n* Pros & Cons\n\n* Examples\n\n* Related Methods\n"
           :if-new (file+head "methods/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: permanent method\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
           :unnarrowed t)

          ("q" "question" plain
           "* Question\n%?\n\n* Current Understanding\n\n* Research Notes\n\n* Potential Answers\n\n* Related Questions\n"
           :if-new (file+head "questions/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: question\n#+date: %<%Y-%m-%d>\n#+created: %U\n")
           :unnarrowed t)))

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n I" . my/org-roam-node-insert-immediate)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n r" . org-roam-extract-subtree)
   ("C-c n t" . my/org-roam-list-notes-by-tag)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n g" . org-roam-ui-open)))

;; ──────────────────────────────────────────────
;; 2. Citar
;; ──────────────────────────────────────────────
(use-package citar
  :ensure t
  :after org                            ; ✅ 不依赖 :defer 的 org-roam
  :config
  (setq citar-bibliography             '("~/wdata/zotero/all.bib")
	citar-notes-paths              '("~/wdata/note/roam/references") ; ✅ 硬编码避免时序问题
	org-cite-global-bibliography   citar-bibliography
	org-cite-insert-processor      'citar
	org-cite-follow-processor      'citar
	org-cite-activate-processor    'citar)
  :bind
  (("C-c n k" . citar-insert-citation)))

;; ──────────────────────────────────────────────
;; 3. citar-org-roam
;; ──────────────────────────────────────────────
(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} (${year}): ${title}"
	citar-org-roam-capture-template-keys '("l"))
  :bind
  (("C-c n L" . citar-open-notes)
   ("C-c n b" . citar-org-roam-ref-add)))

;; ──────────────────────────────────────────────
;; 4. consult-org-roam
;; ──────────────────────────────────────────────
(use-package consult-org-roam
  :ensure t
  :after (consult org-roam)
  :init
  (consult-org-roam-mode 1)
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  :bind
  (("C-c n S" . consult-org-roam-search-node))) ; ✅ 正确函数名

;; ──────────────────────────────────────────────
;; 5. org-roam-ui
;; ──────────────────────────────────────────────
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme     t
	org-roam-ui-follow         t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start  nil))

(provide 'init-roam)
;;; init-roam.el ends here
