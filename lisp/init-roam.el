;;; init-roam.el --- Org-roam optimized for Zettelkasten
;;; Commentary:
;;  C-c n j  → daily fleeting entry
;;  C-c n f  → find/create any node
;;  C-c n L  → open literature note via Citar
;;  C-c n c  → capture (fleeting / concept)
;;  C-c n i  → insert node link inline
;;  C-c n l  → toggle backlink buffer
;;  C-c n r  → extract subtree → new node
;;  C-c n k  → insert citation [cite:@key]
;;  C-c n b  → attach citekey to current node
;;  C-c n s  → full-text ripgrep search

;;; Code:

;; 提升到顶层，消除跨包变量引用的时序问题
(setq org-roam-directory (file-truename "~/wdata/note/roam"))

;; ──────────────────────────────────────────────
;; 1. org-roam core
;; ──────────────────────────────────────────────
(use-package org-roam
  :ensure t
  :defer t
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)

  (require 'org-roam-dailies)

  ;; ── Dailies ──
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* REVIEW %<%H:%M> %?"
	   :if-new (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n\
#+filetags: daily\n"))))

  ;; ── Capture templates ──
  (setq org-roam-capture-templates
	'(("f" "fleeting" plain
	   "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n\
#+filetags: fleeting\n\
#+date: %<%Y-%m-%d>\n")
	   :unnarrowed t)

	  ("c" "concept" plain
	   "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n\
#+filetags: permanent concept\n\
#+date: %<%Y-%m-%d>\n\n\
** Core Idea\n\n\
** Context / Evidence\n\n\
** Links / Backlinks Context\n")
	   :unnarrowed t)))

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n r" . org-roam-extract-subtree)))

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
