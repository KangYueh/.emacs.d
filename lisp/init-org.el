;; init-org.el - Org-mode with GTD + Zettelkasten processing support
(add-hook 'org-mode-hook #'visual-line-mode)
(setq word-wrap t)

(use-package org
  :ensure t
  :defer t
  :config
  ;; habit config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 55
	org-habit-preceding-days 21
	org-habit-following-days 7
	org-habit-show-habits-only-for-today nil)
  ;; Modern, clean look
  (setq org-hide-emphasis-markers t
	org-startup-indented t
	org-ellipsis "…"
	org-return-follows-link t
	org-src-fontify-natively t
	org-fontify-quote-and-verse-blocks t
        org-startup-with-inline-images t
        org-image-actual-width '(600))

  ;; Todo keywords: GTD + REVIEW for processing fleeting/literature to permanent
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "REVIEW(r)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  ;; Agenda: scan main files + roam directory (to see roam TODO/REVIEW)
  (setq org-agenda-files (list "~/wdata/note/org/inbox.org"
			       "~/wdata/note/org/gtd.org"
			       "~/wdata/note/org/projects.org"
			       (expand-file-name "~/wdata/note/roam/daily/")
			       (expand-file-name "~/wdata/note/roam/fleeting/")))

  ;; Refile targets: easy to move fleeting → permanent or hub
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)
			     ("~/wdata/note/roam/index.org" :maxlevel . 2)
                             ("~/wdata/note/org/someday.org" :maxlevel . 2))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; Archive location
  (setq org-archive-location "~/wdata/note/org/archive.org::* From %s")

  ;; Capture templates: GTD + Zettelkasten integration
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/wdata/note/org/inbox.org")
	   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a" :empty-lines 1)

          ("n" "Quick Note" entry (file "~/wdata/note/org/inbox.org")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a" :empty-lines 1)

          ("p" "Project" entry (file "~/wdata/note/org/projects.org")
	   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Purpose\n\n** Outcome\n\n** NEXT First action\n" :empty-lines 1)

          ("m" "Meeting" entry (file "~/wdata/note/org/inbox.org")
           "* Meeting: %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Attendees\n\n** Agenda\n\n** Notes\n\n** Action Items\n" :empty-lines 1)

          ("j" "Journal" entry (file+datetree "~/wdata/note/org/journal.org")
           "* %<%H:%M> %?\n" :empty-lines 1)

          ("r" "Reading/Research" entry (file "~/wdata/note/org/inbox.org")
           "* REVIEW [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %:link\n:END:\n\n%i\n%?" :empty-lines 1)))

  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)))

;; Super agenda for better overview (modern GTD view)
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
	'((:name "⚡ Overdue"
           :deadline past
           :order 1)
          (:name "📅 Today"
           :time-grid t
           :date today
           :scheduled today
           :order 2)
          (:name "🔥 Important & Urgent"
           :and (:priority "A" :todo "TODO")
           :order 3)
          (:name "📝 Process / Review"
           :todo "REVIEW"
           :order 4)
          (:name "➡️  Next Actions"
           :todo "NEXT"
           :order 5)
          (:name "⏳ Waiting"
           :todo "WAIT"
           :order 6)
          (:name "📚 Reading/Research"
           :tag "reading"
           :order 7)
          (:name "📊 Projects"
           :tag "project"
           :order 8)
          (:name "📥 Inbox"
           :file-path "inbox.org"
           :order 9)
          (:discard (:anything t))))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)))))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "📝 To Process")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "➡️  Next Actions")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "⏳ Waiting On")))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "TODO"
                  ((org-agenda-overriding-header "📋 All TODOs")))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "📝 To Process")))))

          ("r" "Reading & Research"
           ((tags-todo "reading"
                       ((org-agenda-overriding-header "📚 Reading List")))
            (tags-todo "research"
                       ((org-agenda-overriding-header "🔬 Research Tasks")))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "📝 To Process")))))

          ("p" "Projects"
           ((tags "project"
                  ((org-agenda-overriding-header "📊 Active Projects")
                   (org-tags-match-list-sublevels t))))))))

;; ──────────────────────────────────────────────
;; Helper functions for Zettelkasten workflow
;; ──────────────────────────────────────────────

(defvar my/org-image-dir (expand-file-name "~/wdata/note/img/")
  "Absolute path to store org images.")

(defun my/org-screenshot ()
  "Take a screenshot, save it to an absolute image directory,
and insert the image link at point in the current org buffer."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))

  ;; ensure image directory exists
  (unless (file-directory-p my/org-image-dir)
    (make-directory my/org-image-dir t))

  (let* ((filename (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))
	 (filepath (expand-file-name filename my/org-image-dir))
	 (status
	 (cond
	 ((and (getenv "WAYLAND_DISPLAY")
		(executable-find "grim")
		(executable-find "slurp"))
	 (let* ((region-output (shell-command-to-string "slurp"))
		 (region (car (split-string region-output "\n" t))))
	 (if region
		 (call-process "grim" nil nil nil "-g" region filepath)
	1)))
	 ((executable-find "scrot")
	 (call-process "scrot" nil nil nil "-s" filepath))
	 ((executable-find "maim")
	 (call-process "maim" nil nil nil "-s" filepath))
	 ((executable-find "import")
	 (call-process "import" nil nil nil filepath))
	 (t
	 (user-error "No screenshot tool found. Install grim+slurp or scrot/maim/import")))))

    ;; minimize Emacs
    (iconify-frame frame)
    ;; small delay to ensure window manager reacts
    (sleep-for 0.3)
    ;; call scrot with selection
    (call-process "scrot" nil nil nil "-s" filepath)

    ;; insert absolute-path org link
    (insert (format "[[file:%s]]" filepath))
    (newline)

    ;; display inline image
    (org-display-inline-images)))

(defun my/org-insert-created-timestamp ()
  "Insert CREATED property with current timestamp."
  (interactive)
  (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))

(defun my/org-process-inbox ()
  "Process inbox items - review and refile or convert to roam notes."
  (interactive)
  (find-file "~/wdata/note/org/inbox.org")
  (goto-char (point-min))
  (org-next-visible-heading 1))

(defun my/org-weekly-review ()
  "Start weekly review process."
  (interactive)
  (org-agenda nil "w"))

(defun my/org-link-to-roam-node ()
  "Convert current heading to org-roam node and replace with link."
  (interactive)
  (let ((title (org-get-heading t t t t))
        (content (org-get-entry)))
    (org-roam-capture- :node (org-roam-node-create :title title)
                       :props '(:immediate-finish t))
    (insert content)
    (org-back-to-heading)
    (org-cut-subtree)
    (org-roam-node-insert)))

;; Auto-add CREATED property to new headings
(defun my/org-add-created-property ()
  "Add CREATED property if not exists."
  (when (and (eq major-mode 'org-mode)
             (org-at-heading-p)
             (not (org-entry-get nil "CREATED")))
    (my/org-insert-created-timestamp)))

(add-hook 'org-insert-heading-hook #'my/org-add-created-property)

;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n s") #'my/org-screenshot)
  (define-key org-mode-map (kbd "C-c w r") #'my/org-weekly-review)
  (define-key org-mode-map (kbd "C-c w p") #'my/org-process-inbox)
  (define-key org-mode-map (kbd "C-c w l") #'my/org-link-to-roam-node))

;; for the font scaling in latex preview
(with-eval-after-load 'org
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))) ; 将 1.5 改为你喜欢的倍数

(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvisvgm
         :programs ("xelatex" "dvisvgm")
         :description "xdv > svg"
         :message "rendering via xelatex + dvisvgm"
         :image-input-type "xdv"
         :image-output-type "svg"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
         ;; 关键修改：去掉 --zoom=%Z，改用固定缩放或让 dvisvgm 默认处理
         :image-converter ("dvisvgm %f --no-fonts --exact-bbox --zoom=1.5 -p 1 -o %O"))))
(provide 'init-org)
