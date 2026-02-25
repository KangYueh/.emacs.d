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
	org-fontify-quote-and-verse-blocks t)

  ;; Todo keywords: GTD + REVIEW for processing fleeting/literature to permanent
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-log-done 'time)

  ;; Agenda: scan main files + roam directory (to see roam TODO/REVIEW)
  (setq org-agenda-files (list "~/wdata/note/org/inbox.org"
			       "~/wdata/note/org/gtd.org"
			       "~/wdata/note/org/projects.org"
			       (expand-file-name "~/wdata/note/roam/daily/")
				(expand-file-name "~/wdata/note/roam/fleeting/")))  ; roam notes can have TODO or REVIEW

  ;; Refile targets: easy to move fleeting → permanent or hub
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)
			     ("~/wdata/note/roam/index.org" :maxlevel . 2))  ; optional hub file
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)

  ;; Capture templates: GTD + fleeting entry point
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/wdata/note/org/inbox.org")
	   "* TODO %?\n%U\n%i" :empty-lines 1)
	  ("f" "Fleeting Note" entry (file+headline "~/wdata/note/org/inbox.org" "Fleeting Ideas")
	   "* REVIEW %?\n%U\n%i\nCaptured in: %a" :empty-lines 1)
	  ("n" "Quick Note" entry (file "~/wdata/note/org/inbox.org")
	   "* %?\n%U\n%i" :empty-lines 1)
	  ("p" "Project" entry (file "~/wdata/note/org/projects.org")
	   "* TODO %?\n%U\n%i\n** NEXT First action" :empty-lines 1)))

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
	'((:name "Process / Review" :todo "REVIEW")
	  (:name "Today" :time-grid t :date today :scheduled today)
	  (:name "Important" :priority "A")
	  (:name "Next Actions" :todo "NEXT")
	  (:discard (:anything t)))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n s") #'my/org-screenshot))

(defvar my/org-image-dir (expand-file-name "~/wdata/note/img/")
  "Absolute path to store org images.")

(defun my/org-screenshot ()
  "Take a screenshot with scrot, save it to an absolute image directory,
and insert the image link at point in the current org buffer."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))

  ;; ensure image directory exists
  (unless (file-directory-p my/org-image-dir)
    (make-directory my/org-image-dir t))

 (let* ((filename (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))
	 (filepath (expand-file-name filename my/org-image-dir))
	 (frame (selected-frame)))

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
(provide 'init-org)
