;;; init-org-appearance.el --- Beautiful org-mode appearance -*- lexical-binding: t -*-
;;; Commentary:
;; Enhanced visual appearance for org-mode and org-roam
;; Makes reading and writing notes more pleasant

;;; Code:

;; ──────────────────────────────────────────────
;; 1. Org Modern - Clean, modern look
;; ──────────────────────────────────────────────

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil)
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-todo t)
  (org-modern-timestamp t)
  (org-modern-statistics t)
  (org-modern-progress t)
  (org-modern-horizontal-rule t)
  (org-modern-star 'replace)
  (org-modern-hide-stars t)
  (org-modern-list '((?+ . "◦") (?- . "•") (?* . "▸"))))

;; ──────────────────────────────────────────────
;; 2. Org Superstar - Better heading bullets
;; ──────────────────────────────────────────────

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist
   '(("TODO" . ?☐)
     ("NEXT" . ?▶)
     ("WAIT" . ?⏸)
     ("REVIEW" . ?📝)
     ("DONE" . ?✓)
     ("CANCELLED" . ?✗))))

;; ──────────────────────────────────────────────
;; 3. Variable Pitch Mode - Mixed fonts
;; ──────────────────────────────────────────────

(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  ;; Keep code blocks, tables, and tags in monospace
  (dolist (face '(org-code
                  org-block
                  org-table
                  org-verbatim
                  org-special-keyword
                  org-meta-line
                  org-checkbox
                  org-tag
                  org-date
                  org-property-value
                  org-drawer))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; ──────────────────────────────────────────────
;; 4. Org Appear - Show emphasis markers on demand
;; ──────────────────────────────────────────────

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;; ──────────────────────────────────────────────
;; 5. Org Fragtog - Auto-toggle LaTeX fragments
;; ──────────────────────────────────────────────

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

;; ──────────────────────────────────────────────
;; 6. Visual Fill Column - Centered text
;; ──────────────────────────────────────────────

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t))

;; ──────────────────────────────────────────────
;; 7. Org Fancy Priorities - Visual priority indicators
;; ──────────────────────────────────────────────

(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔴" "🟡" "🟢")))

;; ──────────────────────────────────────────────
;; 8. Custom Faces & Colors
;; ──────────────────────────────────────────────

(defun my/org-mode-visual-setup ()
  "Custom visual setup for org-mode."
  ;; Increase line spacing for better readability
  (setq-local line-spacing 0.2)

  ;; Better heading sizes
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :weight 'bold
                        :height (cdr face)))

  ;; Better document title
  (set-face-attribute 'org-document-title nil
                      :weight 'bold
                      :height 1.6)

  ;; Subtle meta lines
  (set-face-attribute 'org-meta-line nil
                      :inherit 'font-lock-comment-face
                      :height 0.9)

  ;; Better block appearance
  (set-face-attribute 'org-block nil
                      :background (face-attribute 'default :background)
                      :extend t)

  (set-face-attribute 'org-block-begin-line nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'font-lock-comment-face :foreground)
                      :height 0.9
                      :extend t)

  (set-face-attribute 'org-block-end-line nil
                      :inherit 'org-block-begin-line
                      :extend t))

(add-hook 'org-mode-hook #'my/org-mode-visual-setup)

;; ──────────────────────────────────────────────
;; 9. Better Source Block Editing
;; ──────────────────────────────────────────────

(setq org-src-window-setup 'current-window
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

;; Syntax highlighting in source blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; ──────────────────────────────────────────────
;; 10. Better Link Display
;; ──────────────────────────────────────────────

(setq org-link-descriptive t
      org-link-file-path-type 'relative)

;; Custom link colors
(defun my/org-link-colors ()
  "Set custom colors for org links."
  (set-face-attribute 'org-link nil
                      :foreground "#89b4fa"
                      :underline t))

(add-hook 'org-mode-hook #'my/org-link-colors)

;; ──────────────────────────────────────────────
;; 11. Org Indent Mode Enhancements
;; ──────────────────────────────────────────────

(setq org-startup-indented t
      org-indent-indentation-per-level 2
      org-indent-mode-turns-on-hiding-stars t)

;; ──────────────────────────────────────────────
;; 12. Better Emphasis Markers
;; ──────────────────────────────────────────────

(setq org-emphasis-alist
      '(("*" bold)
        ("/" italic)
        ("_" underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" (:strike-through t))))

;; ──────────────────────────────────────────────
;; 13. Org Roam UI Enhancements
;; ──────────────────────────────────────────────

(with-eval-after-load 'org-roam-ui
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
        org-roam-ui-browser-function #'browse-url))

;; ──────────────────────────────────────────────
;; 14. Better Agenda Appearance
;; ──────────────────────────────────────────────

(setq org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "◀── now ─────────────────────────────────────────────────")

;; ──────────────────────────────────────────────
;; 15. Org Bullets Alternative (if preferred)
;; ──────────────────────────────────────────────

;; Uncomment if you prefer org-bullets over org-superstar
;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(provide 'init-org-appearance)
;;; init-org-appearance.el ends here
