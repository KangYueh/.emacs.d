;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
; 自动括号、缩进、删除覆盖
(use-package elec-pair
  :straight nil
  :hook (after-init . electric-pair-mode))

(use-package elec-indent
  :straight nil
  :hook (after-init . electric-indent-mode))

(use-package simple
  :straight nil
  :hook (after-init . delete-selection-mode))

;; 显示行号
(use-package display-line-numbers
  :straight nil
  :hook ((prog-mode yaml-mode yaml-ts-mode) . display-line-numbers-mode)
  :init (setq-default display-line-numbers-width 3))

;; 显示列指示器
(use-package display-fill-column-indicator
  :straight nil
  :hook (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default indicate-buffer-boundaries 'left
                display-fill-column-indicator-character ?┊))

;; 匹配括号
(use-package paren
  :straight nil
  :hook (after-init . show-paren-mode))

;; repeat-mode
(use-package repeat
  :straight nil
  :hook (after-init . repeat-mode))

;; ---------------------------
;; Large file support
;; ---------------------------
(use-package so-long
  :straight nil
  :hook (after-init . so-long-enable))

(use-package vlf
  :straight t
  :commands (vlf ffap-vlf)
  :config
  (defun ffap-vlf ()
    "Open file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

;; ---------------------------
;; Visual bells and cues
;; ---------------------------
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode))

;; 彩色括号
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 符号高亮
(use-package symbol-overlay
  :straight t
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :config
  (diminish 'symbol-overlay-mode))

;; kill-ring 浏览
(use-package browse-kill-ring
  :straight t
  :bind ("M-Y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-separator "\f")
  (with-eval-after-load 'browse-kill-ring
    (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
    (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous)))

;; ---------------------------
;; Multiple cursors
;; ---------------------------
(use-package multiple-cursors
  :straight t
  :bind (("C-<"   . mc/mark-previous-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("C-+"   . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ---------------------------
;; Move and duplicate lines
;; ---------------------------
(use-package move-dup
  :straight t
  :bind (("M-S-<up>"   . move-dup-move-lines-up)
         ("M-S-<down>" . move-dup-move-lines-down)
         ("C-c u"    . move-dup-duplicate-up)
         ("C-c d"    . move-dup-duplicate-down)))

;; ---------------------------
;; whole-line-or-region
;; ---------------------------
(use-package whole-line-or-region
  :straight t
  :hook (after-init . whole-line-or-region-global-mode)
  :config
  (diminish 'whole-line-or-region-local-mode))

;; ---------------------------
;; Zap up to char
;; ---------------------------
(use-package simple
  :straight nil
  :bind ("M-Z" . zap-up-to-char))

;; ---------------------------
;; Backward-up-sexp smarter
;; ---------------------------
(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))
(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp)

;; ---------------------------
;; Misc utilities
;; ---------------------------
(use-package highlight-escape-sequences
  :straight t
  :hook (after-init . hes-mode))

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :init (setq-default which-key-idle-delay 1.5)
  :config
  (diminish 'which-key-mode))

;; ---------------------------
;; Newline at end of line
;; ---------------------------
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, newline and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; ---------------------------
;; Kill back to indentation
;; ---------------------------
(defun kill-back-to-indentation ()
  "Kill from point back to first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;; ---------------------------
;; Page break lines
;; ---------------------------
(use-package page-break-lines
  :straight t
  :hook (after-init . global-page-break-lines-mode)
  :config
  (diminish 'page-break-lines-mode))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
