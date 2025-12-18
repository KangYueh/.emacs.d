;;; init-snippet.el --- init-snippet config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; YASnippet

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :init
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(yas/hippie-try-expand
            try-complete-file-name-partially
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))

    ;; (defun yas-setup-capf ()
    ;;   (setq-local completion-at-point-functions
    ;;               (cons #'cape-yasnippet
    ;;                     completion-at-point-functions)))

    ;; (add-hook 'prog-mode-hook 'yas-setup-capf)
    ;; (add-hook 'org-mode-hook 'yas-setup-capf)
    )

  )

(use-package consult-yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-snippet)
;;;init-snippet.el ends here
