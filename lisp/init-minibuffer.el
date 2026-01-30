;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :straight t
  :hook (after-init . vertico-mode))
  
(use-package embark
  :straight t
  :after vertico
  :bind
  (:map vertico-map
        ("C-c C-o" . embark-export)
        ("C-c C-c" . embark-act))
  :config
  ;; 与 whole-line-or-region 兼容
  (with-eval-after-load 'whole-line-or-region
    (push 'embark--mark-target
          (alist-get 'whole-line-or-region-delete-region
                     embark-around-action-hooks))))

(use-package embark-consult
  :straight t
  :after (embark consult))  
  
(use-package consult
  :straight t
  :after vertico
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap goto-line] . consult-goto-line)
  :config
  ;; 禁用部分命令预览，避免卡顿
  (defmacro sanityinc/no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key "M-P")))

  (sanityinc/no-consult-preview
   consult-ripgrep
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-recent-file consult-source-project-recent-file consult-source-bookmark)

  ;; 自定义 ripgrep at point
  (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
    (interactive (list current-prefix-arg
                       (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         (if-let ((s (symbol-at-point)))
                             (symbol-name s)))))
    (consult-ripgrep dir initial))

  (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
  (when (executable-find "rg")
    (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point)))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
