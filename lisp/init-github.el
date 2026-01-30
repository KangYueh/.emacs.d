;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 加载 init-git 配置
(require 'init-git)

;; 使用 `use-package` 加载 `yagist`
(use-package yagist
  :if (executable-find "git")  ;; 确保 git 可用
  :config
  ;; 如果你有特定的配置可以在这里设置
)

;; 使用 `use-package` 加载 bug-reference-github
(use-package bug-reference-github
  :ensure t
  :hook (prog-mode . bug-reference-prog-mode))


(use-package github-clone
  :ensure t)

(use-package forge
  :ensure t)

(use-package github-review
  :ensure t)

;; 使用 `flymake-actionlint` 时，确保正确设置钩子
(use-package flymake-actionlint
  :ensure t
  :hook (yaml-mode . flymake-actionlint-action-load-when-actions-file))
  
(provide 'init-github)
;;; init-github.el ends here
