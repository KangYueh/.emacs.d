;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 配置 projectile
(use-package projectile
  :straight t
  :init
  (add-hook 'after-init-hook 'projectile-mode)  ;; 启动时启用 projectile-mode
  :config
  ;; 修改 projectile 在 mode-line 中的显示前缀
  (setq-default projectile-mode-line-prefix " Proj")

  ;; 如果安装了 rg，配置 generic command 使用 rg 作为搜索工具
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  ;; 设置快捷键，方便用户通过 `C-c p` 访问 `projectile` 命令
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; 使用 ibuffer-projectile 来增强 ibuffer 的功能（按项目分组文件）
  (use-package ibuffer-projectile
    :straight t
    :after projectile))




(provide 'init-projectile)
;;; init-projectile.el ends here
