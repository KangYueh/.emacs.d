;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 默认不显示尾随空格
(setq-default show-trailing-whitespace nil)

;; 自动在指定模式显示尾随空格
(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

;; 使用 straight + use-package 安装 whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :straight t
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (diminish 'whitespace-cleanup-mode))

;; 使用 kbd 改写 remap 键绑定，避免报错
(global-set-key (kbd "M-SPC") 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
