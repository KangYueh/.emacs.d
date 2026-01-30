;;; init-gui-frames.el --- GUI-specific frame config -*- lexical-binding: t -*-

;; 阻止 macOS GUI 下 C-z 最小化
(global-set-key (kbd "C-z")
                (lambda ()
                  (interactive)
                  (unless (and *is-a-mac* window-system)
                    (suspend-frame))))

;; -------------------------------
;; GUI 优化
;; -------------------------------
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      window-resize-pixelwise t
      frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))



;; -------------------------------
;; macOS 特定
;; -------------------------------
(when *is-a-mac*
  ;; 全屏快捷键
  (when (fboundp 'toggle-frame-fullscreen)
    (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))
  
  ;; 自动标题栏
  (use-package ns-auto-titlebar
    :straight t
    :hook (after-init . ns-auto-titlebar-mode)))

;; -------------------------------
;; Frame title 显示
;; -------------------------------
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; -------------------------------
;; term-mode 调整
;; -------------------------------
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))

;; -------------------------------
;; 全局字体缩放
;; -------------------------------
(use-package default-text-scale
  :straight t
  :hook (after-init . default-text-scale-mode))

;; -------------------------------
;; 禁用鼠标
;; -------------------------------
(use-package disable-mouse
  :straight t
  :defer t)

;; -------------------------------
;; 平滑滚动
;; -------------------------------
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here

