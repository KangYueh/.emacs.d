;;; init-themes.el --- Modern theme configuration -*- lexical-binding: t -*-

;; ============================================================================
;; 字体配置（可选）
;; ============================================================================
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"  ;; 或 "Fira Code", "Cascadia Code"
                      :height 140)
  (set-face-attribute 'variable-pitch nil
                      :family "Inter"
                      :height 1.0))

;; ============================================================================
;; 主题配置
;; ============================================================================
(use-package catppuccin-theme
  :straight t
  :custom
  (catppuccin-flavor 'mocha)
  (catppuccin-italic-comments t)
  :config
  (load-theme 'catppuccin :no-confirm))

;; ============================================================================
;; UI 增强
;; ============================================================================
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20))

;; 图标支持（必需）
(use-package nerd-icons
  :straight t)

;; 窗口分割线（内置功能，不需要 use-package）
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2
      window-divider-default-places t)
(window-divider-mode 1)

;; 当前窗口强调
(use-package selected-window-accent-mode
  :straight (:host github :repo "captainflasmr/selected-window-accent-mode")
  :hook (after-init . selected-window-accent-mode)
  :custom
  (selected-window-accent-custom-color "#89b4fa")
  (selected-window-accent-fringe-thickness 4))

;; 彩虹括号（强烈推荐）
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 高亮当前行
(global-hl-line-mode 1)

;; 平滑滚动（Emacs 29+）
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(provide 'init-themes)
