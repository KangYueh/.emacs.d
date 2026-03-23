;;; init-themes.el --- Theme configuration -*- lexical-binding: t -*-

;; 字体配置
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 140))

;; 使用内置主题
(load-theme 'modus-vivendi :no-confirm)

;; 高亮当前行
(global-hl-line-mode 1)

(provide 'init-themes)
;;; init-themes.el ends here
