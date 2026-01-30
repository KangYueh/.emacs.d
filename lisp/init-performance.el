;;; init-performance.el --- Emacs general performance tuning -*- lexical-binding: t -*-

(use-package diminish
  :straight t
  :defer t)

(use-package gcmh
  :straight t
  :defer t
  :init
  ;; 内存阈值：避免频繁 GC
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  :config
  ;; 启动 gcmh-mode 并隐藏模式行显示
  (gcmh-mode 1)
  (diminish 'gcmh-mode))
  
(setq jit-lock-defer-time 0)
(provide 'init-performance)
;;; performance.el ends here
