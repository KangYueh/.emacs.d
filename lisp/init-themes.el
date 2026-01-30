;;; init-themes.el --- Theme defaults -*- lexical-binding: t -*-
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25))

(provide 'init-themes)
;;; init-themes.el ends here
