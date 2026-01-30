;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 'never)   ;; 避免卡顿
  (recentf-exclude
   `("/tmp/"
     "/ssh:"
     ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list))


(provide 'init-recentf)
;;; init-recentf.el ends here
