;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)
  :config
  (setq tab-always-indent 'complete
        completion-cycle-threshold 4))

(use-package corfu
  :straight t
  :if (version<= "28.1" emacs-version)
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :config
  ;; eshell 里禁用 auto
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil)))
  ;; 显示 popupinfo
  (corfu-popupinfo-mode))


(use-package corfu-terminal
  :straight t
  :after corfu
  :config
  (corfu-terminal-mode))

(provide 'init-corfu)
;;; init-corfu.el ends here
