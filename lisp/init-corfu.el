;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(when (and (version< "28.1" emacs-version) (maybe-require-package 'corfu))
  (setq-default corfu-auto t)
  (setq corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-preview-current t
        corfu-scroll-margin 5)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)


  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode))

  ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
  (when (maybe-require-package 'corfu-terminal)
    (with-eval-after-load 'corfu
      (corfu-terminal-mode)))

  ;; TODO: https://github.com/jdtsmith/kind-icon
  )
(provide 'init-corfu)
;;; init-corfu.el ends here
