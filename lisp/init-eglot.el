;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'init-eglot)
;;; init-eglot.el ends here
