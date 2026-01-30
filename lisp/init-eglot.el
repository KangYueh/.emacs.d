;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :defer t)

(use-package consult-eglot
  :after (consult eglot)
  :defer t)


(provide 'init-eglot)
;;; init-eglot.el ends here
