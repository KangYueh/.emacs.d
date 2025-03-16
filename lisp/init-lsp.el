;;; init-lsp.el --- Org-lsp config -*- lexical-binding: t -*-
;;; Commentary:

;;; use pyright as server and lsp-mode as client.
;;; setting about pyright and lsp-mode can be found in site.
;;; install nvm as node installer, then run nvm install node
;;; to get latest version of node.


;;; Code:
;;-------------------------zotero--------------------------------

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp)
                            (eglot-ensure))) ;; 启动 eglot
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))
(defun my-auto-set-python-interpreter ()
  (setq-local lsp-pyright-python-executable-cmd (getenv "PYTHON_INTERPRETER")))

(provide 'init-lsp)
;;;init-lsp.el ends here
