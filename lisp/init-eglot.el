;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(
                                        (python-mode python-ts-mode)
                                        "basedpyright-langserver" "--stdio"
                                        )))

(provide 'init-eglot)
;;; init-eglot.el ends here
