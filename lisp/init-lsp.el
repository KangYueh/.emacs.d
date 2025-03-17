;;; init-lsp.el --- Org-lsp config -*- lexical-binding: t -*-
;;; Commentary:

;;; use pyright as server and lsp-mode as client.
;;; setting about pyright and lsp-mode can be found in site.
;;; install nvm as node installer, then run nvm install node
;;; to get latest version of node.


;;; Code:
;;-------------------------zotero--------------------------------

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-ts-mode . (lambda ()
;;                             (require 'lsp-pyright)
;;                             (lsp)
;;                             (eglot-ensure))) ;; 启动 eglot
;;   )
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (python-ts-mode . lsp)
;;   :commands lsp)

(setq lsp-pyright-server-command '("basedpyright"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

;;; use lsp-booster
;;; first of all should install eglot-booster using "m-x package-vc-install
;;; and pasting the url "https://github.com/jdtsmith/eglot-booster"

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(provide 'init-lsp)
;;;init-lsp.el ends here
