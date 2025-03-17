;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package eglot
  :ensure t
  :defer t
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                    (eglot-ensure))))
   ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (add-to-list 'eglot-server-programs '(
                                        (python-mode python-ts-mode)
                                        "basedpyright-langserver" "--stdio"
                                        )))

(use-package consult-eglot
  :ensure t
  :after consult eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))
(provide 'init-eglot)
;;; init-eglot.el ends here
