;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:

;;; note by KangYueh:
;;; (1) install direnv in ubuntu, ensure the version of direnv is 2.32.1 or above.
;;; (2) create the file named .envrc, and filled with
;;; export PYTHON_INTERPRETER=/home/einhep/workspace/vrbi/.venv/bin/python
;;; export VIRTUAL_ENV=/home/einhep/workspace/vrbi/.venv
;;; layout python
;;; ...for PYTHON_INTERPRETER var is defined for lsp, and VIRTUAL_ENV is defined for direnv

;;; Code:


(when (maybe-require-package 'envrc)
  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  (add-hook 'after-init-hook 'envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
