;;; init-darcs.el --- Support for the Darcs DVCS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vc-darcs
  :straight t
  :config
  (add-to-list 'vc-handled-backends 'DARCS)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)
  (setq darcsum-whatsnew-switches "-l"))


(provide 'init-darcs)
;;; init-darcs.el ends here
