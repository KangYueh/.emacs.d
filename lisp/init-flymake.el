;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; -----------------------------
;;; Flymake core (built-in)
;;; -----------------------------
(use-package flymake
  :straight (:type built-in)
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start)))

;;; -----------------------------
;;; Use flycheck checkers inside flymake
;;; -----------------------------
(use-package flymake-flycheck
  :after flymake
  :hook (flymake-mode . flymake-flycheck-auto)
  :config
  ;; 避免和 flymake 自带重复
  (with-eval-after-load 'flycheck
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))))

;;; -----------------------------
;;; Better eldoc integration (Emacs 28+)
;;; -----------------------------
(use-package eldoc
  :straight (:type built-in)
  :if (not (version< emacs-version "28.1"))
  :config
  (setq eldoc-documentation-function #'eldoc-documentation-compose)
  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions
                        #'flymake-eldoc-function nil t))))


(provide 'init-flymake)
;;; init-flymake.el ends here
