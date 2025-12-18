;;; init-writing.el  -*- lexical-binding: t -*-
;;; Commentary:

;; This file support chinese input

;;; Code:

(use-package rime
  :ensure t
  :init
  (setq default-input-method "rime")
  :config
  (progn (set-face-attribute 'rime-default-face nil :foreground "#839496" :background "#073642")
         (setq rime-show-candidate 'posframe)
         (setq rime-user-data-dir "~/.config/ibus/rime")
         (setq rime-posframe-properties
               (list :background-color "#073642"
                     :foreground-color "#839496"
                     :internal-border-width 1))))

(use-package pyim
  :ensure t
  :commands (pyim-cregexp-build)
  :init
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))


  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
  (add-hook 'minibuffer-exit-hook 'disable-py-search)

  (global-set-key (kbd "s-p") 'toggle-chinese-search)
  ;; use #$#pyim to search chinese and also es.exe locate 子龙
  )

(provide 'init-writing)
;;; init-writing.el ends here
