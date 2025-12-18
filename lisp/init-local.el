;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; 设置中文字体为简体中文
(set-fontset-font t 'han "Noto Sans CJK SC" nil 'prepend)


(window-numbering-mode t)

(provide 'init-local)
;;;init-local.el ends here
