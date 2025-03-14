;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:12377")
        ("https" . "127.0.0.1:12377")))


(window-numbering-mode t)

(provide 'init-local)
;;;init-local.el ends here
