;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "http://127.0.0.1:10077")
;;         ("https" . "https://127.0.0.1:10077")))

;; (defun proxy-socks-show ()
;;   "Show SOCKS proxy."
;;   (interactive)
;;   (when (fboundp 'cadddr)
;;     (if (bound-and-true-p socks-noproxy)
;;         (message "Current SOCKS%d proxy is %s:%d"
;;                  (cadddr socks-server) (cadr socks-server) (caddr socks-server))
;;       (message "No SOCKS proxy"))))

;; (defun proxy-socks-enable ()
;;   "Enable SOCKS proxy."
;;   (interactive)
;;   (require 'socks)
;;   (setq url-gateway-method 'socks
;;         socks-noproxy '("localhost")
;;         socks-server '("Default server" "127.0.0.1" 12377 5))
;;   (setenv "all_proxy" "socks5://127.0.0.1:12377")
;;   (proxy-socks-show))

;; (defun proxy-socks-disable ()
;;   "Disable SOCKS proxy."
;;   (interactive)
;;   (require 'socks)
;;   (setq url-gateway-method 'native
;;         socks-noproxy nil)
;;   (setenv "all_proxy" "")
;;   (proxy-socks-show))

;; (defun proxy-socks-toggle ()
;;   "Toggle SOCKS proxy."
;;   (interactive)
;;   (require 'socks)
;;   (if (bound-and-true-p socks-noproxy)
;;       (proxy-socks-disable)
;;     (proxy-socks-enable)))

(window-numbering-mode t)

(provide 'init-local)
;;;init-local.el ends here
