;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs 27+ loads this file before init.el and package.el

;;; Code:

;; Disable package.el to avoid conflicts with straight.el
(setq package-enable-at-startup nil)

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))))

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation-deny-list nil)

(provide 'early-init)
;;; early-init.el ends here
