;;; init.el

;;; 基础设置

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)

;; 编码
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; for customized setting 
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)

;;; straight.el 引导
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use-package 配置（与 straight 集成）
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'init-exec-path)
(require 'init-performance)

;; diminish: 隐藏 minor mode 显示
(use-package diminish
  :straight t
  :defer t)  ;; 延迟加载，不影响启动速度

;; scratch: 增强 scratch buffer（可选）
(use-package scratch
  :straight t
  :defer t
  :init
  ;; 如果想启动时自动打开 scratch buffer，可加下面
  ;; (add-hook 'emacs-startup-hook #'scratch)
  )

;; command-log-mode: 命令记录 / 演示
(use-package command-log-mode
  :straight t
  :defer t
  :commands (command-log-mode clm/open-command-log-buffer))

(require 'init-themes)  
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)

(require 'init-org)
(require 'init-roam)
(require 'init-python)

(provide 'init)
;;; init.el ends here
