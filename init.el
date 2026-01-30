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

;;; 笔记工具：Org Mode
(use-package org
  :straight (:type built-in)
  :config
  ;; 笔记目录
  (setq org-directory "~/Documents/notes")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  
  ;; 快速捕获
  (setq org-capture-templates
        '(("t" "任务" entry (file+headline "" "任务")
           "* TODO %?\n  %i\n  %a")
          ("n" "笔记" entry (file+headline "" "笔记")
           "* %?\n  %i\n  创建时间: %U")
          ("p" "Python 笔记" entry (file+headline "" "Python")
           "* %?\n  #+begin_src python\n  %i\n  #+end_src")))
  
  ;; 显示设置
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t)
  
  ;; 代码块设置
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  
  ;; Babel 支持的语言
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)))
  
  ;; 快捷键
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

;;; Org 增强
(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/Documents/roam")
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)))

;;; Markdown 支持
(use-package markdown-mode
  :straight t
  :config
  (setq markdown-command "pandoc"))

;;; Python 开发环境
(use-package python
  :straight (:type built-in)
  :config
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"))

;; Pyright LSP 支持
(use-package eglot
  :straight (:type built-in)
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-stay-out-of '(flymake))
  :bind (:map eglot-mode-map
         ("C-c d" . eglot-find-declaration)
         ("C-c r" . eglot-rename)
         ("C-c f" . eglot-format-buffer)))

;; Flymake 代码检查
(use-package flymake
  :straight (:type built-in)
  :hook (python-mode . flymake-mode)
  :config
  (setq flymake-python-pycodestyle-extra-args '("--max-line-length=100")))

;; 代码格式化：Black
(use-package python-black
  :straight t
  :hook (python-mode . python-black-on-save-mode))

;; Python 虚拟环境
(use-package pyvenv
  :straight t
  :hook (python-mode . pyvenv-mode)
  :config
  (setq pyvenv-default-virtual-env-name "venv"))

;; IPython REPL
(use-package ein
  :straight t
  :config
  (setq ein:output-area-inlined-images t))

;;; 代码补全
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;;; 代码段模板
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t)

;;; 快速导航：Ivy + Counsel
(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)))

(use-package counsel
  :straight t
  :after ivy
  :config
  (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)))

(use-package ivy-rich
  :straight t
  :after ivy
  :init (ivy-rich-mode 1))

;;; 或者使用 Vertico（更现代的选择）
;; (use-package vertico
;;   :straight t
;;   :init (vertico-mode)
;;   :config
;;   (setq vertico-count 15))

;;; 项目管理
(use-package projectile
  :straight t
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/projects/" "~/workspace/"))
  :bind (("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

;;; Git 集成
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
         (prog-mode . diff-hl-mode)))

;;; 括号配对
(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; 代码注释
(use-package evil-nerd-commenter
  :straight t
  :bind (("C-/" . evilnc-comment-or-uncomment-lines)
         ("C-?" . evilnc-comment-or-uncomment-to-the-line)))

;;; 括号高亮
(use-package paren
  :straight (:type built-in)
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis))

;;; 缓冲区管理
(use-package ibuffer
  :straight (:type built-in)
  :bind (("C-x C-b" . ibuffer)))

;;; 撤销系统
(use-package undo-tree
  :straight t
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil))

;;; 快捷键帮助
(use-package which-key
  :straight t
  :config (which-key-mode 1))

;;; 文件树导航
(use-package treemacs
  :straight t
  :bind (("C-c t" . treemacs)))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

;;; 性能优化
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;;; 自定义快捷键
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c q") 'kill-buffer)
(global-set-key (kbd "C-c s") 'sort-lines)

;;; 文件自动保存
(setq auto-save-interval 100
      make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(provide 'init)
;;; init.el ends here
