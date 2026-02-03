;;; init-python.el --- Deep Learning Research Environment

;; ============================================================================
;; 1. Python基础配置
;; ============================================================================

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :config
  (setq python-shell-interpreter "python3"
	python-indent-offset 4
	python-shell-completion-native-enable t
	python-shell-prompt-detect-failure-warning nil
	;; IPython配置
	python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --no-confirm-exit"
	python-shell-prompt-regexp "In \\[[0-9]+\\]: "
	python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	python-shell-completion-native-enable t))

;; ============================================================================
;; 2. Eglot LSP客户端 - 轻量级但强大
;; ============================================================================

(use-package eglot
  :ensure t
  :hook (python-ts-mode . eglot-ensure)
  :config
  ;; Pyright配置 - 类型检查最佳
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("pyright-langserver" "--outputjson")))

  ;; Eglot行为调整
  (setq eglot-autoshutdown t
	eglot-confirm-server-edits nil
	eglot-extend-to-xref t)

  ;; 自动保存时格式化（可选，取决于你的习惯）
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)

  :bind (:map eglot-mode-map
	      ("C-c l d" . eglot-find-declaration)
	      ("C-c l D" . eglot-find-implementation)
	      ("C-c l r" . eglot-rename)
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l h" . eglot-inlay-hints-mode)
	      ("C-c l s" . eglot-shutdown)))

;; ============================================================================
;; 3. 代码补全 - Company
;; ============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package company								       ;;
;;   :ensure t									       ;;
;;   :hook (prog-mode . company-mode)						       ;;
;;   :config									       ;;
;;   (setq company-minimum-prefix-length 1					       ;;
;;	company-idle-delay 0.05							       ;;
;;	company-show-numbers t							       ;;
;;	company-tooltip-align-annotations t					       ;;
;;	company-backends '(company-capf company-files company-dabbrev-code))	       ;;
;;										       ;;
;;   ;; 快捷键									       ;;
;;   :bind (:map company-active-map						       ;;
;;	      ("C-n" . company-select-next)					       ;;
;;	      ("C-p" . company-select-previous)					       ;;
;;	      ("C-d" . company-show-doc-buffer)					       ;;
;;	      ("M-." . company-show-location)))					       ;;
;;										       ;;
;; ;; UI增强									       ;;
;; (use-package company-box							       ;;
;;   :ensure t									       ;;
;;   :hook (company-mode . company-box-mode)					       ;;
;;   :config									       ;;
;;   (setq company-box-show-single-candidate t					       ;;
;;	company-box-max-candidates 15))						       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ============================================================================
;; 4. 代码格式化和Linting
;; ============================================================================

;; Black - Python格式化
(use-package python-black
  :after python
  :config
  (setq black-line-length 88)
  :bind (:map python-mode-map
	      ("C-c C-f" . python-black-buffer)
	      ("C-c C-F" . python-black-region)))

;; 导入排序
(use-package python-isort
  :ensure t
  :bind (:map python-mode-map
	      ("C-c C-s" . python-isort-buffer)))

;; ============================================================================
;; 6. REPL交互 - 研究必需
;; ============================================================================

(defun python-send-buffer-and-switch ()
  "执行整个buffer并切换到REPL"
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(defun python-send-region-and-switch (start end)
  "执行选中区域并切换到REPL"
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell))

(use-package python
  :bind (:map python-mode-map
	      ("C-c C-c" . python-send-buffer-and-switch)
	      ("C-c C-l" . python-shell-send-file)
	      ("C-c C-r" . python-send-region-and-switch)
	      ("C-c C-z" . python-shell-switch-to-shell)
	      ("C-M-x" . python-shell-send-defun)))

;; ============================================================================
;; 7. 调试 - DAP模式
;; ============================================================================

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python)
  (setq dap-python-executable "python3"
	dap-python-debugger 'debugpy)

  ;; 自动安装debugpy
  (defun dap-python-setup ()
    (message "Setting up DAP for Python...")
    (shell-command "pip install debugpy"))

  :bind (:map dap-mode-map
	      ("<f5>" . dap-debug)
	      ("<f9>" . dap-breakpoint-toggle)
	      ("<f10>" . dap-step-over)
	      ("<f11>" . dap-step-in)
	      ("S-<f11>" . dap-step-out)
	      ("<f6>" . dap-continue)))

(use-package dap-ui
  :straight (:type built-in)
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-ui-buffer-configurations
	`(("*dap-ui-locals*" . ((side . right) (slot . 1) (window-width . 0.2)))
	  ("*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.2)))
	  ("*dap-ui-repl*" . ((side . bottom) (slot . 3) (window-height . 0.2))))))

;; ============================================================================
;; 8. 文档和帮助
;; ============================================================================

;; Eldoc - 内联文档
(use-package eldoc
  :hook (python-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1))

;; Helpful - 增强帮助
(use-package helpful
  :ensure t
  :bind (("C-c C-d" . helpful-at-point)
	 (:map python-mode-map
	       ("C-h f" . helpful-callable)
	       ("C-h v" . helpful-variable))))

;; ============================================================================
;; 9. 语法高亮和美化
;; ============================================================================
;; 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :hook (python-mode . rainbow-delimiters-mode))

;; 缩进指示
(use-package indent-guide
  :ensure t
  :hook (python-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"
	indent-guide-recursive t))

;; ============================================================================
;; 10. Notebook支持（可选但推荐）
;; ============================================================================

;; Jupyter/IPython notebook支持
(use-package ein
  :ensure t
  :config
  (setq ein:jupyter-default-kernel "python3"))

;; ============================================================================
;; 11. 深度学习特定工具
;; ============================================================================

;; Torch/TensorFlow自动补全增强
(defun setup-dl-environment ()
  "设置深度学习开发环境"
  (message "Deep Learning environment initialized")
  ;; 可以添加自定义代码片段、快捷方式等
  )

(add-hook 'python-mode-hook 'setup-dl-environment)

;; ============================================================================
;; 12. 快捷命令定义
;; ============================================================================

(defun python-eval-expression (expr)
  "在Python REPL中计算表达式"
  (interactive "sExpression: ")
  (python-shell-send-string expr)
  (python-shell-switch-to-shell))

(defun python-quick-test ()
  "快速运行当前文件进行测试"
  (interactive)
  (python-shell-send-file (buffer-file-name) nil t))

;; ============================================================================
;; 13. 代码片段（snippets）
;; ============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package yasnippet			       ;;
;;   :ensure t					       ;;
;;   :hook (python-mode . yas-minor-mode)	       ;;
;;   :config					       ;;
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
	      ("C-c t f" . python-pytest-file)
	      ("C-c t t" . python-pytest-function)
	      ("C-c t c" . python-pytest-class)))

;; ============================================================================
;; 15. 性能监控（可选）
;; ============================================================================

(defun python-profiling-setup ()
  "为性能分析配置环境"
  (interactive)
  (setq python-shell-interpreter "python3"
	python-shell-interpreter-args
	"-m cProfile -s cumtime"))


(provide 'init-python)
;;; python-dl-config.el ends here
