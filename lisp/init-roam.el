;;; init-roam.el --- Zettelkasten + 论文阅读完整配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 基于 Org-roam v2 的 Zettelkasten 系统
;; 核心功能：
;; - 知识图谱管理
;; - 论文阅读工作流
;; - 卡片链接和导航
;; - 文献引用集成

;;; Code:

;;==============================================
;; Org-roam 核心配置
;;==============================================

(use-package org-roam
  :ensure t
  :after org
  :demand t
  :init
  (setq org-roam-v2-ack t)

  :custom
  ;; 目录配置
  (org-roam-directory (expand-file-name "/home/einhep/wdata/roam/"))
  (org-roam-dailies-directory "journals/")

  ;; 补全和显示
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   "${title:*} ${tags:30} | ${type:12}")

  ;; 数据库
  (org-roam-database-connector 'sqlite3)

  ;; 捕获模板 - Zettelkasten 核心
  (org-roam-capture-templates
   '(;; 概念卡片
     ("c" "🔹 Concept Card" plain
      "%?"
      :if-new (file+head "permanent-notes/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n#+type: Concept\n\n** 定义\n\n** 特征\n\n** 应用\n\n** 相关概念\n\n** 参考")
      :unnarrowed t)

     ;; 问题卡片
     ("Q" "❓ Question Card" plain
      "%?"
      :if-new (file+head "permanent-notes/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :question:\n#+type: Question\n\n** 问题陈述\n\n** 背景\n\n** 当前理解\n\n** 相关卡片")
      :unnarrowed t)

     ;; 论证卡片
     ("A" "🎯 Argument Card" plain
      "%?"
      :if-new (file+head "permanent-notes/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :argument:\n#+type: Argument\n\n** 主张\n\n** 证据\n\n** 反驳\n\n** 结论")
      :unnarrowed t)

     ;; 论文笔记
     ("p" "📖 Paper Note" plain
      "* 摘要\n\n%?\n\n* 创新贡献\n\n* 方法论\n\n* 实验结果\n\n* 相关工作\n\n* 我的想法\n\n* 提取的卡片"
      :if-new (file+head "papers/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :paper:\n#+type: Paper\n#+cite_key: ${slug}\n")
      :unnarrowed t)

     ;; 临时笔记
     ("f" "💡 Fleeting Note" plain
      "%?"
      :if-new (file+head "inbox/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :fleeting:\n#+type: Fleeting\n")
      :unnarrowed t)

     ;; 索引页
     ("i" "📑 Index Page" plain
      "* 概述\n\n%?\n\n* 子主题\n\n* 关键卡片\n\n* 最近更新"
      :if-new (file+head "indices/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :index:\n#+type: Index\n")
      :unnarrowed t)

     ;; 综述/项目
     ("s" "📊 Survey" plain
      "* 研究问题\n\n%?\n\n* 关键论文\n\n* 主要发现\n\n* 未来方向"
      :if-new (file+head "projects/${slug}.org"
			 "#+title: ${title}\n#+date: %U\n#+filetags: :survey:\n#+type: Survey\n")
      :unnarrowed t)

     ;; 默认
     ("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org"
			 "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)))

  ;; 日记模板
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d> %A\n"))

     ("r" "reading" entry "* 📖 阅读: [[cite:&%^{Cite Key}]]\n%?"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d> %A\n"))

     ("t" "todo" entry "* TODO %?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d> %A\n"))

     ("i" "idea" entry "* 💡 想法: %?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d> %A\n"))

     ("m" "meeting" entry "* 会议: %?\n** 参与者\n\n** 要点\n\n** 行动项"
      :if-new (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d> %A\n"))))

  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n q" . my/zettel-quick-capture)
	 ("C-c n C" . my/zettel-stats)
	 ("C-c n N" . my/zettel-recent)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))

  :bind-keymap
  ("C-c n j" . org-roam-dailies-map)

  :config
  ;; 启用日记
  (require 'org-roam-dailies)

  ;; 自动同步数据库
  (org-roam-db-autosync-mode)

  ;; 创建新节点时自动生成 ID
  (add-hook 'org-roam-capture-new-node-hook #'org-id-get-create)

  ;; 启用标签补全
  (setq org-roam-tag-sources '(prop all-directories))

  ;; 启用反向链接
  (org-roam-setup))

;;==============================================
;; Org-roam UI - 知识图谱可视化
;;==============================================

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-browser-function #'browse-url)
  :bind (:map org-roam-mode-map
	 ("C-c n u" . org-roam-ui-open)))

;;==============================================
;; Org-roam 侧边栏配置
;;==============================================

(with-eval-after-load 'org-roam
  ;; 侧边栏显示位置和大小
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.35)
		 (window-parameters . ((no-other-window . t)
				       (no-delete-other-windows . t)))))

  ;; 侧边栏显示的部分
  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section)))

;;==============================================
;; Citar - 论文和引用管理
;;==============================================

(use-package citar
  :ensure t
  :after org
  :custom
  ;; 配置文件和库路径
  (citar-bibliography '("/home/einhep/wdata/zotero/all.bib"))
  (citar-library-paths '("/home/einhep/wdata/zotero/"))
  (citar-notes-paths '("/home/einhep/wdata/roam/papers/"))

  ;; 显示选项
  (citar-file-additional-files-separator "-")
  (citar-at-point-fallback 'completing-read)
  (citar-symbol-separator "  ")

  ;; 符号配置
  (citar-symbols
   `((Miscellaneous . "📄")
     (key . "🔑")
     (PDF . "📕")
     (URL . "🌐")
     (DOI . "🔗")
     (note . "📝")))

  :bind (("C-c b" . citar-open)
	 ("C-c B" . citar-open-notes)
	 :map org-mode-map
	 ("C-c C-x C-@" . citar-insert-citation)
	 ("C-c n p" . my/paper-note-from-citar))

  :config
  ;; 确保目录存在
  (dolist (dir (list (car citar-bibliography)
		     (car citar-library-paths)
		     (car citar-notes-paths)))
    (unless (file-directory-p (file-name-directory dir))
      (make-directory (file-name-directory dir) t))))

;;==============================================
;; Citar-org-roam - 文献和笔记集成
;;==============================================

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :demand t
  :custom
  ;; 配置
  (citar-org-roam-subdir "papers")
  (citar-org-roam-note-title-template
   "${author editor:1} (${year}) - ${title}")
  (citar-org-roam-capture-template-key "p")

  :config
  ;; 注册为 citar 的笔记源
  (citar-org-roam-mode)

  (citar-register-notes-source
   'citar-org-roam
   '(:name "Org-Roam Notes"
     :category org-roam
     :items citar-org-roam-items
     :open citar-org-roam-open
     :create citar-org-roam-create-note
     :edit citar-org-roam-edit-note))

  (setq citar-notes-source 'citar-org-roam))

;;==============================================
;; Citar-Embark 集成
;;==============================================

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config
  (citar-embark-mode))

;;==============================================
;; 图片滚动
;;==============================================

(use-package iscroll
  :ensure t
  :diminish iscroll-mode
  :hook ((org-mode markdown-mode) . iscroll-mode))

;;==============================================
;; Zettelkasten 自定义函数
;;==============================================

;; 快速卡片捕获
(defun my/zettel-quick-capture (card-type)
  "快速创建卡片，按 c(概念) q(问题) a(论证) f(临时)"
  (interactive "cCard type: [c]oncept [q]uestion [a]rgument [f]leeting")
  (let ((template-key (case card-type
			(?c "c")
			(?q "q")
			(?a "a")
			(?f "f")
			(t "d"))))
    (org-roam-capture- :node (org-roam-node-create)
		       :templates (list (assoc template-key org-roam-capture-templates nil #'string=)))))

;; 卡片统计
(defun my/zettel-stats ()
  "显示 Zettelkasten 统计信息"
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
	 (concepts (seq-count (lambda (n)
			       (member "concept" (org-roam-node-tags n)))
			     all-nodes))
	 (questions (seq-count (lambda (n)
				(member "question" (org-roam-node-tags n)))
			      all-nodes))
	 (arguments (seq-count (lambda (n)
			       (member "argument" (org-roam-node-tags n)))
			     all-nodes))
	 (papers (seq-count (lambda (n)
			    (member "paper" (org-roam-node-tags n)))
			  all-nodes))
	 (fleeting (seq-count (lambda (n)
			      (member "fleeting" (org-roam-node-tags n)))
			    all-nodes)))
    (message "📊 Zettelkasten 统计\n💡 概念: %d\n❓ 问题: %d\n🎯 论证: %d\n📖 论文: %d\n💫 临时: %d\n═════\n合计: %d"
	     concepts questions arguments papers fleeting
	     (length all-nodes))))

;; 显示最近创建的卡片
(defun my/zettel-recent ()
  "显示最近创建的卡片"
  (interactive)
  (let ((nodes (seq-sort-by
		(lambda (n) (org-roam-node-file-mtime n))
		#'>
		(org-roam-node-list))))
    (if nodes
	(org-roam-node-visit (car nodes))
      (message "No nodes found"))))

;; 从 Citar 创建论文笔记
(defun my/paper-note-from-citar ()
  "从 Citar 选中的论文创建笔记"
  (interactive)
  (let ((citar-notes-source 'citar-org-roam))
    (citar-create-note)))

;; 处理临时笔记
(defun my/process-fleeting-notes ()
  "找到所有临时笔记并转换为永久笔记"
  (interactive)
  (org-roam-node-find nil nil
		      (lambda (node)
			(member "fleeting" (org-roam-node-tags node)))))

;; 按类型查找卡片
(defun my/zettel-find-by-type (type-name)
  "按类型查找卡片"
  (interactive (list (completing-read "类型: " '("concept" "question" "argument" "paper" "fleeting"))))
  (org-roam-node-find nil nil
		      (lambda (node)
			(member type-name (org-roam-node-tags node)))))

;; 卡片关联分析
(defun my/zettel-analyze-links ()
  "分析当前卡片的链接情况"
  (interactive)
  (let* ((node (org-roam-node-at-point))
	 (backlinks (org-roam-backlinks-get node))
	 (forward-links (org-roam-node-links-get node)))
    (message "🔍 卡片分析: %s\n← 被链接: %d\n→ 链接到: %d"
	     (org-roam-node-title node)
	     (length backlinks)
	     (length forward-links))))

;; 生成卡片摘要
(defun my/zettel-generate-summary ()
  "为当前卡片生成摘要"
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (message "卡片: %s\nID: %s\n标签: %s"
	     (org-roam-node-title node)
	     (org-roam-node-id node)
	     (string-join (org-roam-node-tags node) ", "))))

;; 按标签聚合卡片
(defun my/zettel-aggregate-by-tag (tag-name)
  "聚合具有特定标签的所有卡片"
  (interactive "s标签名称: ")
  (let ((nodes (seq-filter (lambda (node)
			    (member tag-name (org-roam-node-tags node)))
			  (org-roam-node-list))))
    (message "📍 标签 '%s' 包含 %d 张卡片" tag-name (length nodes))))

;;==============================================
;; 显示和交互
;;==============================================

;; 禁用行截断
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq truncate-lines nil
		  word-wrap t)
	    (org-display-inline-images t)))

;; 侧边栏自动刷新
(with-eval-after-load 'org-roam
  (add-hook 'org-roam-mode-hook
	    (lambda ()
	      (add-hook 'window-configuration-change-hook
		       'org-roam-buffer-refresh nil t))))

;;==============================================
;; 缓存优化
;;==============================================

;; 禁用 org-roam 的本地缓存
(setq org-roam-cache-sync-on-save t)

;;==============================================
;; 截图功能 - org-download
;;==============================================

(use-package org-download
  :ensure t
  :after org
  :demand t
  :bind (:map org-mode-map
	 ("C-c n s" . org-download-screenshot)
	 ("C-c n y" . org-download-clipboard)
	 ("C-c n d" . org-download-delete))
  :custom
  ;; gnome-screenshot 配置
  (org-download-screenshot-method "gnome-screenshot -a -f %s")
  (org-download-image-dir (expand-file-name "images/" org-roam-directory))
  (org-download-filename-format "img-%<%Y%m%d-%H%M%S>.png")
  (org-download-link-format "[[file:images/%s]]")
  (org-download-link-format-function #'org-download-link-format)
  (org-download-display-inline-images t)
  :config
  (unless (file-directory-p org-download-image-dir)
    (make-directory org-download-image-dir t)))

;;==============================================
;; 提供模块
;;==============================================

(provide 'init-roam)
;;; init-roam.el ends here
