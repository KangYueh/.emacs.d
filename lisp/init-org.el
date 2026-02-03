;;; init-org.el --- Zettelkasten 优化的 Org-mode 配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 为卡片笔记法和论文阅读优化的 Org-mode 配置
;; 工作流：收集 → 临时笔记 → 永久卡片 → 索引 → 输出

;;; Code:

;;==============================================
;; 基础包依赖
;;==============================================

(use-package org
  :ensure t
  :demand t
  :custom
  ;; 显示
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-indent-mode-turns-on-hiding-stars t)

  ;; 编辑
  (org-edit-timestamp-down-means-later t)
  (org-catch-invisible-edits 'show)
  (org-return-follows-link nil)
  (org-use-speed-commands t)
  (org-use-effective-time t)

  ;; 导出
  (org-export-coding-system 'utf-8)
  (org-export-kill-product-buffer-when-displayed t)
  (org-html-validation-link nil)

  ;; 标签
  (org-tags-column 80)
  (org-fast-tag-selection-single-key 'expert)

  ;; 日志
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t))

;;==============================================
;; Org-modern - 美化界面
;;==============================================

(use-package org-modern
  :ensure t
  :after org
  :demand t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "✸"))
  (org-modern-list '((42 . "•") (43 . "◦") (45 . "–")))
  (org-modern-tag nil)
  (org-modern-priority nil)
  (org-modern-checkbox '((88 . "☑") (45 . "▪") (32 . "☐")))
  (org-modern-progress nil))

;;==============================================
;; 全局快捷键
;;==============================================

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

;; 时钟快捷键
(define-key global-map (kbd "C-c o j") 'org-clock-goto)
(define-key global-map (kbd "C-c o l") 'org-clock-in-last)
(define-key global-map (kbd "C-c o i") 'org-clock-in)
(define-key global-map (kbd "C-c o o") 'org-clock-out)

;;==============================================
;; Capture 模板 - Zettelkasten 核心
;;==============================================

(setq org-capture-templates
      '(;; 📥 临时笔记：快速捕获想法（待处理）
	("f" "💡 Fleeting Note" entry
	 (file+headline "inbox/fleeting-notes.org" "Inbox")
	 "* INBOX %?\n:PROPERTIES:\n:CREATED: %U\n:TYPE: fleeting\n:SOURCE: %a\n:END:\n%i"
	 :prepend t
	 :clock-in t
	 :clock-resume t)

	;; 📖 论文阅读笔记（需处理）
	("r" "📖 Paper Reading Note" entry
	 (file+headline "inbox/fleeting-notes.org" "Reading Notes")
	 "* INBOX 论文: %^{论文标题}\n:PROPERTIES:\n:CREATED: %U\n:TYPE: paper-reading\n:CITE_KEY: %^{Cite Key}\n:AUTHORS: %^{作者}\n:YEAR: %^{年份}\n:END:\n\n** 快速摘要\n%?\n\n** 关键点\n- \n\n** 我的问题\n- \n"
	 :prepend t)

	;; 💡 概念卡片（永久笔记）
	("c" "🔹 Concept Card" entry
	 (file+headline "permanent-notes/concepts.org" "Concepts")
	 "* %^{概念名称}\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: concept\n:TAGS: %^{标签|AI|ML|NLP|CV|DL|Math}\n:END:\n\n** 定义\n\n%?\n\n** 特征\n- \n\n** 应用场景\n- \n\n** 相关概念\n- [[id:][相关概念1]]\n- [[id:][相关概念2]]\n\n** 参考文献\n- [[cite:&key1]]\n"
	 :prepend t)

	;; ❓ 问题卡片（研究驱动）
	("q" "❓ Question Card" entry
	 (file+headline "permanent-notes/questions.org" "Questions")
	 "* %^{问题}\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: question\n:STATUS: Open\n:PRIORITY: %^{优先级|High|Medium|Low}\n:END:\n\n** 问题陈述\n\n%?\n\n** 背景和动机\n\n** 当前理解\n- \n\n** 已知的解答\n- \n\n** 需要进一步调查\n- \n\n** 相关卡片\n- [[id:][相关卡片1]]\n"
	 :prepend t)

	;; 🎯 论证卡片（观点和见解）
	("a" "🎯 Argument Card" entry
	 (file+headline "permanent-notes/arguments.org" "Arguments")
	 "* %^{论证/主张}\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: argument\n:STRENGTH: %^{强度|weak|medium|strong}\n:CONFIDENCE: %^{信度|low|medium|high}\n:END:\n\n** 主张\n\n%?\n\n** 理由\n1. \n2. \n3. \n\n** 证据\n- \n\n** 反驳和限制\n- \n\n** 结论\n\n** 相关卡片和文献\n- [[id:][相关论点]]\n- [[cite:&key1]]\n"
	 :prepend t)

	;; 📝 论文笔记（与具体论文绑定）
	("p" "📄 Paper Note" entry
	 (file+headline "papers/%<%Y>-%(my/get-paper-slug).org" "Paper Notes")
	 "* 论文笔记\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: paper\n:CITE_KEY: %^{Cite Key}\n:END:\n\n** 论文元信息\n- 标题: %^{标题}\n- 作者: %^{作者}\n- 年份: %^{年份}\n- DOI: %^{DOI}\n- PDF: [[file:%^{PDF 路径}]]\n\n** 摘要（用自己的语言）\n\n%?\n\n** 创新贡献（3 个主要贡献）\n1. \n2. \n3. \n\n** 方法论和技术\n\n** 实验和结果\n\n** 相关工作\n\n** 关键数据和图表\n\n** 我的想法和评论\n\n** 提取的卡片\n- [ ] 概念 1: \n- [ ] 概念 2: \n- [ ] 问题: \n- [ ] 论点: \n\n** 后续行动\n- [ ] 深入理解某个部分\n- [ ] 寻找相关论文\n- [ ] 应用到我的研究\n"
	 :prepend t)

	;; 📑 索引页（主题导航）
	("i" "📑 Index Page" entry
	 (file+headline "indices/%<%Y>-index.org" "Indices")
	 "* %^{主题名称}\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: index\n:TAGS: Index\n:END:\n\n** 主题概述\n\n%?\n\n** 子主题和分类\n*** [[id:][子主题1]]\n*** [[id:][子主题2]]\n\n** 关键概念卡片\n- [[id:][概念1]]\n- [[id:][概念2]]\n\n** 重要论文\n- [[cite:&key1]]\n- [[cite:&key2]]\n\n** 核心问题\n- [[id:][问题1]]\n- [[id:][问题2]]\n\n** 主要论点和见解\n- [[id:][论点1]]\n- [[id:][论点2]]\n\n** 相关领域\n- [[id:][相关领域1]]\n- [[id:][相关领域2]]\n\n** 最近更新\n- [2025-01-15] 添加新概念\n- [2025-01-10] 更新论文列表\n\n** 下一步\n- [ ] 阅读论文 X\n- [ ] 深化理解 Y\n"
	 :prepend t)

	;; 📊 综述/项目（汇总性笔记）
	("s" "📊 Survey/Review" entry
	 (file+headline "projects/%<%Y>-${slug}.org" "Projects")
	 "* %^{标题}\n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:TYPE: survey\n:STATUS: Active\n:END:\n\n** 研究问题和范围\n\n%?\n\n** 相关论文清单\n- [ ] [[cite:&key1]] - \n- [ ] [[cite:&key2]] - \n\n** 关键发现\n1. \n2. \n3. \n\n** 知识空白和挑战\n- \n\n** 未来研究方向\n- \n\n** 提取的核心卡片\n- [[id:][概念1]]\n- [[id:][问题1]]\n- [[id:][论点1]]\n"
	 :prepend t)

	;; 🎓 文献阅读清单
	("l" "📚 Reading List" entry
	 (file+headline "reading-list/todo.org" "To Read")
	 "* TODO [[cite:&%^{Cite Key}]]\n:PROPERTIES:\n:CREATED: %U\n:PRIORITY: %^{优先级|High|Medium|Low}\n:TAGS: Reading|%^{领域|AI|ML|NLP|CV}\n:END:\n\n%?\n"
	 :prepend t)))

;;==============================================
;; Refiling 配置
;;==============================================

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)
			    (nil :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-cache nil)

;; 自动保存重新分类后的缓冲区
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; 快速 refile 到永久笔记
(defun my/refile-to-permanent-notes ()
  "快速将临时笔记转移到永久笔记"
  (interactive)
  (let ((org-refile-targets '(("permanent-notes/concepts.org" :maxlevel . 1)
			      ("permanent-notes/questions.org" :maxlevel . 1)
			      ("permanent-notes/arguments.org" :maxlevel . 1))))
    (org-refile)))

(global-set-key (kbd "C-c n r") 'my/refile-to-permanent-notes)

;;==============================================
;; TODO 关键字 - Zettelkasten 处理流程
;;==============================================

(setq org-todo-keywords
      '((sequence "INBOX(i)" "PROCESSING(p)" "|" "PERMANENT(d!)")
	(sequence "READING(r)" "EXTRACTING(e)" "|" "DONE(✓)")
	(sequence "TODO(t)" "IN-PROGRESS()" "|" "DONE(d!)" "CANCELLED(c@)"))
      org-todo-repeat-to-state "TODO")

(setq org-todo-keyword-faces
      '(("INBOX" . (:foreground "#e74c3c" :weight bold :background "#fadbd8"))
	("PROCESSING" . (:foreground "#e67e22" :weight bold :background "#fdebd0"))
	("PERMANENT" . (:foreground "#27ae60" :weight bold :background "#d5f4e6"))
	("READING" . (:foreground "#3498db" :weight bold :background "#d6eaf8"))
	("EXTRACTING" . (:foreground "#9b59b6" :weight bold :background "#ebdef0"))
	("TODO" . (:foreground "#2c3e50" :weight normal))
	("IN-PROGRESS" . (:foreground "#f39c12" :weight bold))
	("DONE" . (:foreground "#27ae60" :weight normal :strike-through t))
	("CANCELLED" . (:foreground "#7f8c8d" :weight normal :strike-through t))))

;;==============================================
;; 标签体系 - 导航和组织
;;==============================================

(setq org-tag-alist
      '(;; 卡片类型
	(:startgroup . nil)
	("concept" . ?c)
	("question" . ?q)
	("argument" . ?a)
	("fleeting" . ?f)
	("paper" . ?p)
	("index" . ?i)
	("survey" . ?s)
	(:endgroup . nil)

	;; 研究领域
	(:startgroup . nil)
	("AI" . ?A)
	("ML" . ?M)
	("NLP" . ?N)
	("CV" . ?V)
	("DL" . ?D)
	("Math" . ?T)
	(:endgroup . nil)

	;; 优先级和重要性
	(:startgroup . nil)
	("keystone" . ?k)
	("important" . ?!)
	("reference" . ?r)
	("foundational" . ?b)
	(:endgroup . nil)

	;; 处理状态
	("inbox" . ?I)
	("todo" . ?t)
	("reading" . ?R)
	("literature" . ?L)
	("todo" . ?t)))

;;==============================================
;; Agenda 视图 - Zettelkasten 工作流
;;==============================================

(setq org-agenda-sticky t
      org-agenda-compact-blocks t
      org-agenda-start-on-weekday 1
      org-agenda-span 'week
      org-agenda-include-diary nil
      org-agenda-window-setup 'current-window
      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
	(todo . " %i %-12:c")
	(tags . " %i %-12:c")
	(search . " %i %-12:c")))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
	(todo category-up priority-down effort-up)
	(tags category-up effort-up)
	(search category-up)))

;; 自定义 Agenda 视图
(setq org-agenda-custom-commands
      '(;; 📥 待处理收件箱
	("i" "📥 Inbox Processing"
	 ((todo "INBOX"
	  ((org-agenda-overriding-header "⚡ 待处理的临时笔记")
	   (org-agenda-skip-function
	    '(lambda () (org-agenda-skip-entry-if 'scheduled 'deadline)))
	   (org-agenda-sorting-strategy '(priority-down effort-up))))
	  (todo "PROCESSING"
	  ((org-agenda-overriding-header "🔄 处理中的笔记")))))

	;; 📖 阅读工作流
	("R" "📖 Reading Workflow"
	 ((todo "READING"
	  ((org-agenda-overriding-header "👀 正在阅读的论文")
	   (org-agenda-skip-function
	    '(lambda () (org-agenda-skip-entry-if 'scheduled 'deadline)))))
	  (todo "EXTRACTING"
	  ((org-agenda-overriding-header "✂️ 提取卡片中")))
	  (tags "TYPE=\"paper-reading\""
	  ((org-agenda-overriding-header "📝 阅读笔记")))))

	;; 💡 永久卡片浏览
	("c" "🔹 Concept Cards"
	 ((tags "TYPE=\"concept\""
	  ((org-agenda-overriding-header "所有概念卡片")
	   (org-tags-match-list-sublevels t)))))

	("q" "❓ Question Cards"
	 ((tags "TYPE=\"question\""
	  ((org-agenda-overriding-header "所有问题卡片")
	   (org-tags-match-list-sublevels t)
	   (org-agenda-sorting-strategy '(priority-down))))))

	("a" "🎯 Argument Cards"
	 ((tags "TYPE=\"argument\""
	  ((org-agenda-overriding-header "所有论证卡片")
	   (org-tags-match-list-sublevels t)))))

	;; 📚 文献管理
	("l" "📚 Reading List"
	 ((tags "Reading"
	  ((org-agenda-overriding-header "📋 待读清单")
	   (org-tags-match-list-sublevels nil)))))

	;; 🌟 最近创建的卡片
	("n" "✨ Recently Added (7 days)"
	 ((tags-todo "CREATED:>=\"<2025-01-10>\""
	  ((org-agenda-overriding-header "最近创建的卡片")))))

	;; 📊 知识图谱概览
	("k" "📊 Knowledge Graph Overview"
	 ((tags "TYPE=\"index\""
	  ((org-agenda-overriding-header "📑 索引页面")
	   (org-tags-match-list-sublevels t)))
	  (tags "TYPE=\"survey\""
	  ((org-agenda-overriding-header "📊 综述和项目")))
	  (tags "keystone"
	  ((org-agenda-overriding-header "🔑 基石卡片")))))))

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;==============================================
;; Org Clock - 时间追踪
;;==============================================

(use-package org-clock
  :straight nil
  :after org
  :custom
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-clock-into-drawer "LOGBOOK")
  (org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  :config
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-in-hook
	    (lambda ()
	      (setq-default header-line-format
			   '((" 🕐 " org-mode-line-string " ")))))
  (add-hook 'org-clock-out-hook
	    (lambda ()
	      (setq-default header-line-format nil))))

;;==============================================
;; Babel - 代码块执行
;;==============================================

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (dot . t)
     (gnuplot . t)
     (latex . t)))

  (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t))

;;==============================================
;; 自定义函数
;;==============================================

;; 获取论文 slug
(defun my/get-paper-slug ()
  "生成论文文件名 slug"
  (or (and (boundp 'citar-current-cite-key) citar-current-cite-key)
      (format-time-string "%Y%m%d")))

;; 卡片统计
(defun my/zettel-card-stats ()
  "显示各类卡片的统计信息"
  (interactive)
  (let* ((inbox-count (org-count-matches "^\\*.*INBOX" nil nil))
	 (concept-count (org-count-matches ":TYPE: concept" nil nil))
	 (question-count (org-count-matches ":TYPE: question" nil nil))
	 (argument-count (org-count-matches ":TYPE: argument" nil nil))
	 (paper-count (org-count-matches ":TYPE: paper" nil nil)))
    (message "📊 Zettelkasten 统计\n📥 待处理: %d\n💡 概念: %d\n❓ 问题: %d\n🎯 论证: %d\n📖 论文: %d\n合计: %d"
	     inbox-count concept-count question-count argument-count paper-count
	     (+ inbox-count concept-count question-count argument-count paper-count))))

(global-set-key (kbd "C-c n C") 'my/zettel-card-stats)

;; 快速导出为 Markdown
(defun my/export-to-markdown ()
  "导出当前文件为 Markdown"
  (interactive)
  (org-export-to-file 'md (concat (file-name-sans-extension (buffer-file-name)) ".md")))

;;==============================================
;; 显示和交互
;;==============================================

;; 禁用行截断
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq truncate-lines nil
		  word-wrap t)
	    (org-display-inline-images t)))

;; 高亮代码块
(setq org-src-preserve-indentation t
      org-edit-src-content-indentation 0)

;;==============================================
;; 导出设置
;;==============================================

(with-eval-after-load 'ox-html
  (setq org-html-postamble nil
	org-html-preamble nil
	org-html-htmlize-output-type 'css))

(provide 'init-org)
;;; init-org.el ends here
