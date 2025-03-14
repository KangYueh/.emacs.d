;;; init-roam.el --- Org-roam config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;-------------------------zotero--------------------------------
(setq zot_bib '("/home/einhep/wdata/zotero/all.bib") ; Zotero 用 Better BibTeX 导出的 .bib 文件. 可以是多个文件
      zot_pdf "/home/einhep/wdata/zotero/" ; Zotero 的 ZotFile 同步文件夹
      org_refs "/home/einhep/wdata/roam/paper/" ) ; 自定义的 org-roam 文献笔记目录. 我的 org-roam 根目录是 ~/repos/notes

;; 第二步: 让 helm-bibtex 读取 Zotero 的信息
(use-package ivy-bibtex; 这里也可以用 ivy-bibtex 替换 helm-bibtex
  :ensure t
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

;; 第三步: 让 org-roam-bibtex 使用 helm-bibtex 的信息, 并绑定 orb 的快捷键
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-action))
  :custom
  (orb-insert-interface 'helm-bibtex) ; 与上面 helm-bibtex/ivy-bibtex 的选择保持一致
  (orb-insert-link-description 'citekey) ; 默认是用标题, 但是论文的标题一般很长, 不适合作为笔记链接的名字
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

;;-----------------------------------org-roam-ui_setting----------------
(use-package org-roam-ui
  :ensure t ;; auto install
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; sync emacs theme
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

;;------------------------org-roam-setting--------------
(use-package org-roam
  :ensure t
  :after org
  :demand t ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/home/einhep/wdata/roam/")
  (org-roam-dailies-directory "journals/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;;------------------------func-used-in-roam---------------------------
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (setq tag tag-name)
  (lambda (node)
    (member tag (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   nil
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))
;;select a file, copy to asset folder of org-roam directory and rename
;;then insert file link to current cursor
(defun my/org-roam-insert-link-and-move-file ()
  "Prompt to select a file, move it to Org-roam's asset directory, and insert a link to it at point."
  (interactive)
  (let* ((file-path (read-file-name "Select file: "))
         (asset-dir (concat org-roam-directory "asset/"))
         (file-name (file-name-nondirectory file-path)))
    (when (and file-path asset-dir)
      ;; Create asset directory if it doesn't exist
      (unless (file-directory-p asset-dir)
        (make-directory asset-dir t))
      ;; Generate random suffix
      (setq random-suffix (format "%010d" (random (expt 10 10))))
      ;; Construct new file name with random suffix
      (setq new-file-name (concat (file-name-base file-name) "-" random-suffix "." (file-name-extension file-name)))
      ;; Move file to asset directory with new name
      (rename-file file-path (concat asset-dir new-file-name) t)
      ;; Insert link to file
      (insert (format "[[file+sys:%s]]" (concat "asset/" new-file-name))))))

(global-set-key (kbd "C-c n c") 'my/org-roam-insert-link-and-move-file)

(provide 'init-roam)
;;;init-roam.el ends here
