;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; Git Modes (basic Git support)
(use-package git-modes
  :straight t)

;; Git Timemachine (View Git history in time machine style)
(use-package git-timemachine
  :straight t
  :bind (("C-x v t" . git-timemachine-toggle)))

;; Git Link (Generate links to Git files)
(use-package git-link
  :straight t)

;; Magit (Powerful Git integration)
(use-package magit
  :straight t
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-visit-prefer-worktree t)
  (sanityinc/fullframe-mode 'magit-status-mode)
  :bind (("C-x g" . magit-status)
         ("M-<F12>" . magit-status)
         ("C-x M-g" . magit-dispatch)
         :map magit-status-mode-map
         ("C-M-<up>" . magit-section-up))
  :hook (git-commit-mode . goto-address-mode)
  :commands (magit-status magit-dispatch magit-log-buffer-file))

;; Magit Todos (Show TODOs in Magit)
(use-package magit-todos
  :straight t)

;; VC Git Log (Log for Git files, fallback to VC log if not a Git file)
(use-package vc
  :config
  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  :bind (:map vc-prefix-map
              ("l" . sanityinc/magit-or-vc-log-file)))

;; Git SVN (Support for git-svn)
(use-package magit-svn
  :straight t
  :config
  (defvar git-svn--available-commands nil "Cached list of git svn subcommands")
  (defun git-svn--available-commands ()
    (or git-svn--available-commands
        (setq git-svn--available-commands
              (sanityinc/string-all-matches
               "^  \\([a-z\\-]+\\) +"
               (shell-command-to-string "git svn help") 1))))
  (autoload 'vc-git-root "vc-git")

  (defun git-svn (dir command)
    "Run a git svn subcommand in DIR."
    (interactive (list (read-directory-name "Directory: ")
                       (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
    (let* ((default-directory (vc-git-root dir))
           (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
      (compile (concat "git svn " command)))))

;; Convenient binding for vc-git-grep
(use-package vc
  :bind (:map vc-prefix-map
              ("f" . vc-git-grep)))

;; Ensure Git-Svn error patterns are correctly recognized
(use-package compile
  :config
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))


(provide 'init-git)
;;; init-git.el ends here
