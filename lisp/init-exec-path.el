;;; init-exec-path.el --- Set up exec-path to help Emacs find programs -*- lexical-binding: t -*-

(use-package exec-path-from-shell
  :straight t
  :defer t
  :config
  ;; 先指定要同步的环境变量
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID"
                 "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                 "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; 在 GUI 或 daemon 启动时初始化
  (when (or (memq window-system '(mac ns x pgtk))
            (and (not (memq system-type '(ms-dos windows-nt)))
                 (daemonp)))
    (exec-path-from-shell-initialize)))


(provide 'init-exec-path)
;;; init-exec-path.el ends here

