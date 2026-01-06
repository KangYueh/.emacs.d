;;; init-tramp.el --- some basic setting for tramp

;; TRAMP 远程 shell 使用 bash

(setq tramp-default-method "ssh")
(setq tramp-remote-shell "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")
(setq tramp-remote-shell-login t)   ;; -l 参数，启动登录 shell
(setq tramp-remote-shell-args '("-c")) ;; 使用 -c 运行命令
(provide 'init-tramp)
;;; init-tramp.el ends here
