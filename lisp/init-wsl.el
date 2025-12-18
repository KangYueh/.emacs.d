;;; init-wsl.el

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.png\\'" . "wslview %s")
        ("\\.jpg\\'" . "wslview %s")
        ("\\.svg\\'" . "wslview %s")
        ("\\.pdf\\'" . "wslview %s")))

(provide 'init-wsl)
;;; init-wsl.el ends here
