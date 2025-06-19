;;; file-management.el -*- lexical-binding: t; -*-

;; Better dired
(use-package dirvish
  :ensure t
  :bind
  (:map global-map
        ("C-x C-j" . dirvish-dwim))
  :config
  (setq dirvish-default-layout '(0 0.2 0.6))
  (setq dirvish-use-mode-line nil)
  :init
  (dirvish-override-dired-mode)
  )

(use-package zoxide
  :ensure t
  :bind ("<escape> z" . zoxide-find-file)   ; your keybinding
  :config
  (defun my/zoxide-find-file (dir)
    "Start the usual ‘find-file’ prompt rooted at DIR (a directory)."
    ;; zoxide passes DIR as its one argument
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (call-interactively #'find-file)))
  
  (setq zoxide-find-file-function #'my/zoxide-find-file))

(provide 'file-management)
;;; file-management.el ends here
