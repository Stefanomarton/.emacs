;;; file-management.el -*- lexical-binding: t; -*-

;; Better dired
(use-package dirvish
  :commands (dired-jump dirvish-dwim)
  :config
  (setq dirvish-default-layout '(0.8 0.2 0.5))
  :init
  (dirvish-override-dired-mode)
  )

(use-package openwith
  :config
  (openwith-mode 1)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

(use-package dired-narrow
  :after dirvish
  :config
  (defun dired-narrow-ex-ac ()
    ;; Revert buffer and enter the directory after narrowing
    (revert-buffer)
    (dired-find-alternate-file))
  (setq dired-narrow-exit-when-1-left t)
  (setq dired-narrow-exit-action 'dired-narrow-ex-ac)
  )

;; (use-package dired
;;   :commands dired
;;   :straight nil
;;   :ensure nil
;;   :config
;;   (put 'dired-find-alternate-file 'disabled nil)
;;   )

(provide 'file-management)

;;; file-management.el ends here
