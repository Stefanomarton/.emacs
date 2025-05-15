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
  (dirvish-override-dired-mode))

;; (use-package dired-narrow
;;   :defer 1
;;   :ensure t
;;   :after dirvish
;;   :config
;;   (defun dired-narrow-ex-ac ()
;;     ;; Revert buffer and enter the directory after narrowing
;;     (revert-buffer)
;;     (dired-find-alternate-file))
;;   (setq dired-narrow-exit-when-1-left t)
;;   (setq dired-narrow-exit-action 'dired-narrow-ex-ac))

(use-package transient
  :ensure t)


(provide 'file-management)
;;; file-management.el ends here
