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

;; (use-package openwith
;;   :config
;;   (openwith-mode 1)
;;   (setq openwith-associations '(
;;                                 ("\\.pdf\\'" "zathura" (file))
;;                                 ("https'" "floorp" (file))
;;                                 )
;;         ))

(use-package dired-narrow
  :ensure t
  :after dirvish
  :config
  (defun dired-narrow-ex-ac ()
    ;; Revert buffer and enter the directory after narrowing
    (revert-buffer)
    (dired-find-alternate-file))
  (setq dired-narrow-exit-when-1-left t)
  (setq dired-narrow-exit-action 'dired-narrow-ex-ac))

(use-package transient
  :ensure t)

(use-package casual-lib
  :ensure t)

(use-package casual-dired
  :after casual-lib
  :ensure (:host github :repo "kickingvegas/casual-dired")
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(provide 'file-management)
;;; file-management.el ends here
