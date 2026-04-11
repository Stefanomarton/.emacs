;;; project-management.el --- Project management packages -*- lexical-binding: t; -*-

;; (use-package projectile
;;     :ensure t
;;     :config
;;     (projectile-mode)
;;     (setq projectile-track-known-projects-automatically nil)
;;     (setq projectile-completion-system 'consult)
;;     (setq projectile-indexing-method 'alien)

;;     (use-package consult-projectile
;;         :ensure t
;;         :bind
;;         ("C-x p p" . consult-projectile)
;;         ("C-x p f" . consult-projectile-find-file)
;;         ("C-x p t" . projectile-run-vterm-other-window)
;;         ("C-x p s" . consult-projectile-switch-project)
;;         ("C-x p a" . projectile-add-known-project))

;;     :init
;;     (add-hook 'after-init-hook 'projectile-mode)
;;     )

(use-package project
  :ensure nil
  :config
  (setq project-vc-ignores '("node_modules/" "target/" "dist/" ".cache/" "elpa"))
  
  (defun my/project-try-multi-marker (dir)
    (let ((root (or (locate-dominating-file dir ".project")
                    (locate-dominating-file dir "flake.nix")
                    )))
      (when root
        (cons 'transient root))))

  (setq project-find-functions '(my/project-try-multi-marker project-try-vc))

  (use-package consult-project-extra
    :ensure t
    :custom (consult-project-function #'consult-project-extra-project-fn) ;; Optional but recommended for a more consistent UI
    :bind
    (("C-x p f" . consult-project-extra-find)
     ("C-x p o" . consult-project-extra-find-other-window)))
  )

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FF0000")
     ("FIXME"  . "#FF0000")
     ("ASK"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("STUB"   . "#1E90FF"))))

(use-package magit
  :ensure t
  :bind ("<escape> G" . magit)
  :config
  (magit-auto-revert-mode)
  (setq magit-commit-ask-to-stage 'stage))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode text-mode) . git-gutter-mode)
  :config
  (git-gutter-mode)
  (setq git-gutter:update-interval 0.02)
  (setq-default left-margin-width 1)

  (use-package git-gutter-fringe
    :ensure t
    :config
    (setq git-gutter:update-interval 0.02)
    (fringe-mode nil)
    (setq-default left-margin-width 1)
    ;; (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    ;; (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    ;; (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated))
    )
  )


(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-keep-variants nil)
  )

(provide 'project-management)
;;; project-management.el ends here
