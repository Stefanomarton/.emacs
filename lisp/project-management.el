;;; project-management.el --- Project management packages -*- lexical-binding: t; -*-

(provide 'project-management)

(use-package projectile
  :config
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-completion-system 'consult)
  (projectile-mode)

  (use-package consult-projectile
    :bind
    ("C-x p p" . consult-projectile)
    ("C-x p f" . consult-projectile-find-file))

  (setq projectile-indexing-method 'native)
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  )

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (org-mode . hl-todo-mode)
  (LaTeX-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FF0000")
     ("FIXME"  . "#FF0000")
     ("ASK"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("STUB"   . "#1E90FF"))))


(use-package magit
  :commands (magit-status magit-file-dispatch magit-dispatch dotfiles-magit-status magit-status-with-removed-dotfiles-args)
  :config
  (magit-auto-revert-mode)
  (setq magit-commit-ask-to-stage 'stage)
  ;; prepare the arguments
  (setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
  (setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

  ;; function to start magit on dotfiles
  (defun dotfiles-magit-status ()
    (interactive)
    (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
    (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
    (call-interactively 'magit-status))

  ;; wrapper to remove additional args before starting magit
  (defun magit-status-with-removed-dotfiles-args ()
    (interactive)
    (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
    (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
    (call-interactively 'magit-status))

  ;; Fixing keybinding
  (evil-define-key 'normal magit-mode-map (kbd "l") 'magit-section-backward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "k") 'magit-section-forward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "SPC") 'magit-section-cycle)
  )

(use-package magit-delta
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode))

(use-package git-gutter+
  :hook ((prog-mode text-mode-hook) . git-gutter-mode)
  :after git-gutter
  :config
  (git-gutter+-mode))

(use-package git-gutter
  ;; :hook ((prog-mode markdown-mode LaTeX-mode) . git-gutter-mode)
  :config
  (git-gutter-mode)
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    ("n" git-gutter:next-hunk "next hunk")
    ("p" git-gutter:previous-hunk "previous hunk")
    ("h" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "first hunk")
    ("l" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "last hunk")
    ("<SPC>" git-gutter:popup-hunk "popup hunk")
    ("s" git-gutter:stage-hunk "stage hunk")
    ("r" git-gutter:revert-hunk "revert hunk")
    ("c" git-gutter+-commit "commit hunk")
    ("C" git-gutter+-stage-and-commit "stange and commit hunk")
    ("q" nil "quit"))
  (setq git-gutter:update-interval 0.02)
  :bind
  (("<leader>gn" . hydra-git-gutter/body)))

(use-package git-gutter-fringe
  :config
  (fringe-mode nil)
  (setq-default left-margin-width 1)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))


;;; project-management.el ends here
