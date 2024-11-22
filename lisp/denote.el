;;; denote.el -*- lexical-binding: t; -*-;;;

(use-package denote
  :demand t
  :ensure (:wait t)

  :bind
  ("<escape>do" . my/denote-open-or-create)
  ("<escape>dj" . denote-journal-extras-new-or-existing-entry)
  ("<escape>dr" . denote-region)
  ("<escape>dt" . denote-type)

  :hook
  (dired-mode . denote-dired-mode)
  (text-mode-hook . denote-fontify-links-mode-maybe)

  :config
  ;; Modify default command to use `denote-subdirectory' when creating TARGET
  (defun my/denote-open-or-create (target)
    (interactive (list (denote-file-prompt nil nil :no-require-match)))
    (if (and target (file-exists-p target))
        (find-file target)
      (denote--command-with-features #'denote-subdirectory :use-last-input-as-def-title nil nil nil)))


  (setq denote-directory (concat drive-folder "denote"))
  (setq denote-excluded-directories-regexp ".output")

  (setq denote-backlinks-show-context t)

  ;; journal config
  (setq denote-journal-extras-directory (expand-file-name "personal/journal" denote-directory))
  (setq denote-journal-extras-title-format "%Y %m %d")

  (denote-rename-buffer-mode 1)
  )

(use-package consult-denote
  :requires (denote consult)
  :ensure t
  :bind
  ("<escape>dg" . consult-denote-grep)
  :config
  (setq consult-denote-grep-command #'consult-ripgrep) ; use ripgrep
  (consult-denote-mode))

(use-package citar-denote
  :requires (citar denote)
  :ensure t
  :bind
  ("<escape>dp" . citar-create-note)
  :custom
  (citar-open-always-create-notes t)
  (citar-denote-subdir "uni/papers")
  )

(provide 'denote)
