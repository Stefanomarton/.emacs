;;; typst-extensions.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package outline-indent-mode
  :vc (:url "https://git.sr.ht/~meow_king/outline-indent-mode" :branch "main")
  :ensure t)

(use-package typst-download
  :after typst-ts-mode
  :ensure t
  :vc (:url "https://github.com/Stefanomarton/typst-download" :branch "main")
  :config
  
  (defun my/typst-download-clipboard ()
    (interactive)
    (org-set-attachments-folder)
    (setq-local typst-download-image-dir org-attachments-folder)
    (typst-download-clipboard)
    )

  :bind
  (:map typst-ts-mode-map
        ("C-c y" . my/typst-download-clipboard))

  )

(provide 'typst-extensions)
