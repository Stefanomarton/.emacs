;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
            :branch "main")
  :bind (:map typst-ts-mode-map
              ("C-c <tab>" . outline-cycle-buffer)
              ("<backtab>" . outline-cycle)
              ("C-c n" . outline-next-heading)
              ("C-c p" . outline-previous-heading)
              ("C-c t" . typst-generate-table-snippet)
              )

  :hook
  (typst-ts-mode . outline-indent-mode)
  
  :config

  (setq typst-ts-output-directory "/tmp/pdf")
  (setq typst-ts-compile-options "--root=$HOME/.marton-drive/notes/ --pdf-standard=a-2b")
  (setq typst-ts-watch-options (list (concat "--root=" (expand-file-name "~/.marton-drive/notes/")) "--open")))

(provide 'typst-config)

;;; typst.el ends here
