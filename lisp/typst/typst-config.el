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
              ("C-c o" . consult-outline)
              
              ("C-c t" . typst-snippets-table)
              
              ("C-c e" . typst-consult-equations-jump)
              ("C-c E" . typst-consult-equations-insert)
              ("C-c l" . typst-consult-insert-ref)
              ("C-c L" . typst-consult-labels-jump)
              
              ("C-c m" . typst-select-math-after)
              ("C-c M" . typst-select-math-before)

              ("C-c c" . citar-insert-citation)

              )

  

  :hook
  (typst-ts-mode . outline-indent-mode)
  
  :config
  (setq typst-ts-output-directory "/tmp/pdf")
  (setq typst-ts-compile-options "--root=$HOME/.marton-drive/ --pdf-standard=a-2b")
  (setq typst-ts-watch-options
        (list (concat "--root=" (expand-file-name "~/.marton-drive/")) "--open"))
  )

(provide 'typst-config)

;;; typst.el ends here
