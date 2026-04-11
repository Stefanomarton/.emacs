;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
            :branch "main")
  :hook
  (typst-ts-mode . display-line-numbers-mode)
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

              ("C-c x" . sm/typst-extract-region-to-file)
              )


  :hook
  (typst-ts-mode . outline-indent-mode)

  :config
  ;; (setq typst-ts-output-directory "/tmp/pdf")
  (setenv "TYPST_PROJECT_ROOT" denote-directory)
  (setq typst-ts-compile-options "--root=$TYPST_PROJECT_ROOT --pdf-standard=a-2b")
  (setq typst-ts-watch-options (list "--root=/home/sm/.marton-drive/work/its/courses/chimica-dei-metalli"))

  (require 'typst-citar)
  (require 'typst-extensions)
  (require 'typst-movement)
  (require 'typst-select)
  (require 'typst-consult)
  (require 'typst-snippets)
  (require 'typst-surround)
  )

(provide 'typst-config)

;;; typst.el ends here
