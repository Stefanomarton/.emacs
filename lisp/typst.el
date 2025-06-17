;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
            :branch "main")
  :bind (:map typst-ts-mode-map
              ("C-c <tab>" . outline-cycle))

  :hook
  (typst-ts-mode . outline-indent-mode)
  :config
  (setq typst-ts-output-directory "/tmp/pdf")
  (setq typst-ts-compile-options "--root=$HOME/.marton-drive/notes/"))

(use-package outline-indent-mode
  :vc (:url "https://git.sr.ht/~meow_king/outline-indent-mode" :branch "main")
  :ensure t)

;;; typst.el ends here
