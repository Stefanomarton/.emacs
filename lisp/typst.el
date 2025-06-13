;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://git.sr.ht/~meow_king/typst-ts-mode"
            :branch "main")
  :config
  (setq typst-ts-output-directory "/tmp/pdf")
  (setq typst-ts-compile-options "--root=/home/sm/.marton-drive/notes/")
  )

(use-package outline-indent
  :ensure t)

;;; typst.el ends here
