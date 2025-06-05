;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :vc (:url "https://git.sr.ht/~meow_king/typst-ts-mode"
            :branch "main")
  :config
  (setq typst-ts-output-directory "/tmp/pdf"
        typst-ts-compile-options "--root=/home/sm/.marton-drive/notes/"
        ))

(provide 'programming)

;;; typst.el ends here
