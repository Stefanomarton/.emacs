;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :config
  (setq typst-ts-output-directory "/tmp/pdf"
        typst-ts-compile-options "--root=/home/sm/.marton-drive/notes/"
        ))

(provide 'programming)

;;; typst.el ends here
