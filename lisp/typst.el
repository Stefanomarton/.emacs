;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :config
  (with-eval-after-load 'eglot
    (with-eval-after-load 'typst-ts-mode
      (add-to-list 'eglot-server-programs
                   `((typst-ts-mode) .
                     ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                            "tinymist"
                                            "typst-lsp"))))))
  )

(provide 'programming)

;;; typst.el ends here
