;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-
(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook
  (python-ts-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  (typst-ts-mode . eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs

               '((typst-ts-mode) . ("tinymist")))
  
  (setq-default eglot-workspace-configuration
                '(:tinymist (:exportPdf "onSave")))
  )

(use-package eglot-booster
  :ensure t
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "master")
  :after eglot
  :config (eglot-booster-mode))

(use-package eldoc
  :ensure nil)

(use-package jsonrpc
  :ensure t
  :after eglot)

(provide 'lsp)

;;; lsp.el ends here
