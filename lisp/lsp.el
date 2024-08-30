;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-
(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook
  (python-ts-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  :config
  (setq eglot-workspace-configuration
        '((pylsp
           (plugins
            (jedi_completion (fuzzy . t))
            (pydocstyle (enabled . t)))))))

(use-package eldoc
  :ensure nil)

(use-package jsonrpc
  :ensure t
  :after eglot)

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  )

(provide 'lsp)

;;; lsp.el ends here
