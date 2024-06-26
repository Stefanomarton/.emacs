;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-
(use-package eglot
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

(provide 'lsp)

;;; lsp.el ends here
