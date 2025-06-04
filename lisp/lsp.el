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
               `((typst-ts-mode) .
                 ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                        "tinymist"
                                        "typst-lsp"))))
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

;; (use-package eglot-booster
;;   :ensure (:host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-mode)
;;   )

;; (use-package lsp-bridge
;;   :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;                        :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                        :build (:not compile))
;;   :config
;;   (setq lsp-bridge-tex-lsp-server "digestif")
;;   (global-lsp-bridge-mode)
;;   (acm-mode))

(provide 'lsp)

;;; lsp.el ends here
