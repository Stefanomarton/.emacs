;; -*- lexical-binding: t; -*-
(use-package auctex
  :ensure t
  :hook
  (latex-mode . display-line-numbers-mode)
  (LaTeX-mode-hook . prettify-symbols-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(latex-mode . LaTeX-mode))

  (setq TeX-save-query nil
        TeX-clean-confirm nil
        TeX-command-extra-options "--shell-escape"
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex))


(provide 'config-latex)
