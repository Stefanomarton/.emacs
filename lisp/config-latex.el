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
        TeX-source-correlate-method 'synctex)

  (use-package laas
    :ensure t
    :hook
    (LaTeX-mode . laas-mode))


  (use-package aas
    :ensure t
    :hook
    (LaTeX-mode . aas-activate-for-major-mode)
    :config
    (aas-set-snippets 'LaTeX-mode
                      "jf" (lambda () (interactive)
                             (yas-expand-snippet "\\\\($1\\\\) $0"))
                      "jc" (lambda () (interactive)
                             (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
                      "kd" (lambda () (interactive)
                             (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))))


(provide 'config-latex)
