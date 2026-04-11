;;; markdown.el --- Modulo Markdown -*- lexical-binding: t; -*-
(use-package md-ts-mode
  :ensure t
  ;; Corretta la regex per i file .md
  :mode ("\\.md\\'" . md-ts-mode)
  
  ;; Qui mettiamo solo le funzioni native di Emacs che esistono già
  :hook
  (md-ts-mode . display-line-numbers-mode)
  
  :config
  (use-package jinx
    :ensure t
    :hook (md-ts-mode . jinx-mode)
    
    ;; Aggiunta la parola chiave :bind
    :bind (:map md-ts-mode-map
                ("M-$" . jinx-correct)))

  (use-package aas
    :ensure t
    :hook
    (md-ts-mode . aas-activate-for-major-mode)
    :config
    (aas-set-snippets 'md-ts-mode
                      "jf" (lambda () (interactive)
                             (yas-expand-snippet "\\\\( $1 \\\\) $0"))
                      "kd" (lambda () (interactive)
                             (setq-local yas-indent-line 'auto)
                             (yas-expand-snippet "\\[ \n $1 \n \\]\n $0"))))

  (use-package laas
    :ensure t
    :hook
    (md-ts-mode . laas-mode))

  (use-package selected
    :ensure t
    :hook
    (md-ts-mode . selected-minor-mode)
    :bind 
    (:map selected-md-ts-mode-map
          ("i" . surround-region-with-italic))
    :config
    (setq selected-md-ts-mode-map (make-sparse-keymap))
    (defun surround-region-with-italic ()
      "Surround the active region with hard-coded strings"
      (interactive)
      (yas-expand-snippet "_`(yas-selected-text)`_"))))

(provide 'markdown)
