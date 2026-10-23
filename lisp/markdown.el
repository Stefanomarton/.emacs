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
    :ensure nil   ; from Nix
    :hook (md-ts-mode . jinx-mode)
    
    ;; Aggiunta la parola chiave :bind
    :bind (:map md-ts-mode-map
                ("M-$" . jinx-correct)))

  (use-package selected
    :ensure t
    :hook
    (md-ts-mode . selected-minor-mode)
    :bind 
    (:map selected-md-ts-mode-map
          ("i" . surround-region-with-md-italic))
    :config
    (setq selected-md-ts-mode-map (make-sparse-keymap))
    (defun surround-region-with-md-italic ()
      "Surround the active region with hard-coded strings"
      (interactive)
      (yas-expand-snippet "_`(yas-selected-text)`_"))))

(provide 'markdown)
