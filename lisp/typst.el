;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
            :branch "main")
  :bind (:map typst-ts-mode-map
              ("C-c <tab>" . outline-cycle-buffer)
              ("<backtab>" . outline-cycle)
              ("C-c n" . outline-next-heading)
              ("C-c p" . outline-previous-heading)
              )

  :hook
  (typst-ts-mode . outline-indent-mode)
  :config

  (setq typst-ts-output-directory "/tmp/pdf")
  (setq typst-ts-compile-options "--root=$HOME/.marton-drive/notes/ --pdf-standard=a-2b")

  (defvar-keymap my/cycle-repeat-map
    :repeat (:enter (outline-cycle-buffer outline-next-heading outline-previous-heading) :exit (consult-outline))
    "<tab>" #'outline-cycle-buffer
    "n" #'outline-next-heading
    "p" #'outline-previous-heading
    "<down>" #'outline-move-subtree-down
    "<up>" #'outline-move-subtree-up
    ">" #'outline-demote
    "<" #'outline-promote
    "o" #'consult-outline)

  (add-hook 'typst-ts-mode-hook
            (lambda ()
              (embrace-add-pair ?M "\$" "\$" t t)
              (embrace-add-pair ?m "\\\(" "\\\)" t nil)
              (embrace-add-pair ?P "\\left\(" "\\right\)" t nil)
              (embrace-add-pair ?p "\(" "\)" t nil)
              (embrace-add-pair ?S "\\left[" "\\right]" t nil)
              (embrace-add-pair ?s "[" "]" t nil)
              (embrace-add-pair ?t "\\text{" "}" t nil)
              )
            )
  )

(use-package outline-indent-mode
  :vc (:url "https://git.sr.ht/~meow_king/outline-indent-mode" :branch "main")
  :ensure t)

(use-package typst-download
  :after typst-ts-mode
  :ensure t
  :vc (:url "https://github.com/Stefanomarton/typst-download" :branch "main")
  :config
  
  (defun my/typst-download-clipboard ()
    (interactive)
    (org-set-attachments-folder)
    (setq-local typst-download-image-dir org-attachments-folder)
    (typst-download-clipboard)
    )

  :bind
  (:map typst-ts-mode-map
        ("C-c y" . my/typst-download-clipboard))

  )

(provide 'typst)

;;; typst.el ends here
