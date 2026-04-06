;;; document-production.el --- document production configuration -*- lexical-binding: t; -*-

(use-package md-ts-mode
  :ensure t
  :hook
  (md-ts-mode . display-line-numbers-mode))

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

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)
  :hook
  (text-mode . yas-minor-mode)
  (prog-mode . yas-minor-mode)
  (LaTeX-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  (yas-minor-mode . yas-reload-all)
  (snippet-mode . disable-final-newline)

  :preface
  (defun make-silent (func &rest args)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) nil)))
      (apply func args)))
  (advice-add 'yas-reload-all :around #'make-silent)
  (add-hook 'org-mode-hook (lambda () (setq-local yas-indent-line 'fixed)))

  :config

  ;; disable `highlight' to avoid confusing with region
  (custom-set-faces
   '(yas-field-highlight-face         ; the face you’re changing
     ((t (:underline t)))             ; the spec: apply to all “t” display classes
     "Face used to highlight active snippet fields."))


  (defun disable-final-newline ()
    (interactive)
    (set (make-local-variable 'require-final-newline) nil))
  (yas-global-mode 1)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")))

(use-package warnings
  :ensure nil
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package auto-yasnippet
  :ensure t
  :bind
  (:map global-map
        ("<escape> y c" . my-aya-create)
        ("<escape> y e" . aya-expand)
        ("<escape> y E" . aya-expand-from-history)
        ("<escape> y d" . aya-delete-from-history)
        ("<escape> y h" . aya-clear-history)
        ("<escape> y n" . aya-next-in-history)
        ("<escape> y p" . aya-previous-in-history)
        ("<escape> y p" . aya-persist-snippet)
        ("<escape> y o" . aya-open-line))
  :config
  (defun my-aya-create (beg end)
    (interactive "r")
    (let ((count 0))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "\\b\\([0-9]+\\)\\b" nil t)
          (replace-match "~\\1")
          (setq count (1+ count)))
        (aya-create beg (+ count end))
        (delete-region beg end)
        (aya-expand count)))
    (recenter-top-bottom)
    ))

(use-package aas
  :ensure t
  :hook
  (org-mode . aas-activate-for-major-mode)
  (markdown-mode . aas-activate-for-major-mode)
  (LaTeX-mode . aas-activate-for-major-mode)
  :config


  (aas-set-snippets 'LaTeX-mode
                    "jf" (lambda () (interactive)
                           (yas-expand-snippet "\\\\($1\\\\) $0"))
                    "jc" (lambda () (interactive)
                           (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
                    "kd  " (lambda () (interactive)
                             (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'org-mode
                    "jf" (lambda () (interactive)
                           (yas-expand-snippet "\\\\( $1 \\\\) $0"))
                    "jc" (lambda () (interactive)
                           (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
                    "kd" (lambda () (interactive)
                           (setq-local yas-indent-line 'auto)
                           (yas-expand-snippet "\\[ \n $1 \n \\]\n $0")))
  (aas-set-snippets 'markdown-mode
                    "jf" (lambda () (interactive)
                           (yas-expand-snippet "$ $1$ $0 $"))
                    "jc" (lambda () (interactive)
                           (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
                    "kd" (lambda () (interactive)
                           (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))


(use-package laas
  :ensure t
  :hook
  (LaTeX-mode . laas-mode)
  (markdown-mode . laas-mode)
  (org-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math

                    ",t" (lambda () (interactive)
                           (yas-expand-snippet "\\int"))

                    ".." (lambda () (interactive)
                           (yas-expand-snippet "_{$1}$0"))
                    "ds" (lambda () (interactive)
                           (yas-expand-snippet "\\Delta S $0"))
                    "dh" (lambda () (interactive)
                           (yas-expand-snippet "\\Delta H $0"))
                    "dg" (lambda () (interactive)
                           (yas-expand-snippet "\\Delta G $0"))

                    ;; positive apices
                    ",," (lambda () (interactive)
                           (yas-expand-snippet "^{$1}$0"))
                    ",x" (lambda () (interactive)
                           (yas-expand-snippet "^{1}$0"))
                    ",c" (lambda () (interactive)
                           (yas-expand-snippet "^{2}$0"))
                    ",v" (lambda () (interactive)
                           (yas-expand-snippet "^{3}$0"))
                    ",s" (lambda () (interactive)
                           (yas-expand-snippet "^{4}$0"))
                    ",d" (lambda () (interactive)
                           (yas-expand-snippet "^{5}}$0"))
                    ",f" (lambda () (interactive)
                           (yas-expand-snippet "^{6}$0"))
                    ",w" (lambda () (interactive)
                           (yas-expand-snippet "^{7}$0"))
                    ",e" (lambda () (interactive)
                           (yas-expand-snippet "^{8}$0"))
                    ",r" (lambda () (interactive)
                           (yas-expand-snippet "^{9}$0"))

                    ;; negative apices
                    ".." (lambda () (interactive)
                           (yas-expand-snippet "^{-$1}$0"))
                    ".x" (lambda () (interactive)
                           (yas-expand-snippet "^{-1}$0"))
                    ".c" (lambda () (interactive)
                           (yas-expand-snippet "^{-2}$0"))
                    ".v" (lambda () (interactive)
                           (yas-expand-snippet "^{-3}$0"))
                    ".s" (lambda () (interactive)
                           (yas-expand-snippet "^{-4}$0"))
                    ".d" (lambda () (interactive)
                           (yas-expand-snippet "^{-5}$0"))
                    ".f" (lambda () (interactive)
                           (yas-expand-snippet "^{-6}$0"))
                    ".w" (lambda () (interactive)
                           (yas-expand-snippet "^{-7}$0"))
                    ".e" (lambda () (interactive)
                           (yas-expand-snippet "^{-8}$0"))
                    ".r" (lambda () (interactive)
                           (yas-expand-snippet "^{-9}$0"))

                    ".," (lambda () (interactive)
                           (yas-expand-snippet "^{$1}_{$0}"))

                    "kk" (lambda () (interactive)
                           (yas-expand-snippet "_{$1}$0"))

                    "++" (lambda () (interactive)
                           (yas-expand-snippet "^+ $0"))

                    "--" (lambda () (interactive)
                           (yas-expand-snippet "^- $0"))

                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    ".q" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                    ".v" (lambda () (interactive) (laas-wrap-previous-object "vec"))
                    ".t" (lambda () (interactive) (laas-wrap-previous-object "text"))
                    ".b" (lambda () (interactive) (laas-wrap-previous-object "mathbf")))
  )

(use-package jinx
  :ensure t
  :hook
  (org-mode . jinx-mode)
  :bind
  (:map org-mode-map
        ("M-$" . jinx-correct))
  (:map text-mode-map
        ("M-$" . jinx-correct))
  :init
  ;; must load it before starting jinx-mode
  (setq jinx-languages "it en_US"))


(provide 'document-production)

;;; document-production.el ends here
