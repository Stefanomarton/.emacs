;;; document-production.el --- document production configuration -*- lexical-binding: t; -*-
(use-package yasnippet
  :ensure t
  :defer 1   ; yas-global-mode below covers every buffer

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
  (add-hook 'snippet-mode-hook #'disable-final-newline)

  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  ;; load the snippets once (it used to reload them all on every buffer)
  (yas-reload-all)
  (yas-global-mode 1))

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
        ("<escape> y s" . aya-persist-snippet)
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

(use-package jinx
  :defer t
  :ensure nil   ; from Nix (editors.nix), with its native module
  :init
  (setq jinx-languages "it en_US"))

(provide 'document-production)

;;; document-production.el ends here
