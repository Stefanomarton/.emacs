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

  ;; ── Typst support for citar ───────────────────────────────────────────
  (with-eval-after-load 'citar             ; run only after citar is loaded
    ;; 1 ─ Helpers --------------------------------------------------------
    (defun citar-typst-insert-citation (keys _context)
      "Insert Typst `#cite[...]` for KEYS (a list of cite keys)."
      (insert (format "#cite(<%s>)" (string-join keys ","))))

    (defun citar-typst-key-at-point ()
      "Return cite keys if point is inside a `#cite[...]`."
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (sym    (buffer-substring-no-properties
                           (car bounds) (cdr bounds))))
        (when (save-excursion
                (goto-char (car bounds))
                (search-backward "#cite[" (line-beginning-position) t))
          (split-string sym "[, ]+" t))))

    (defun citar-typst-citation-at-point ()
      "Return full `#cite[foo,bar]` string at point, or nil."
      (save-excursion
        (when (re-search-backward "#cite\\[" (line-beginning-position) t)
          (let ((start (point)))
            (ignore-errors (forward-sexp))
            (buffer-substring-no-properties start (point))))))

    ;; 2 ─ Entry for the alist -------------------------------------------
    (let ((typst-entry
           '((typst-ts-mode typst-mode) .
             ((insert-citation . citar-typst-insert-citation)
              (key-at-point    . citar-typst-key-at-point)
              (citation-at-point . citar-typst-citation-at-point)))))

      ;; 3 ─ Replace any existing Typst entry, leave others intact -------
      (setq citar-major-mode-functions
            (cons typst-entry
                  (cl-remove-if
                   (lambda (e)
                     (and (listp (car e))      ; only touch proper mode lists
                          (member 'typst-ts-mode (car e))))
                   citar-major-mode-functions)))))

  
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
