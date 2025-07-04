;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(with-eval-after-load 'typst-ts-mode

  (defvar typst-citar-citation-style "#sidecite(<%s>)")
  
  ;; ── Typst support for citar ───────────────────────────────────────────
  (with-eval-after-load 'citar             ; run only after citar is loaded
    ;; 1 ─ Helpers --------------------------------------------------------
    (defun citar-typst-insert-citation (keys _context)
      "Insert Typst `#cite[...]` for KEYS (a list of cite keys)."
      (insert (format typst-citar-citation-style (string-join keys ","))))

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
                   citar-major-mode-functions))))))

(provide 'typst-citar)
