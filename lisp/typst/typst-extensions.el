;;; typst-extensions.el --- Programming languages configuration  -*- lexical-binding: t; -*-

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

(defun sm/typst-extract-region-to-file (beg end filename)
  "Save region BEG..END to FILENAME and replace it with an #include statement.
FILENAME is prompted relative to the current buffer's directory.
A .typ extension is appended automatically if omitted."
  (interactive
   (if (use-region-p)
       (list (region-beginning)
             (region-end)
             (read-file-name "Extract to file: "
                             (file-name-directory (buffer-file-name))
                             nil nil ""))
     (user-error "No active region")))
  (let* ((dir     (file-name-directory (buffer-file-name)))
         (abs     (expand-file-name
                   (if (string-suffix-p ".typ" filename)
                       filename
                     (concat filename ".typ"))))
         (rel     (file-relative-name abs dir))
         (content (buffer-substring-no-properties beg end)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    (delete-region beg end)
    (insert (format "#include \"%s\"" rel))
    (message "Extracted to \"%s\"" rel)))

(provide 'typst-extensions)
