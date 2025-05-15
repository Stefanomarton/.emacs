;;; beamer.el --- org-mode beamer configuration -*- lexical-binding: t; -*-

(use-package ox-beamer
  :defer t
  :ensure nil
  :config
  (defun my-beamer-maketitle-filter (output backend info)
    "Filter to modify \\maketitle for Beamer exports in Org mode."
    (if (org-export-derived-backend-p backend 'framed-ex)
        (replace-regexp-in-string
         "\\\\maketitle"
         "\\\\begin{frame}[plain]\n\\\\titlepage\n\\\\end{frame}"
         output)
      output))

  (add-to-list 'org-export-filter-final-output-functions
               'my-beamer-maketitle-filter)

  ;; (defvar org-beamer-title-format "\\frame[plain]{\\titlepage}")

  (defun org-framed-export-to-pdf
      (&optional async subtreep visible-only body-only ext-plist)
    (interactive)
    (let ((file (org-export-output-file-name ".tex" subtreep)))
      (org-export-to-file 'framed-ex file
        async subtreep visible-only body-only ext-plist
        #'org-latex-compile)))

  (org-export-define-derived-backend 'framed-ex 'beamer
    :menu-entry
    '(?l 1
         ((?h "As PDF file (Beamer)" org-framed-export-to-pdf)
          (?H "As PDF file and open (Beamer)"
              (lambda (a s v b)
                (if a (org-framed-export-to-pdf t s v b)
  	              (org-open-file (org-framed-export-to-pdf nil s v b)))))))
    :options-alist
    '(
      ;; (:latex-title-command nil org-beamer-title-format t)
      (:date nil nil "\\today" t)
      (:)
      ))
  )

(provide 'org-beamer)

;;; beamer.el ends here
