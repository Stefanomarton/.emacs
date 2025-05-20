;;; org-export.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package org-src
  :after org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (python . t) (latex . t)(gnuplot . t)(plantuml . t)))

  (setq org-confirm-babel-evaluate nil)

  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-src-fontify-natively t)
  (setq-default
   org-src-tab-acts-natively t
   org-src-preserve-indentation t))

(use-package ox
  :ensure nil
  :after org
  :commands org-export-dispatch
  :bind (:map org-mode-map
              ("<escape>el" . my/org-latex-export)
              ("<escape>eL" . my/org-latex-export-and-open)
              )
  :config
  (setq org-export-with-broken-links t)
  (setq org-export-in-background t)
  (setq org-export-headline-levels 6)
  (setq org-export-async-debug t)
  (setq org-export-async-init-file "~/.config/emacs/async-init.el")

  ;; modify export folder for org export
  ;; taken from https://stackoverflow.com/questions/9559753/emacs-org-mode-export-to-another-directory
  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "/tmp/pdf")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))

  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

  (defun copy-new-pdf-files ()
    "Copy new PDF files from /tmp/pdf to home/stefanom/pdf."
    (interactive)
    (let ((source-directory "/tmp/pdf/")
          (destination-directory (concat notes-folder "/.output/")))
      (dolist (file (directory-files source-directory t "\\.pdf$"))
        (let ((filename (file-name-nondirectory file))
              (destination-file (concat destination-directory (file-name-nondirectory file))))
          (copy-file file destination-file t)))))

  (defun my/org-latex-export ()
    (interactive)
    (org-latex-export-to-pdf t)
    (copy-new-pdf-files))

  (defun my/org-latex-export-and-open ()
    "Export the current Org buffer to PDF via LaTeX and open it."
    (interactive)
    (let ((pdf-file (org-latex-export-to-pdf)))
      (when pdf-file
        (message "Opening PDF: %s" pdf-file)
        (start-process "org-pdf-open" nil
                       (cond
                        ((eq system-type 'darwin) "open")
                        ((eq system-type 'gnu/linux) "xdg-open")
                        ((eq system-type 'windows-nt) "start"))
                       pdf-file))))

  (use-package ox-latex
    :ensure nil
    :after ox
    :config

    ;; setup directory to latex `preview'
    (setq org-preview-latex-image-directory ".ltximg/")
    (setq org-latex-preview-ltxpng-directory ".ltximg/")

    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            ))

    (setq org-latex-tables-centered t
          org-latex-tables-booktabs t
          org-export-with-smart-quotes t
          org-latex-prefer-user-labels t
          )

    ;; remove default classes
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))

    (setq org-latex-default-class "report")

    (add-to-list 'org-latex-classes
                 '("report"
                   "\\documentclass[a4paper,11pt,titlepage]{report}
                   \\input{~/projects/programming/latex/templates/basic.tex}
                   \\input{~/projects/programming/latex/templates/report.tex}
                   [NO-DEFAULT-PACKAGES]
                   [PACKAGES]
                   [EXTRA]
                   \\linespread{1.1}
                   \\hypersetup{pdfborder=0 0 0}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("article"
                   "\\documentclass[11pt,a4paper]{article}
                    \\input{~/projects/programming/latex/templates/basic.tex}
                   \\input{~/projects/programming/latex/templates/article.tex}
                   [NO-DEFAULT-PACKAGES]
                   [PACKAGES]
                   [EXTRA]
                   \\linespread{1.1}
                   \\hypersetup{pdfborder=0 0 0}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("memoir"
                   "\\documentclass[a4paper,11pt,twoside,openany]{memoir}
                    \\input{~/projects/programming/latex/templates/basic.tex}
                    \\input{~/projects/programming/latex/templates/memoir.tex}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]
                    \\linespread{1.1}
                    \\hypersetup{pdfborder=0 0 0}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\begin{enumerate} \\item \\textit{%s}" "\\end{enumerate}")
                   ))

    (add-to-list 'org-latex-classes
                 '("big-memoir"
                   "\\documentclass[a4paper,11pt]{memoir}
                    \\input{~/projects/programming/latex/templates/basic.tex}
                    \\input{~/projects/programming/latex/templates/memoir.tex}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]
                    \\linespread{1.1}
                    \\hypersetup{pdfborder=0 0 0}"
                   ("\\part{%s}" . "\\part*{%s}")
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\begin{enumerate} \\item \\textit{%s}" "\\end{enumerate}")
                   ))

    (add-to-list 'org-latex-classes
                 '("marton"
                   "\\documentclass[a4paper,11pt,article,openany]{memoir}
                    \\input{~/projects/programming/latex/templates/basic.tex}
                    \\input{~/projects/programming/latex/templates/marton.tex}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]
                    \\linespread{1.1}
                    \\hypersetup{pdfborder=0 0 0}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\begin{enumerate} \\item \\textit{%s}" "\\end{enumerate}")
                   ))
    )
  )

(defconst my/titlepage-command (concat
                                "\\begin{figure}\n"
                                "\\centering\n"
                                "\\includegraphics[height=4cm]{~/projects/programming/latex/templates/logo.png}"
                                "\\end{figure}\n"
                                "\\begin{center}\n"
                                "\\vfill\n"
                                "{\\Huge\\bfseries %t \\par}\n"
                                "{\\Large %a \\par}\n"
                                "{\\large \\today \\par}\n"
                                "\\vfill\n"
                                "\\thispagestyle{empty}\n"
                                "\\clearpage"
                                "\\end{center}\n"
                                ;; "\\restorepagestyle"
                                ))

(defconst my/maketitle-command (concat
                                "\\begin{figure}\n"
                                "\\centering\n"
                                "\\includegraphics[height=4cm]{~/projects/programming/latex/templates/logo.png}"
                                "\\end{figure}\n"
                                "\\begin{center}\n"
                                "{\\huge\\bfseries %t \\par}\n"
                                "{\\Large %a \\par}\n"
                                "{\\large stefano@marton.dev \\par}\n"
                                "{\\large \\today \\par}\n"
                                "\\thispagestyle{empty}\n"
                                "\\end{center}\n"
                                ;; "\\restorepagestyle"
                                ))

(defun my/org-latex-setup-commands-based-on-class (backend)
  "Set `org-latex-toc-command` and `org-latex-title-command` based on LaTeX class used."
  (when (eq backend 'latex)
    (let* ((info (org-export-get-environment 'latex))
           (class (plist-get info :latex-class)))
      (cond
       ((string= class "article")
        (setq-local org-latex-toc-command "\\tableofcontents"
                    org-latex-title-command "\\maketitle"))
       ((string= class "marton")
        (setq-local org-latex-toc-command "\\tableofcontents \\clearpage"
                    org-latex-title-command my/maketitle-command
                    org-latex-default-footnote-command "\\sidefootnote{%s}"
                    ))
       ((string= class "memoir")
        (setq-local org-latex-toc-command "\\clearpage \\tableofcontents \\clearpage"
                    org-latex-title-command my/titlepage-command
                    org-latex-default-footnote-command "\\sidefootnote{%s}"
                    ))
       (t
        (setq-local org-latex-toc-command "\\tableofcontents"
                    org-latex-title-command "\\maketitle"))))))

(add-hook 'org-export-before-processing-hook #'my/org-latex-setup-commands-based-on-class)


(provide 'org-export)

;;; org-export ends here
