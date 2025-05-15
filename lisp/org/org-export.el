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
                 \\usepackage[inkscapelatex=false]{svg}
                 \\hbadness 99999
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\usepackage[marginal]{footmisc} % cleaner footnotes
                 \\usepackage[utf8]{inputenc}
                 \\usepackage[margin=3cm]{geometry}
                 % \\usepackage[T1]{fontenc}
                 \\usepackage{fixltx2e}
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage{cancel}
                 \\setlength{\\parskip}{1pt}
                 \\usepackage{parskip}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\usepackage{mhchem}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{cleveref}
                 \\renewcommand\\labelenumi{(\\roman{enumi})}
                 \\renewcommand\\theenumi\\labelenumi
                 \\usepackage{mathtools}
                 \\DeclarePairedDelimiter\\bra{\\langle}{\\rvert}
                 \\DeclarePairedDelimiter\\ket{\\lvert}{\\rangle}
                 \\DeclarePairedDelimiterX\\braket[2]{\\langle}{\\rangle}{#1\\,\\delimsize\\vert\\,\\mathopen{}#2}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{xfrac}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }
                 %% \\usepackage{mathpazo}

                 \\usepackage{newpxtext}
                 \\usepackage{newpxmath}
                 \\usepackage{color}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 % Define cool colorboxes
                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{enumitem}
                 \\setlist{noitemsep}
                 \\tolerance=1000
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
                 \\setlength{\\parindent}{0pt}
                 \\setlength{\\parskip}{5pt}
                 \\usepackage{mhchem}
                 \\usepackage[inkscapelatex=false]{svg}
                 \\usepackage[utf8]{inputenc}
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\usepackage[T1]{fontenc}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }

                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{fixltx2e}
                 \\renewcommand\\labelenumi{(\\roman{enumi})}
                 \\renewcommand\\theenumi\\labelenumi
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{xfrac}
                 \\usepackage[margin=2.5cm]{geometry}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\usepackage{mathpazo}
                 \\usepackage{color}
                 \\usepackage{enumerate}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 \\tolerance=1000
                 \\renewcommand{\\tableofcontents}{}
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
                   "\\documentclass[a4paper,11pt, openany]{memoir}
                 %%% set up the recto page layout
                 \\usepackage{gensymb}
                 \\checkandfixthelayout
                 \\setlength{\\evensidemargin}{\\oddsidemargin}% after \\checkandfix
                 \\sidefootmargin{right}
                 \\usepackage[inkscapelatex=false]{svg}
                 \\hbadness 99999
                 \\usepackage{xfrac}
                 \\usepackage[italian]{babel}
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\setcounter{tocdepth}{2}
                 \\renewcommand\\labelenumi{(\\roman{enumi})}
                 \\renewcommand\\theenumi\\labelenumi
                 % \\usepackage[marginal]{footmisc} % cleaner footnotes
                 \\setlength{\\sidefoothsep}{0.5cm}
                 \\usepackage[utf8]{inputenc}
                 % \\usepackage[T1]{fontenc}
                 \\usepackage{fixltx2e}
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage{cancel}
                 \\setlength{\\parskip}{5pt}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\usepackage{mhchem}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{cleveref}

                 \\usepackage{mathtools}
                 \\DeclarePairedDelimiter\\bra{\\langle}{\\rvert}
                 \\DeclarePairedDelimiter\\ket{\\lvert}{\\rangle}
                 \\DeclarePairedDelimiterX\\braket[2]{\\langle}{\\rangle}{#1\\,\\delimsize\\vert\\,\\mathopen{}#2}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }
                 \\setsecnumdepth{subsection}
                 \\setlength{\\parindent}{0em}
                 \\usepackage{newpxtext}
                 \\usepackage{newpxmath}
                 \\usepackage{color}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 % Define cool colorboxes
                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{enumitem}
                 \\setlist{noitemsep}
                 \\tolerance=1000
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
                   "\\documentclass[a4paper,11pt, openany]{memoir}
                 %%% set up the recto page layout
                 \\usepackage{gensymb}
                 \\checkandfixthelayout
                 \\setlength{\\evensidemargin}{\\oddsidemargin}% after \\checkandfix
                 \\sidefootmargin{right}
                 \\usepackage[inkscapelatex=false]{svg}
                 \\hbadness 99999
                 \\usepackage{xfrac}
                 \\usepackage[italian]{babel}
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\setcounter{tocdepth}{2}
                 \\renewcommand\\labelenumi{(\\roman{enumi})}
                 \\renewcommand\\theenumi\\labelenumi
                 % \\usepackage[marginal]{footmisc} % cleaner footnotes
                 \\setlength{\\sidefoothsep}{0.5cm}
                 \\usepackage[utf8]{inputenc}
                 % \\usepackage[T1]{fontenc}
                 \\usepackage{fixltx2e}
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage{cancel}
                 \\setlength{\\parskip}{5pt}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\usepackage{mhchem}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{cleveref}

                 \\usepackage{mathtools}
                 \\DeclarePairedDelimiter\\bra{\\langle}{\\rvert}
                 \\DeclarePairedDelimiter\\ket{\\lvert}{\\rangle}
                 \\DeclarePairedDelimiterX\\braket[2]{\\langle}{\\rangle}{#1\\,\\delimsize\\vert\\,\\mathopen{}#2}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }
                 \\setsecnumdepth{subsection}
                 \\setlength{\\parindent}{0em}
                 \\usepackage{newpxtext}
                 \\usepackage{newpxmath}
                 \\usepackage{color}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 % Define cool colorboxes
                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{enumitem}
                 \\setlist{noitemsep}
                 \\tolerance=1000
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
                 '("guide"
                   "\\documentclass[a4paper,11pt, openany]{memoir}
                 %%% set up the recto page layout
                 \\usepackage{gensymb}
                 \\checkandfixthelayout
                 \\setlength{\\evensidemargin}{\\oddsidemargin}% after \\checkandfix
                 \\sidefootmargin{right}
                 \\usepackage[inkscapelatex=false]{svg}
                 \\hbadness 99999
                 \\usepackage{xfrac}
                 \\usepackage[italian]{babel}
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\setcounter{tocdepth}{2}
                 \\renewcommand\\labelenumi{(\\roman{enumi})}
                 \\renewcommand\\theenumi\\labelenumi
                 % \\usepackage[marginal]{footmisc} % cleaner footnotes
                 \\setlength{\\sidefoothsep}{0.5cm}
                 \\usepackage[utf8]{inputenc}
                 % \\usepackage[T1]{fontenc}
                 \\usepackage{fixltx2e}
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage{cancel}
                 \\setlength{\\parskip}{5pt}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\usepackage{mhchem}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{cleveref}

                 \\usepackage{mathtools}
                 \\DeclarePairedDelimiter\\bra{\\langle}{\\rvert}
                 \\DeclarePairedDelimiter\\ket{\\lvert}{\\rangle}
                 \\DeclarePairedDelimiterX\\braket[2]{\\langle}{\\rangle}{#1\\,\\delimsize\\vert\\,\\mathopen{}#2}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }
                 \\setsecnumdepth{subsection}
                 \\setlength{\\parindent}{0em}
                 \\usepackage{newpxtext}
                 \\usepackage{newpxmath}
                 \\usepackage{color}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 % Define cool colorboxes
                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{enumitem}
                 \\setlist{noitemsep}
                 \\tolerance=1000
                 \\addtopsmarks{headings}{}{
  \\createmark{chapter}{left}{nonumber}{}{}
}
\\pagestyle{headings}
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

(provide 'org-export)

;;; org-export ends here
