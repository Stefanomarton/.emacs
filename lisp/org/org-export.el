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
    (defun org-latex--inline-image (link info)
      "Return LaTeX code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
      (let* ((parent (org-element-parent-element link))
             (path (let ((raw-path (org-element-property :path link)))
                     (if (not (file-name-absolute-p raw-path)) raw-path
                       (expand-file-name raw-path))))
             (filetype (file-name-extension path))
             (caption (org-latex--caption/label-string parent info))
             (caption-above-p (org-latex--caption-above-p link info))
             (attr (org-export-read-attribute :attr_latex parent))
             (sidecaptions (plist-get attr :sidecaptions)) ;; now string or nil
             (float (let ((float (plist-get attr :float)))
                      (cond
                       ((org-element-map (org-element-contents parent) t
                          (lambda (node)
                            (cond
                             ((and (org-element-type-p node 'plain-text)
                                   (not (org-string-nw-p node)))
                              nil)
                             ((eq link node)
                              (throw :org-element-skip nil))
                             (t 'not-a-float)))
                          info 'first-match)
                        nil)
                       ((string= float "wrap") 'wrap)
                       ((string= float "sideways") 'sideways)
                       ((string= float "multicolumn") 'multicolumn)
                       ((string= float "t") 'figure)
                       ((and (plist-member attr :float) (not float)) 'nonfloat)
                       (float float)
                       ((or (org-element-property :caption parent)
                            (org-string-nw-p (plist-get attr :caption)))
                        'figure)
                       (t 'nonfloat))))
             (placement
              (let ((place (plist-get attr :placement)))
                (cond
                 (place (format "%s" place))
                 ((eq float 'wrap) "{l}{0.5\\textwidth}")
                 ((eq float 'figure)
                  (format "[%s]" (plist-get info :latex-default-figure-position)))
                 (t ""))))
             (center
              (cond
               ((org-element-type-p (org-element-parent link) 'link) nil)
               ((plist-member attr :center) (plist-get attr :center))
               (t (plist-get info :latex-images-centered))))
             (comment-include (if (plist-get attr :comment-include) "%" ""))
             (scale (cond ((eq float 'wrap) "")
                          ((plist-get attr :scale))
                          (t (plist-get info :latex-image-default-scale))))
             (width (cond ((org-string-nw-p scale) "")
                          ((plist-get attr :width))
                          ((plist-get attr :height) "")
                          ((eq float 'wrap) "0.48\\textwidth")
                          (t (plist-get info :latex-image-default-width))))
             (height (cond ((org-string-nw-p scale) "")
                           ((plist-get attr :height))
                           ((or (plist-get attr :width)
                                (memq float '(figure wrap))) "")
                           (t (plist-get info :latex-image-default-height))))
             (options (let ((opt (or (plist-get attr :options)
                                     (plist-get info :latex-image-default-option))))
                        (if (not (string-match "\\`\\[\\(.*\\)\\]\\'" opt)) opt
                          (match-string 1 opt))))
             image-code)

        ;; tikz and pgf
        (if (member filetype '("tikz" "pgf"))
            (progn
              (setq image-code (format "\\input{%s}" path))
              (when (org-string-nw-p options)
                (setq image-code
                      (format "\\begin{tikzpicture}[%s]\n%s\n\\end{tikzpicture}"
                              options image-code)))
              (setq image-code
                    (cond ((org-string-nw-p scale)
                           (format "\\scalebox{%s}{%s}" scale image-code))
                          ((or (org-string-nw-p width) (org-string-nw-p height))
                           (format "\\resizebox{%s}{%s}{%s}"
                                   (if (org-string-nw-p width) width "!")
                                   (if (org-string-nw-p height) height "!")
                                   image-code))
                          (t image-code))))
          ;; normal image
          (if (org-string-nw-p scale)
              (setq options (concat options ",scale=" scale))
            (when (org-string-nw-p width) (setq options (concat options ",width=" width)))
            (when (org-string-nw-p height) (setq options (concat options ",height=" height))))
          (let ((search-option (org-element-property :search-option link)))
            (when (and search-option
                       (equal filetype "pdf")
                       (string-match-p "\\`[0-9]+\\'" search-option)
                       (not (string-match-p "page=" options)))
              (setq options (concat options ",page=" search-option))))
          (setq image-code
                (format "\\includegraphics%s{%s}"
                        (cond ((not (org-string-nw-p options)) "")
                              ((string-prefix-p "," options)
                               (format "[%s]" (substring options 1)))
                              (t (format "[%s]" options)))
                        (if (and (string-match-p "[^[:ascii:]]" path)
                                 (equal filetype "svg"))
                            (concat "\\detokenize{" path "}")
                          path)))
          (when (equal filetype "svg")
            (setq image-code (replace-regexp-in-string "^\\\\includegraphics"
                                                       "\\includesvg"
                                                       image-code
                                                       nil t))
            (setq image-code (replace-regexp-in-string "\\.svg}"
                                                       "}"
                                                       image-code
                                                       nil t))))

        ;; If :sidecaptions attribute is a string, output the custom sidecaption block
        (when (and sidecaptions (stringp sidecaptions) (not (string-empty-p sidecaptions)))
          (setq image-code
                (format "\\begin{figure}
\\begin{sidecaption}{%s}
\\centering
%s
\\end{sidecaption}
\\end{figure}"
                        sidecaptions image-code)))

        ;; Return early if sidecaptions used
        (if (and sidecaptions (stringp sidecaptions) (not (string-empty-p sidecaptions)))
            image-code
          ;; Otherwise default float handling
          (pcase float
            ((and (pred stringp) env-string)
             (format "\\begin{%s}%s
%s%s
%s%s
%s\\end{%s}"
                     env-string
                     placement
                     (if caption-above-p caption "")
                     (if center "\\centering" "")
                     comment-include image-code
                     (if caption-above-p "" caption)
                     env-string))
            (`wrap (format "\\begin{wrapfigure}%s
%s%s
%s%s
%s\\end{wrapfigure}"
                           placement
                           (if caption-above-p caption "")
                           (if center "\\centering" "")
                           comment-include image-code
                           (if caption-above-p "" caption)))
            (`sideways (format "\\begin{sidewaysfigure}
%s%s
%s%s
%s\\end{sidewaysfigure}"
                               (if caption-above-p caption "")
                               (if center "\\centering" "")
                               comment-include image-code
                               (if caption-above-p "" caption)))
            (`multicolumn (format "\\begin{figure*}%s
%s%s
%s%s
%s\\end{figure*}"
                                  placement
                                  (if caption-above-p caption "")
                                  (if center "\\centering" "")
                                  comment-include image-code
                                  (if caption-above-p "" caption)))
            (`figure (format "\\begin{figure}%s
%s%s
%s%s
%s\\end{figure}"
                             placement
                             (if caption-above-p caption "")
                             (if center "\\centering" "")
                             comment-include image-code
                             (if caption-above-p "" caption)))
            ((guard center)
             (format "\\begin{center}
%s%s
%s\\end{center}"
                     (if caption-above-p caption "")
                     image-code
                     (if caption-above-p "" caption)))
            (_
             (concat (if caption-above-p caption "")
                     image-code
                     (if caption-above-p caption "")))))))
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
                    ;; org-latex-default-footnote-command "\\sidefootnote{%s}"
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
