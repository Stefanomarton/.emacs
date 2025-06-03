;;; beamer.el --- org-mode beamer configuration -*- lexical-binding: t; -*-

(use-package ox-beamer
  :defer t
  :ensure nil
  :config
  (setq org-beamer-frame-level 3)

  (defun org-beamer--format-frame (headline contents info)
    "Format HEADLINE as a frame.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
    (let* ((fragilep
            (org-element-map headline org-beamer-verbatim-elements 'identity
                             info 'first-match))
           (frame (let ((selection
                         (or (and fragilep
                                  (or (string-match-p "\\\\begin{frame}" contents)
                                      (string-match-p "\\\\end{frame}" contents))
                                  org-beamer-frame-environment)
                             "frame")))
                    (unless (string= selection "frame")
                      (setq info (plist-put info :beamer-define-frame t)))
                    selection))
           ;; Frame options
           (beamer-opt (org-element-property :BEAMER_OPT headline))
           (options
            (cl-remove-if-not #'org-string-nw-p
                              (append
                               (org-split-string
                                (plist-get info :beamer-frame-default-options) ",")
                               (and beamer-opt
                                    (org-split-string
                                     (and (string-match "^\\[?\\(.*?\\)\\]?$" beamer-opt)
                                          (match-string 1 beamer-opt))
                                     ",")))))
           (fragile
            (and fragilep (not (member "fragile" options)) (list "fragile")))
           (label
            (and (not (member "allowframebreaks" options))
                 (not (cl-some (lambda (s) (string-match-p "^label=" s)) options))
                 (list
                  (let ((label (org-beamer--get-label headline info)))
                    (format (if (string-match-p ":" label)
                                "label={%s}" "label=%s")
                            label)))))
           (opt-string
            (org-beamer--normalize-argument
             (mapconcat #'identity (append label fragile options) ",")
             'option))
           (title (org-export-data (org-element-property :title headline) info))
           (env (org-element-property :BEAMER_ENV headline))
           (frame-title (if (and env (equal (downcase env) "fullframe")) "" title))
           (subtitle (org-element-property :BEAMER_SUBTITLE headline))
           ;; Fragile frame content fix
           (body (if (not fragilep)
                     contents
                   (replace-regexp-in-string "\\`\n*" "\\& " (or contents "")))))

      (concat
       ;; ✅ Add subsection here
       (format "\\subsection{%s}\n" title)

       ;; ✅ Correct begin{frame} line
       (format "\\begin{%s}%s{%s}"
               frame
               (if (org-string-nw-p opt-string)
                   (format "%s" opt-string)
                 "")
               frame-title)

       ;; Optional subtitle
       (when subtitle
         (format "{%s}"
                 (org-export-data
                  (org-element-parse-secondary-string
                   subtitle
                   (org-element-restriction 'keyword))
                  info)))

       "\n"
       body
       (format "\\end{%s}" frame))))


  (add-to-list 'org-latex-classes
               ;; beamer class, for presentations
               '("its-lesson"
                 "\\input{~/projects/programming/latex/templates/beamer/its-lessons/lucid.tex}"

                 ("\\makepart{%s}" . "\\makepart{%s}")
                 ("\\section{%s}" . "\\section{%s}")
                 ;; ("\\begin{frame}[fragile]\\frametitle{%s}"
                 ;;  "\\end{frame}")
                 ))
  )

(provide 'org-beamer)

;;; beamer.el ends here
