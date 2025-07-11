;;; surround.el -*- lexical-binding: t; -*-
;; Embrace
(use-package embrace
  :ensure t
  :demand t
  :bind
  (:map global-map
        ("C-," . embrace-commander)
        )
  
  :config
  (defun embrace-with-latex-environment ()
    (let ((block-type (completing-read
                       "Enviroment: "
                       '(center equation))))
      (setq block-type (downcase block-type))
      (cons (format "\\begin{%s}" block-type)
            (format "\\end{%s}" block-type))))

  (add-hook 'org-mode-hook
            (lambda ()
              (embrace-add-pair ?M "\\[" "\\]" t t)
              (embrace-add-pair ?m "\\\(" "\\\)" t nil)
              (embrace-add-pair ?P "\\left\(" "\\right\)" t nil)
              (embrace-add-pair ?p "\(" "\)" t nil)
              (embrace-add-pair ?S "\\left[" "\\right]" t nil)
              (embrace-add-pair ?s "[" "]" t nil)
              (embrace-add-pair ?t "\\text{" "}" t nil)

              (embrace-add-pair-regexp ?e "^[ \t]*\\\\begin{.+}.*$" "^[ \t]*\\\\end{.+}.*$" 'embrace-with-latex-environment
                                       (embrace-build-help "\\begin{}" "\\end{}") t)
              )
            )
  )

(defun mark-backward-paragraph ()
  (interactive)
  (exchange-point-and-mark)
  (mark-paragraph)
  (backward-paragraph)
  )

(use-package selected
  :ensure t
  :bind (:map selected-keymap
              ("U" . upcase-region)
              ("D" . downcase-region)
              ("C" . capitalize-region)

              ("u" . undo-in-region)

              ("p" . surround-region-with-parethesis)
              ("s" . surround-region-with-square-brackets)
              ("c" . surround-region-with-curly-brackets)

              ("{" . surround-region-newline-with-curly-brackets)

              ("h" . mark-paragraph)
              ("H" . mark-backward-paragraph)

              (";" . comment-dwim)

              ("a" . embrace-add)
              ("d" . embrace-delete)
              ("e" . move-end-of-line)

              ("y" . kill-ring-save)

              ("q" . er/mark-inside-quotes)
              ("\(" . er/mark-outside-pairs)
              
              )

  (:map selected-text-mode-map
        ("U" . upcase-region)
        ("D" . downcase-region)
        ("C" . capitalize-region)

        ("~" . surround-region-with-tilde)

        ("u" . undo-in-region)

        ("p" . surround-region-with-parethesis)
        ("s" . surround-region-with-square-brackets)
        ("c" . surround-region-with-curly-brackets)

        ("i" . surround-region-with-italic)

        (",c" . surround-region-with-command)
        (",C" . surround-region-with-cancel)
        (",t" . surround-region-with-text)
        (",r" . surround-region-with-mathrm)


        ;; ("{" . surround-region-newline-with-curly-brackets)

        ;; Environments
        ("ec" . (lambda () (interactive) (LaTeX-insert-environment "center")))
        ("eb" . (lambda () (interactive) (LaTeX-insert-environment "bx")))
        ("ee" . (lambda () (interactive) (LaTeX-insert-environment "equation*")))
        ("eE" . (lambda () (interactive) (LaTeX-insert-environment "equation")))
        ("eg" . (lambda () (interactive) (LaTeX-insert-environment "gather*")))
        ("eG" . (lambda () (interactive) (LaTeX-insert-environment "gather")))

        ("m" . surround-region-with-math)
        ("M" . surround-region-newline-with-math)
        ("w" . surround-region-with-chem)
        ("W" . surround-region-with-math-and-chem)

        ("_" . surround-region-with-subscript)
        (",," . surround-region-with-upperscript)

        (";" . comment-dwim)

        ("<tab>" . cdlatex-tab)

        ("a" . embrace-add)
        ("c" . embrace-change)
        ("d" . embrace-change)

        ("y" . kill-ring-save)
        ("J" . fix-pasted-text)

        ("q" . er/mark-inside-quotes)
        ("\(" . er/mark-outside-pairs))

  (:map selected-typst-ts-mode-map
        ("c" . surround-region-with-curly-brackets)
        ("p" . surround-region-with-parethesis)
        ("s" . surround-region-with-square-brackets)
        ("C" . surround-region-newline-with-curly-brackets)
        ("i" . surround-region-with-underscore)
        ("b" . surround-region-with-asterisk)
        ("f" . surround-region-with-function)
        ("u" . surround-region-with-upperscript)
        ("d" . surround-region-with-subscript)
        ("A" . surround-region-with-typst-align)
        )

  :config

  (setq selected-typst-ts-mode-map (make-sparse-keymap))
  
  (setq selected-text-mode-map (make-sparse-keymap))
  
  (defun fix-pasted-text ()
    (interactive)
    (join-line)
    (fill-paragraph)
    )

  (defun surround-region--surround (opening-delimiter closing-delimiter)
    "Surround the active region with hard-coded strings"
    (when (region-active-p)
      (save-excursion
        (let ((beginning (region-beginning))
              (end (region-end)))

          (goto-char beginning)
          (insert opening-delimiter)

          (goto-char (+ end (length closing-delimiter)))
	      (insert closing-delimiter)))))

  (defun surround-region-with-typst-align (align)
    "Surround the region with Typst align block: align(<style>)[ ... ]"
    (interactive
     (list (completing-read "Align style: " '("center" "left" "right" "justify") nil t nil)))
    (yas-expand-snippet (format "#align(%s)[`(yas-selected-text)`]" align)))


  (defun surround-region-with-parethesis ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "\(" "\)"))

  (defun surround-region-with-square-brackets ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "[" "]"))

  (defun surround-region-with-curly-brackets ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "{" "}"))

  (defun surround-region-with-underscore ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "_" "_"))

  (defun surround-region-with-asterisk ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "*" "*"))

  (defun surround-region-newline-with-curly-brackets ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "\n{\n`(yas-selected-text)`\n}\n"))

  (defun surround-region-with-math ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "\\\(" "\\\)"))

  (defun surround-region-with-tilde ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "~" "~"))

  (defun surround-region-with-chem ()
    "Surround the active region with hard-coded strings"
    (interactive)
	(yas-expand-snippet "\\ce{`(yas-selected-text)`}")
    (previous-line)
    (end-of-line))

  (defun surround-region-with-command (command)
    "Surround the active region with hard-coded strings"
    (interactive "sCommand: ")
	(yas-expand-snippet (concat "\\" command "{" "`(yas-selected-text)`}")))

  (defun surround-region-with-function (fn)
    "Surround the active region with hard-coded strings"
    (interactive "sFunction: ")
	(yas-expand-snippet (concat "#" fn "()[" "`(yas-selected-text)`]")))

  (defun surround-region-with-cancel ()
    "Surround the active region with hard-coded strings"
    (interactive)
	(yas-expand-snippet "\\cancel{`(yas-selected-text)`}"))

  (defun surround-region-with-text ()
    "Surround the active region with hard-coded strings"
    (interactive)
	(yas-expand-snippet "\\text{`(yas-selected-text)`}"))

  (defun surround-region-with-math-and-chem ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "\\\\(\\ce{`(yas-selected-text)`}\\\\)"))

  (defun surround-region-with-mathrm ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "\\mathrm{`(yas-selected-text)`}"))

  (defun surround-region-with-subscript ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "_{`(yas-selected-text)`}"))

  (defun surround-region-with-upperscript ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "^{`(yas-selected-text)`}"))

  (defun surround-region-with-italic ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "/`(yas-selected-text)`/"))

  (defun surround-region-newline-with-math ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (yas-expand-snippet "\\[\n`(yas-selected-text)`\n\\]")
    (previous-line)
    (end-of-line))
  :init
  (selected-global-mode))

(provide 'surround)
