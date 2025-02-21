;;; orgconfig.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("C-," . embrace-commander)
              ("C-c o h" . consult-org-heading)
              ("<escape> >" . org-promote-subtree)
              ("<escape> <" . org-demote-subtree)
              ("<escape> J" . my-fix-text-region))
  :hook
  (org-mode . org-margin-mode)
  (org-mode . yas-minor-mode-on)
  (org-mode . er/add-latex-in-org-mode-expansions)
  (org-mode . my/org-header-outline-path-mode)
  (org-mode . auto-fill-mode)

  :custom
  (org-use-speed-commands t)
  (org-adapt-indentation nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width 500)
  (org-hide-leading-stars nil)
  (org-special-ctrl-a/e t)
  (org-catch-invisible-edits 'show)
  (org-id-link-to-org-use-id nil)

  :init
  (setq org-fold-core-style 'text-properties)

  :config
  ;; Dinamically define attachments folder
  (defvar org-attachments-folder nil
    "Variable to store the attachments folder for Org mode files.")

  (defun extract-timestamp (filename)
    "Extract the timestamp from a FILENAME in the format '20240622T150711--...'."
    (string-match "\\`\\([0-9T]+\\)--" filename)
    (match-string 1 filename))

  (defun org-set-attachments-folder ()
    "Set the attachments folder for the current Org mode buffer."
    (let ((buf-dir (file-name-directory (buffer-file-name)))
          (org-dir (expand-file-name notes-folder)))
      (setq org-attachments-folder
            (if (string-prefix-p org-dir buf-dir)
                (concat org-dir "/.attachments/" (extract-timestamp (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
              ".attachments"))))

  (setq org-cite-global-bibliography '("~/.marton-drive/notes/.resources/bibliography.bib"))
  (setq org-cite-processor '((t csl ~/marton-drive/notes/.resources/ieee.csl ~/GoogleDrive/org/.resources/ieee.csl)))

  ;; Make surround with latex env work nicely
  (require 'tex-site)
  (use-package org-ref
    :after org-mode)

  (defun my-fix-text-region (pos1 pos2)
    "Replace strings within a region."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region pos1 pos2)
        (save-excursion
          (goto-char pos1)
          (while (and (< (point) pos2) (not (eobp)))
            (join-line 1)))
        (fill-paragraph t t)
        (replace-regexp "- \([A-z]+\)" "\1")
        (dolist (ele (list "`a" "`e" "`o" "`u" "`i" "’"))
          (setq elt ele)
          (goto-char (point-min))
          (while (search-forward elt nil t 1)
            (replace-match
             (char-to-string
              (pcase ele
                ("` a" ?à)
                ("’" ?')
                ("` i" ?ì)
                ("` e" ?è)
                ("` o" ?ò)
                ("` u" ?ù)
                ("`a" ?à)
                ("`i" ?ì)
                ("`e" ?è)
                ("`o" ?ò)
                ("`u" ?ù)
                ))))))))

  (defun sbr-org-insert-dwim (&optional arg)
    "Insert another entry of the same type as the current
entry. For example, if the point is on a list item, then add
another list item of the same type, and if the point is on a
checkbox
 list item, then add an empty checkbox item. If instead
the point is in a heading, then add another heading. If the point
is in a TODO heading, then add another TODO heading (set to the
TODO state).

By default, the new entry is inserted below the current
subtree/item. With a 'C-u' prefix, insert the entry above the
current heading/item instead."
    (interactive "P")
    (when (eq major-mode 'org-mode)
      (let ((org-special-ctrl-a/e t)
            (below? (unless  (equal arg '(4)) '(4))))
        ;; hack to ensure that the point is not after ellipses because
        ;; that would mess up org-at-item-p etc.
        (org-beginning-of-line)
        (cond ((org-at-item-p) ;; at list item or checkbox
               (let ((org-M-RET-may-split-line nil)
                     (org-enable-sort-checkbox nil))
                 ;; hack to make item be inserted after the current one
                 ;; doesn't work if we are on an empty item line
                 (when below?
                   (org-end-of-line))
                 (org-insert-item (org-at-item-checkbox-p))))
              ((org-before-first-heading-p) ;; above first heading
               (org-insert-heading))
              (t ;; in some kind of heading
               (org-back-to-heading)
               (if (org-get-todo-state)
                   ;; at TODO heading
                   (org-insert-todo-heading t below?)
                 ;; at non-TODO heading
                 (org-insert-heading below?)))))))

  (defun sbr-org-shift-return (&optional arg)
    "If point is at a table, copy the table cell downward (i.e.,
the usual effect of typing S-RET). Otherwise,  insert the same
kind of heading or item as the current entry containing the
point. "
    (interactive "P")
    (if (org-at-table-p)
        (org-table-copy-down (prefix-numeric-value arg))
      (sbr-org-insert-dwim arg)))

  (setq org-directory notes-folder)

  (setq denote-directory notes-folder)

  (setq org-link-abbrev-alist
        `(("image-dir" . ,(format "file:%s%s" notes-folder "/.attachments/"))))

  (bind-keys :map org-mode-map ("<S-return>" . sbr-org-insert-dwim))

  (setq org-blank-before-new-entry
        '((heading . nil)
          (plain-list-item . auto)))
  (setq
   org-ellipsis " ↓"
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t)

  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)))

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (cl-set-difference er/try-expand-list
                                      '(er/mark-method-call
                                        er/mark-inside-pairs
                                        er/mark-outside-pairs))
                   '(LaTeX-mark-environment
                     er/mark-LaTeX-inside-math
                     er/mark-latex-inside-pairs
                     er/mark-latex-outside-pairs
                     er/mark-LaTeX-math)))))


  (add-hook 'org-mode-hook (lambda ()
                             (setq-local fill-column 110)
                             (setq-local set-fill-column 115)))


  ;; all possible latex highlight
  (setq org-highlight-latex-and-related '(native))

  (defun my/org-time-stamp ()
    (interactive)
    (org-timestamp '(16) nil)
    )

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; Modify org-pretty entity superscript
  (defun my/org-raise-scripts-no-braces (_)
    (when (and (eq (char-after (match-beginning 3)) ?{)
	           (eq (char-before (match-end 3)) ?}))
      (remove-text-properties (match-beginning 3) (1+ (match-beginning 3))
		                      (list 'invisible nil))
      (remove-text-properties (1- (match-end 3)) (match-end 3)
		                      (list 'invisible nil))))

  (advice-add 'org-raise-scripts :after #'my/org-raise-scripts-no-braces)

  (setq org-export-headline-levels 6)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)) ;; fix dimension of latex fragments

  (add-to-list 'org-file-apps '("\\.pdf" . "zathura %s")) ;; open pdf files with zathura

  ;; (advice-add 'org-latex-compile :after #'delete-file) ;; delete compilation files after .tex export

  ;; modify export folder for org export
  ;; taken from https://stackoverflow.com/questions/9559753/emacs-org-mode-export-to-another-directory

  (setq org-export-in-background t)
  (setq org-export-async-debug t)
  ;; (setq org-export-async-init-file "~/.config/emacs/async-init.el")

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
          (destination-directory "~/GoogleDrive/denote/.output/"))
      (dolist (file (directory-files source-directory t "\\.pdf$"))
        (let ((filename (file-name-nondirectory file))
              (destination-file (concat destination-directory (file-name-nondirectory file))))
          (copy-file file destination-file t)))))

  ;; (defun export-org-latex-and-copy-pdf ()
  ;;   "Export Org mode to LaTeX asynchronously and copy new PDF files."
  ;;   (interactive)
  ;;   (org-latex-export-to-pdf t)
  ;;   (run-at-time "5 sec" nil 'copy-new-pdf-files))

  ;; (add-hook 'org-mode-hook
  ;;           (lambda () (add-hook 'after-save-hook 'export-org-latex-and-copy-pdf nil 'local)))

  (setq org-latex-default-class "report")
  (setq org-startup-folded t)
  (setq org-pretty-entities nil)
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-use-sub-superscripts '{})

  (defun vz/org-prettify--predicate (_start end _match)
    ;; There's no need the check the character before the entity match
    ;; since all of them start with \. The characters that are
    ;; acceptable after the match are mathematical operators and some
    ;; special characters.
    (seq-contains-p '(?\C-j ?} ?{ ?\\ ?_ ?- ?+ ?^ ?\( ?\) ?$ ?  ?/ ?| ?. ?, ?\;)
                    (char-after end)))

  (defun my/org-mode/load-prettify-symbols ()
    (interactive)
    (setq-local prettify-symbols-alist
                (cl-copy-list my-org-prettify-symbols-alist))
    (setq-local prettify-symbols-compose-predicate #'vz/org-prettify--predicate)

    (prettify-symbols-mode))

  (add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

  (defvar my-org-prettify-symbols-alist
    '( ;; Lowercase Greek letters.
      ("\\\\" . ?↩)
      ("\\alpha" . ?α)
      ("\\beta" . ?β)
      ("\\gamma" . ?γ)
      ("\\delta" . ?δ)
      ("\\epsilon" . ?ϵ)
      ("\\zeta" . ?ζ)
      ("\\eta" . ?η)
      ("\\theta" . ?θ)
      ("\\iota" . ?ι)
      ("\\kappa" . ?κ)
      ("\\lambda" . ?λ)
      ("\\mu" . ?μ)
      ("\\nu" . ?ν)
      ("\\xi" . ?ξ)
      ;; There is no \omicron because it looks like a latin o.
      ("\\pi" . ?π)
      ("\\rho" . ?ρ)
      ("\\sigma" . ?σ)
      ("\\tau" . ?τ)
      ("\\upsilon" . ?υ)
      ("\\phi" . ?ϕ)
      ("\\chi" . ?χ)
      ("\\psi" . ?ψ)
      ("\\omega" . ?ω)
      ;; Uppercase Greek letters.
      ("\\Gamma" . ?Γ)
      ("\\Delta" . ?Δ)
      ("\\Lambda" . ?Λ)
      ("\\Phi" . ?Φ)
      ("\\Pi" . ?Π)
      ("\\Psi" . ?Ψ)
      ("\\Sigma" . ?Σ)
      ("\\Theta" . ?Θ)
      ("\\Upsilon" . ?Υ)
      ("\\Xi" . ?Ξ)
      ("\\Omega" . ?Ω)

      ;; Other math symbols (taken from leim/quail/latin-ltx.el).
      ("\\Box" . ?□)
      ("\\Bumpeq" . ?≎)
      ("\\Cap" . ?⋒)
      ("\\Cup" . ?⋓)
      ("\\Diamond" . ?◇)
      ("\\Downarrow" . ?⇓)
      ("\\H{o}" . ?ő)
      ("\\Im" . ?ℑ)
      ("\\Join" . ?⋈)
      ("\\Leftarrow" . ?⇐)
      ("\\Leftrightarrow" . ?⇔)
      ("\\Ll" . ?⋘)
      ("\\Lleftarrow" . ?⇚)
      ("\\Longleftarrow" . ?⇐)
      ("\\Longleftrightarrow" . ?⇔)
      ("\\Longrightarrow" . ?⇒)
      ("\\Lsh" . ?↰)
      ("\\Re" . ?ℜ)
      ("\\Rightarrow" . ?⇒)
      ("\\Rrightarrow" . ?⇛)
      ("\\Rsh" . ?↱)
      ("\\Subset" . ?⋐)
      ("\\Supset" . ?⋑)
      ("\\Uparrow" . ?⇑)
      ("\\Updownarrow" . ?⇕)
      ("\\Vdash" . ?⊩)
      ("\\Vert" . ?‖)
      ("\\Vvdash" . ?⊪)
      ("\\aleph" . ?ℵ)
      ("\\amalg" . ?∐)
      ("\\angle" . ?∠)
      ("\\approx" . ?≈)
      ("\\approxeq" . ?≊)
      ("\\ast" . ?∗)
      ("\\asymp" . ?≍)
      ("\\backcong" . ?≌)
      ("\\backepsilon" . ?∍)
      ("\\backprime" . ?‵)
      ("\\backsim" . ?∽)
      ("\\backsimeq" . ?⋍)
      ("\\backslash" . ?\\)
      ("\\barwedge" . ?⊼)
      ("\\because" . ?∵)
      ("\\beth" . ?ℶ)
      ("\\between" . ?≬)
      ("\\bigcap" . ?⋂)
      ("\\bigcirc" . ?◯)
      ("\\bigcup" . ?⋃)
      ("\\bigstar" . ?★)
      ("\\bigtriangledown" . ?▽)
      ("\\bigtriangleup" . ?△)
      ("\\bigvee" . ?⋁)
      ("\\bigwedge" . ?⋀)
      ("\\blacklozenge" . ?✦)
      ("\\blacksquare" . ?▪)
      ("\\blacktriangle" . ?▴)
      ("\\blacktriangledown" . ?▾)
      ("\\blacktriangleleft" . ?◂)
      ("\\blacktriangleright" . ?▸)
      ("\\bot" . ?⊥)
      ("\\bowtie" . ?⋈)
      ("\\boxminus" . ?⊟)
      ("\\boxplus" . ?⊞)
      ("\\boxtimes" . ?⊠)
      ("\\bullet" . ?•)
      ("\\bumpeq" . ?≏)
      ("\\cap" . ?∩)
      ("\\cdots" . ?⋯)
      ("\\centerdot" . ?·)
      ("\\checkmark" . ?✓)
      ("\\chi" . ?χ)
      ("\\cdot" . ?⋅)
      ("\\cdots" . ?⋯)
      ("\\circ" . ?∘)
      ("\\circeq" . ?≗)
      ("\\circlearrowleft" . ?↺)
      ("\\circlearrowright" . ?↻)
      ("\\circledR" . ?®)
      ("\\circledS" . ?Ⓢ)
      ("\\circledast" . ?⊛)
      ("\\circledcirc" . ?⊚)
      ("\\circleddash" . ?⊝)
      ("\\clubsuit" . ?♣)
      ("\\coloneq" . ?≔)
      ("\\complement" . ?∁)
      ("\\cong" . ?≅)
      ("\\coprod" . ?∐)
      ("\\cup" . ?∪)
      ("\\curlyeqprec" . ?⋞)
      ("\\curlyeqsucc" . ?⋟)
      ("\\curlypreceq" . ?≼)
      ("\\curlyvee" . ?⋎)
      ("\\curlywedge" . ?⋏)
      ("\\curvearrowleft" . ?↶)
      ("\\curvearrowright" . ?↷)
      ("\\dag" . ?†)
      ("\\dagger" . ?†)
      ("\\daleth" . ?ℸ)
      ("\\dashv" . ?⊣)
      ("\\ddag" . ?‡)
      ("\\ddagger" . ?‡)
      ("\\ddots" . ?⋱)
      ("\\diamond" . ?⋄)
      ("\\diamondsuit" . ?♢)
      ("\\divideontimes" . ?⋇)
      ("\\doteq" . ?≐)
      ("\\doteqdot" . ?≑)
      ("\\dotplus" . ?∔)
      ("\\dotsquare" . ?⊡)
      ("\\downarrow" . ?↓)
      ("\\downdownarrows" . ?⇊)
      ("\\downleftharpoon" . ?⇃)
      ("\\downrightharpoon" . ?⇂)
      ("\\ell" . ?ℓ)
      ("\\emptyset" . ?∅)
      ("\\eqcirc" . ?≖)
      ("\\eqcolon" . ?≕)
      ("\\eqslantgtr" . ?⋝)
      ("\\eqslantless" . ?⋜)
      ("\\equiv" . ?≡)
      ("\\exists" . ?∃)
      ("\\fallingdotseq" . ?≒)
      ("\\flat" . ?♭)
      ("\\forall" . ?∀)
      ("\\frown" . ?⌢)
      ("\\ge" . ?≥)
      ("\\geq" . ?≥)
      ("\\geqq" . ?≧)
      ("\\geqslant" . ?≥)
      ("\\gets" . ?←)
      ("\\gg" . ?≫)
      ("\\ggg" . ?⋙)
      ("\\gimel" . ?ℷ)
      ("\\gnapprox" . ?⋧)
      ("\\gneq" . ?≩)
      ("\\gneqq" . ?≩)
      ("\\gnsim" . ?⋧)
      ("\\gtrapprox" . ?≳)
      ("\\gtrdot" . ?⋗)
      ("\\gtreqless" . ?⋛)
      ("\\gtreqqless" . ?⋛)
      ("\\gtrless" . ?≷)
      ("\\gtrsim" . ?≳)
      ("\\gvertneqq" . ?≩)
      ("\\hbar" . ?ℏ)
      ("\\heartsuit" . ?♥)
      ("\\hookleftarrow" . ?↩)
      ("\\hookrightarrow" . ?↪)
      ("\\iff" . ?⇔)
      ("\\imath" . ?ı)
      ("\\in" . ?∈)
      ("\\infty" . ?∞)
      ("\\int" . ?∫)
      ("\\intercal" . ?⊺)
      ("\\langle" . 10216)          ; Literal ?⟨ breaks indentation.
      ("\\lbrace" . ?{)
      ("\\lbrack" . ?\[)
      ("\\lceil" . ?⌈)
      ("\\ldots" . ?…)
      ("\\le" . ?≤)
      ("\\leadsto" . ?↝)
      ("\\leftarrow" . ?←)
      ("\\leftarrowtail" . ?↢)
      ("\\leftharpoondown" . ?↽)
      ("\\leftharpoonup" . ?↼)
      ("\\leftleftarrows" . ?⇇)
      ;; ("\\leftparengtr" ?〈), see bug#12948.
      ("\\leftrightarrow" . ?↔)
      ("\\leftrightarrows" . ?⇆)
      ("\\leftrightharpoons" . ?⇋)
      ("\\leftrightsquigarrow" . ?↭)
      ("\\leftthreetimes" . ?⋋)
      ("\\leq" . ?≤)
      ("\\leqq" . ?≦)
      ("\\leqslant" . ?≤)
      ("\\lessapprox" . ?≲)
      ("\\lessdot" . ?⋖)
      ("\\lesseqgtr" . ?⋚)
      ("\\lesseqqgtr" . ?⋚)
      ("\\lessgtr" . ?≶)
      ("\\lesssim" . ?≲)
      ("\\lfloor" . ?⌊)
      ("\\lhd" . ?◁)
      ("\\rhd" . ?▷)
      ("\\ll" . ?≪)
      ("\\llcorner" . ?⌞)
      ("\\lnapprox" . ?⋦)
      ("\\lneq" . ?≨)
      ("\\lneqq" . ?≨)
      ("\\lnsim" . ?⋦)
      ("\\longleftarrow" . ?←)
      ("\\longleftrightarrow" . ?↔)
      ("\\longmapsto" . ?↦)
      ("\\longrightarrow" . ?→)
      ("\\looparrowleft" . ?↫)
      ("\\looparrowright" . ?↬)
      ("\\lozenge" . ?✧)
      ("\\lq" . ?‘)
      ("\\lrcorner" . ?⌟)
      ("\\ltimes" . ?⋉)
      ("\\lvertneqq" . ?≨)
      ("\\maltese" . ?✠)
      ("\\mapsto" . ?↦)
      ("\\measuredangle" . ?∡)
      ("\\mho" . ?℧)
      ("\\mid" . ?∣)
      ("\\models" . ?⊧)
      ("\\mp" . ?∓)
      ("\\multimap" . ?⊸)
      ("\\nLeftarrow" . ?⇍)
      ("\\nLeftrightarrow" . ?⇎)
      ("\\nRightarrow" . ?⇏)
      ("\\nVDash" . ?⊯)
      ("\\nVdash" . ?⊮)
      ("\\nabla" . ?∇)
      ("\\napprox" . ?≉)
      ("\\natural" . ?♮)
      ("\\ncong" . ?≇)
      ("\\ne" . ?≠)
      ("\\nearrow" . ?↗)
      ("\\neg" . ?¬)
      ("\\neq" . ?≠)
      ("\\nequiv" . ?≢)
      ("\\newline" . ? )
      ("\\nexists" . ?∄)
      ("\\ngeq" . ?≱)
      ("\\ngeqq" . ?≱)
      ("\\ngeqslant" . ?≱)
      ("\\ngtr" . ?≯)
      ("\\ni" . ?∋)
      ("\\nleftarrow" . ?↚)
      ("\\nleftrightarrow" . ?↮)
      ("\\nleq" . ?≰)
      ("\\nleqq" . ?≰)
      ("\\nleqslant" . ?≰)
      ("\\nless" . ?≮)
      ("\\nmid" . ?∤)
      ;; ("\\not" ?̸)              ;FIXME: conflict with "NOT SIGN" ¬.
      ("\\notin" . ?∉)
      ("\\nparallel" . ?∦)
      ("\\nprec" . ?⊀)
      ("\\npreceq" . ?⋠)
      ("\\nrightarrow" . ?↛)
      ("\\nshortmid" . ?∤)
      ("\\nshortparallel" . ?∦)
      ("\\nsim" . ?≁)
      ("\\nsimeq" . ?≄)
      ("\\nsubset" . ?⊄)
      ("\\nsubseteq" . ?⊈)
      ("\\nsubseteqq" . ?⊈)
      ("\\nsucc" . ?⊁)
      ("\\nsucceq" . ?⋡)
      ("\\nsupset" . ?⊅)
      ("\\nsupseteq" . ?⊉)
      ("\\nsupseteqq" . ?⊉)
      ("\\ntriangleleft" . ?⋪)
      ("\\ntrianglelefteq" . ?⋬)
      ("\\ntriangleright" . ?⋫)
      ("\\ntrianglerighteq" . ?⋭)
      ("\\nvDash" . ?⊭)
      ("\\nvdash" . ?⊬)
      ("\\nwarrow" . ?↖)
      ("\\odot" . ?⊙)
      ("\\oint" . ?∮)
      ("\\ominus" . ?⊖)
      ("\\oplus" . ?⊕)
      ("\\oslash" . ?⊘)
      ("\\otimes" . ?⊗)
      ("\\parallel" . ?∥)
      ("\\partial" . ?∂)
      ("\\perp" . ?⊥)
      ("\\pitchfork" . ?⋔)
      ("\\prec" . ?≺)
      ("\\precapprox" . ?≾)
      ("\\preceq" . ?≼)
      ("\\precnapprox" . ?⋨)
      ("\\precnsim" . ?⋨)
      ("\\precsim" . ?≾)
      ("\\prime" . ?′)
      ("\\prod" . ?∏)
      ("\\propto" . ?∝)
      ("\\qed" . ?∎)
      ("\\qquad" . ?⧢)
      ("\\quad" . ?␣)
      ("\\rangle" . 10217)            ; Literal ?⟩ breaks indentation.
      ("\\rbrace" . ?})
      ("\\rbrack" . ?\])
      ("\\rceil" . ?⌉)
      ("\\rfloor" . ?⌋)
      ("\\rightarrow" . ?→)
      ("\\rightarrowtail" . ?↣)
      ("\\rightharpoondown" . ?⇁)
      ("\\rightharpoonup" . ?⇀)
      ("\\rightleftarrows" . ?⇄)
      ("\\rightleftharpoons" . ?⇌)
      ;; ("\\rightparengtr" ?⦔) ;; Was ?〉, see bug#12948.
      ("\\rightrightarrows" . ?⇉)
      ("\\rightthreetimes" . ?⋌)
      ("\\risingdotseq" . ?≓)
      ("\\rtimes" . ?⋊)
      ("\\times" . ?×)
      ("\\sbs" . ?﹨)
      ("\\searrow" . ?↘)
      ("\\setminus" . ?∖)
      ("\\sharp" . ?♯)
      ("\\shortmid" . ?∣)
      ("\\shortparallel" . ?∥)
      ("\\sim" . ?∼)
      ("\\simeq" . ?≃)
      ("\\smallamalg" . ?∐)
      ("\\smallsetminus" . ?∖)
      ("\\smallsmile" . ?⌣)
      ("\\smile" . ?⌣)
      ("\\spadesuit" . ?♠)
      ("\\sphericalangle" . ?∢)
      ("\\sqcap" . ?⊓)
      ("\\sqcup" . ?⊔)
      ("\\sqsubset" . ?⊏)
      ("\\sqsubseteq" . ?⊑)
      ("\\sqsupset" . ?⊐)
      ("\\sqsupseteq" . ?⊒)
      ("\\square" . ?□)
      ("\\squigarrowright" . ?⇝)
      ("\\star" . ?⋆)
      ("\\straightphi" . ?φ)
      ("\\subset" . ?⊂)
      ("\\subseteq" . ?⊆)
      ("\\subseteqq" . ?⊆)
      ("\\subsetneq" . ?⊊)
      ("\\subsetneqq" . ?⊊)
      ("\\succ" . ?≻)
      ("\\succapprox" . ?≿)
      ("\\succcurlyeq" . ?≽)
      ("\\succeq" . ?≽)
      ("\\succnapprox" . ?⋩)
      ("\\succnsim" . ?⋩)
      ("\\succsim" . ?≿)
      ("\\sum" . ?∑)
      ("\\supset" . ?⊃)
      ("\\supseteq" . ?⊇)
      ("\\supseteqq" . ?⊇)
      ("\\supsetneq" . ?⊋)
      ("\\supsetneqq" . ?⊋)
      ("\\surd" . ?√)
      ("\\swarrow" . ?↙)
      ("\\therefore" . ?∴)
      ("\\thickapprox" . ?≈)
      ("\\thicksim" . ?∼)
      ("\\to" . ?→)
      ("\\top" . ?⊤)
      ("\\triangle" . ?▵)
      ("\\triangledown" . ?▿)
      ("\\triangleleft" . ?◃)
      ("\\trianglelefteq" . ?⊴)
      ("\\triangleq" . ?≜)
      ("\\triangleright" . ?▹)
      ("\\trianglerighteq" . ?⊵)
      ("\\twoheadleftarrow" . ?↞)
      ("\\twoheadrightarrow" . ?↠)
      ("\\ulcorner" . ?⌜)
      ("\\uparrow" . ?↑)
      ("\\updownarrow" . ?↕)
      ("\\upleftharpoon" . ?↿)
      ("\\uplus" . ?⊎)
      ("\\uprightharpoon" . ?↾)
      ("\\upuparrows" . ?⇈)
      ("\\urcorner" . ?⌝)
      ("\\u{i}" . ?ĭ)
      ("\\vDash" . ?⊨)
      ("\\varepsilon" . ?ε)
      ("\\varphi" . ?φ)
      ("\\varprime" . ?′)
      ("\\varpropto" . ?∝)
      ("\\varrho" . ?ϱ)
      ("\\varsigma" . ?ς)
      ("\\vartriangleleft" . ?⊲)
      ("\\vartriangleright" . ?⊳)
      ("\\vdash" . ?⊢)
      ("\\vdots" . ?⋮)
      ("\\vee" . ?∨)
      ("\\veebar" . ?⊻)
      ("\\vert" . ?|)
      ("\\wedge" . ?∧)
      ("\\wp" . ?℘)
      ("\\wr" . ?≀)
      ("\\Bbb{N}" . ?ℕ)			; AMS commands for blackboard bold
      ("\\Bbb{P}" . ?ℙ)			; Also sometimes \mathbb.
      ("\\Bbb{Q}" . ?ℚ)
      ("\\Bbb{R}" . ?ℝ)
      ("\\Bbb{T}" . ?𝕋)
      ("\\Bbb{Z}" . ?ℤ)
      ("\\mathbb{N}" . ?ℕ)			; AMS commands for blackboard bold
      ("\\mathbb{P}" . ?ℙ)			; Also sometimes \mathbb.
      ("\\mathbb{Q}" . ?ℚ)
      ("\\mathbb{R}" . ?ℝ)
      ("\\mathbb{T}" . ?𝕋)
      ("\\mathbb{Z}" . ?ℤ)
      ("\\pm" . ?±)
      ("\\|" . ?‖)
      ("\\varkappa" . ?ϰ)
      ;; caligraphic
      ("\\mathcal{A}" . ?𝒜)
      ("\\mathcal{B}" . ?ℬ)
      ("\\mathcal{C}" . ?𝒞)
      ("\\mathcal{D}" . ?𝒟)
      ("\\mathcal{E}" . ?ℰ)
      ("\\mathcal{F}" . ?ℱ)
      ("\\mathcal{G}" . ?𝒢)
      ("\\mathcal{H}" . ?ℋ)
      ("\\mathcal{I}" . ?ℐ)
      ("\\mathcal{J}" . ?𝒥)
      ("\\mathcal{K}" . ?𝒦)
      ("\\mathcal{L}" . ?ℒ)
      ("\\mathcal{M}" . ?ℳ)
      ("\\mathcal{N}" . ?𝒩)
      ("\\mathcal{O}" . ?𝒪)
      ("\\mathcal{P}" . ?𝒫)
      ("\\mathcal{Q}" . ?𝒬)
      ("\\mathcal{R}" . ?ℛ)
      ("\\mathcal{S}" . ?𝒮)
      ("\\mathcal{T}" . ?𝒯)
      ("\\mathcal{U}" . ?𝒰)
      ("\\mathcal{V}" . ?𝒱)
      ("\\mathcal{W}" . ?𝒲)
      ("\\mathcal{X}" . ?𝒳)
      ("\\mathcal{Y}" . ?𝒴)
      ("\\mathcal{Z}" . ?𝒵)
      ;; fractur
      ("\\mathfrak{A}" . ?𝔄)
      ("\\mathfrak{B}" . ?𝔅)
      ("\\mathfrak{C}" . ?ℭ)
      ("\\mathfrak{D}" . ?𝔇)
      ("\\mathfrak{E}" . ?𝔈)
      ("\\mathfrak{F}" . ?𝔉)
      ("\\mathfrak{G}" . ?𝔊)
      ("\\mathfrak{H}" . ?ℌ)
      ("\\mathfrak{I}" . ?ℑ)
      ("\\mathfrak{J}" . ?𝔍)
      ("\\mathfrak{K}" . ?𝔎)
      ("\\mathfrak{L}" . ?𝔏)
      ("\\mathfrak{M}" . ?𝔐)
      ("\\mathfrak{N}" . ?𝔑)
      ("\\mathfrak{O}" . ?𝔒)
      ("\\mathfrak{P}" . ?𝔓)
      ("\\mathfrak{Q}" . ?𝔔)
      ("\\mathfrak{R}" . ?ℜ)
      ("\\mathfrak{S}" . ?𝔖)
      ("\\mathfrak{T}" . ?𝔗)
      ("\\mathfrak{U}" . ?𝔘)
      ("\\mathfrak{V}" . ?𝔙)
      ("\\mathfrak{W}" . ?𝔚)
      ("\\mathfrak{X}" . ?𝔛)
      ("\\mathfrak{Y}" . ?𝔜)
      ("\\mathfrak{Z}" . ?ℨ)
      ("\\mathfrak{a}" . ?𝔞)
      ("\\mathfrak{b}" . ?𝔟)
      ("\\mathfrak{c}" . ?𝔠)
      ("\\mathfrak{d}" . ?𝔡)
      ("\\mathfrak{e}" . ?𝔢)
      ("\\mathfrak{f}" . ?𝔣)
      ("\\mathfrak{g}" . ?𝔤)
      ("\\mathfrak{h}" . ?𝔥)
      ("\\mathfrak{i}" . ?𝔦)
      ("\\mathfrak{j}" . ?𝔧)
      ("\\mathfrak{k}" . ?𝔨)
      ("\\mathfrak{l}" . ?𝔩)
      ("\\mathfrak{m}" . ?𝔪)
      ("\\mathfrak{n}" . ?𝔫)
      ("\\mathfrak{o}" . ?𝔬)
      ("\\mathfrak{p}" . ?𝔭)
      ("\\mathfrak{q}" . ?𝔮)
      ("\\mathfrak{r}" . ?𝔯)
      ("\\mathfrak{s}" . ?𝔰)
      ("\\mathfrak{t}" . ?𝔱)
      ("\\mathfrak{u}" . ?𝔲)
      ("\\mathfrak{v}" . ?𝔳)
      ("\\mathfrak{w}" . ?𝔴)
      ("\\mathfrak{x}" . ?𝔵)
      ("\\mathfrak{y}" . ?𝔶)
      ("\\mathfrak{z}" . ?𝔷)
      ("--" . ?–)
      ("---" . ?—)
      ("\\ordfeminine" . ?ª)
      ("\\ordmasculine" . ?º)
      ("\\lambdabar" . ?ƛ)
      ("\\celsius" . ?℃)
      ;; Text symbols formerly part of textcomp package:
      ("\\textdollar" . ?$)
      ("\\textborn" . ?*)
      ("\\textless" . ?<)
      ("\\textgreater" . ?>)
      ("\\textbackslash" . ?\\)
      ("\\textasciicircum" . ?^)
      ("\\textunderscore" . ?_)
      ("\\textbraceleft" . ?\{)
      ("\\textbar" . ?|)
      ("\\textbraceright" . ?\})
      ("\\textasciitilde" . ?~)
      ("\\textexclamdown" . ?¡)
      ("\\textcent" . ?¢)
      ("\\textsterling" . ?£)
      ("\\textcurrency" . ?¤)
      ("\\textyen" . ?¥)
      ("\\textbrokenbar" . ?¦)
      ("\\textsection" . ?§)
      ("\\textasciidieresis" . ?¨)
      ("\\textcopyright" . ?©)
      ("\\textordfeminine" . ?ª)
      ("\\guillemetleft" . ?«)
      ("\\guillemotleft" . ?«)
      ("\\textlnot" . ?¬)
      ("\\textregistered" . ?®)
      ("\\textasciimacron" . ?¯)
      ("\\textdegree" . ?°)
      ("\\textpm" . ?±)
      ("\\texttwosuperior" . ?²)
      ("\\textthreesuperior" . ?³)
      ("\\textasciiacute" . ?´)
      ("\\textmu" . ?µ)
      ("\\textparagraph" . ?¶)
      ("\\textpilcrow" . ?¶)
      ("\\textperiodcentered" . ?·)
      ("\\textonesuperior" . ?¹)
      ("\\textordmasculine" . ?º)
      ("\\guillemetright" . ?»)
      ("\\guillemotright" . ?»)
      ("\\textonequarter" . ?¼)
      ("\\textonehalf" . ?½)
      ("\\textthreequarters" . ?¾)
      ("\\textquestiondown" . ?¿)
      ("\\texttimes" . ?×)
      ("\\textdiv" . ?÷)
      ("\\textflorin" . ?ƒ)
      ("\\textasciicaron" . ?ˇ)
      ("\\textasciibreve" . ?˘)
      ("\\textacutedbl" . ?˝)
      ("\\textgravedbl" . 757)
      ("\\texttildelow" . 759)
      ("\\textbaht" . ?฿)
      ("\\textendash" . ?–)
      ("\\textemdash" . ?—)
      ("\\textbardbl" . ?‖)
      ("\\textquoteleft" . 8216)
      ("\\textquoteright" . 8217)
      ("\\quotesinglbase" . 8218)
      ("\\textquotedblleft" . 8220)
      ("\\textquotedblright" . 8221)
      ("\\quotedblbase" . 8222)
      ;; \textdagger and \textdied are replaced with DAGGER (#x2020) and
      ;; not with LATIN CROSS (#x271d)
      ("\\textdagger" . ?†)
      ("\\textdied" . ?†)
      ("\\textdaggerdbl" . ?‡)
      ("\\textbullet" . ?•)
      ("\\textellipsis" . ?…)
      ("\\textperthousand" . ?‰)
      ("\\textpertenthousand" . ?‱)
      ("\\guilsinglleft" . ?‹)
      ("\\guilsinglright" . ?›)
      ("\\textreferencemark" . ?※)
      ("\\textinterrobang" . ?‽)
      ("\\textfractionsolidus" . ?⁄)
      ("\\textlquill" . 8261) ; Literal ?⁅ breaks indentation
      ("\\textrquill" . 8262) ; Literal ?⁆ breaks indentation
      ("\\textdiscount" . ?⁒)
      ("\\textcolonmonetary" . ?₡)
      ("\\textlira" . ?₤)
      ("\\textnaira" . ?₦)
      ("\\textwon" . ?₩)
      ("\\textdong" . ?₫)
      ("\\texteuro" . ?€)
      ("\\textpeso" . ?₱)
      ("\\textguarani" . ?₲)
      ("\\textcelsius" . ?℃)
      ("\\textnumero" . ?№)
      ("\\textcircledP" . ?℗)
      ("\\textrecipe" . ?℞)
      ("\\textservicemark" . ?℠)
      ("\\texttrademark" . ?™)
      ("\\textohm" . ?Ω)
      ("\\textmho" . ?℧)
      ("\\textestimated" . ?℮)
      ("\\textleftarrow" . ?←)
      ("\\textuparrow" . ?↑)
      ("\\textrightarrow" . ?→)
      ("\\textdownarrow" . ?↓)
      ("\\textminus" . ?−)
      ("\\textsurd" . ?√)
      ("\\textlangle" . 9001) ; Literal ?〈 breaks indentation
      ("\\textrangle" . 9002) ; Literal ?〉 breaks indentation
      ("\\textblank" . ?␢)
      ("\\textvisiblespace" . ?␣)
      ("\\textopenbullet" . ?◦)
      ;; \textbigcircle is replaced with LARGE CIRCLE (#x25ef) and not
      ;; with COMBINING ENCLOSING CIRCLE (#x20dd)
      ("\\textbigcircle" . ?◯)
      ("\\textmusicalnote" . ?♪)
      ("\\textmarried" . ?⚭)
      ("\\textdivorced" . ?⚮)
      ("\\textlbrackdbl" . 10214) ; Literal ?⟦ breaks indentation
      ("\\textrbrackdbl" . 10215) ; Literal ?⟧ breaks indentation
      ("\\textinterrobangdown" . ?⸘)))
  )

(use-package ox
  :ensure nil
  :after org
  :commands org-export-dispatch
  :config
  (setq org-export-with-broken-links t)
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
                 \\usepackage{mhchem}
                 \\usepackage[inkscapelatex=false]{svg}
                 \\usepackage[utf8]{inputenc}
                 \\usepackage{tabularx}
                 \\usepackage{booktabs}
                 \\usepackage[T1]{fontenc}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\hypersetup{
	             colorlinks=true,       % false: boxed links; true: colored links
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
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]
                 \\linespread{1.1}
                 \\hypersetup{pdfborder=0 0 0}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))


    (add-to-list 'org-latex-classes '("ebook"
                                      "\\documentclass[11pt, oneside]{memoir}
                 \\setstocksize{9in}{6in}
                 \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
                 \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
                 \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
                 \\checkandfixthelayout
                 % Much more laTeX code omitted
                 "
                                      ("\\chapter{%s}" . "\\chapter*{%s}")
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")))

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
                   )))

  (use-package ox-hugo
    :ensure t
    :after ox)
  )

(use-package org-table-auto-align
  :hook
  (org-mode . org-table-auto-align-mode)
  :ensure (:host github :repo "Stefanomarton/org-table-auto-align-mode"))

(use-package org-src
  :after org
  :ensure nil
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (python . t) (latex . t)(gnuplot . t)(plantuml . t)))
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-src-fontify-natively t)
  (setq-default
   org-src-tab-acts-natively t
   org-src-preserve-indentation t))

(use-package org-download
  :ensure t
  :commands (my/org-download-clipboard)
  :init
  (setq org-download-display-inline-images 'posframe)
  (setq org-download-method 'directory)
  (setq org-download-image-latex-width 7)
  (setq org-download-heading-lvl nil)

  :config
  (defun custom/org-download-dir ()
    "Download files `org-attachments-folder'"
    (setq-local org-download-image-dir (concat
                                        (org-set-attachments-folder)
    			         			    "/")))

  (advice-add 'my/org-download-clipboard :before #'custom/org-download-dir)

  ;; Modify function to avoid writing useless comment
  (defun my-org-download-annotate-default (link)
    "Annotate LINK with the time of download."
    (format ""
            (if (equal link org-download-screenshot-file)
                "screenshot"
              link)
            (format-time-string "%Y-%m-%d %H:%M:%S")))

  (setq org-download-annotate-function 'my-org-download-annotate-default)


  (defun my/org-download-link-format-function (filename)
    "Generate an Org link with a cleaned path, removing everything before the timestamp."
    (let ((regex ".*/\\([0-9]+T[0-9]+/.*\\)"))
      (format "[[image-dir:%s]]"
              (if (string-match regex filename)
                  (match-string 1 filename)
                filename))))

  (defun my/org-download-clipboard ()
    (interactive)
    (setq-local org-download-link-format-function 'my/org-download-link-format-function)
    (org-download-clipboard)))


(use-package org-plantuml-mindmap
  :after org
  :ensure (:host github :repo "Stefanomarton/org-plantuml-mindmap")
  :config
  (setq org-plantuml-mindmap-result-type "svg")
  (advice-add 'org-plantuml-mindmap-create :before #'custom/plantuml-mindmap-folder)

  (defun custom/plantuml-mindmap-folder ()
    (setq-local org-plantuml-mindmap-folder (concat
    			                             (org-set-attachments-folder)
    			                             (file-name-sans-extension (buffer-name))
    			                             "/"))))

(use-package citar
  :ensure t
  :after (citar-denote)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)

  (citar-file-note-extensions)
  :config

  (setq citar-notes-paths (list (expand-file-name "uni/papers" notes-folder)))

  (setq citar-bibliography (expand-file-name ".resources/bibliography.bib" notes-folder))

  (defun citar-file-open (file)
	"Open FILE. Overwritten by hgi, to open pdf files from citar in external PDF viewer and not in internal one."
	(if (or (equal (file-name-extension file) "pdf") (equal (file-name-extension file) "html"))
		(citar-file-open-external (expand-file-name file))
	  (funcall citar-file-open-function (expand-file-name file))))

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after citar
  :config
  (citar-embark-mode))

(use-package org-appear
  :ensure (:type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autolinks t)
  (setq org-appear-autokeywords t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-trigger 'always))

(use-package anki-editor
  :bind
  (:map org-mode-map
        ("<escape>aP" . anki-editor-push-notes))
  :ensure (:repo "anki-editor/anki-editor")
  :config
  (setq anki-editor-default-note-type "Personal")
  (setq anki-editor-note-match "+LEVEL>1&+ANKI")
  (setq anki-editor-ignored-org-tags '("ANKI"))

  ;; ;; see anki-editor.el source file
  ;; (defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  ;;   (let ((org-use-property-inheritance t))
  ;;     (apply #'org-map-entries
  ;;            func
  ;;            anki-editor-note-match
  ;;            scope
  ;;            skip)))
  )

(use-package org-anki
  :ensure t
  :bind
  (:map org-mode-map
        ("<escape>as" . my/org-anki-sync-all)
        ("<escape>aS" . org-anki-sync-entry)
        ("<escape>ad" . org-anki-delete-entry)
        ("<escape>aD" . org-anki-delete-all)
        ("<escape>ab" . org-anki-browse-entry)
        ("<escape>aS" . org-anki-sync-entry)
        ("<escape>af" . oaff-create-flashcard)
        )

  :config
  (defun my/org-anki-sync-all ()
    (interactive)
    "set `org-use-property-inheritance' before `org-anki-sync-all'"
    (setq-local org-use-property-inheritance t)
    (org-anki-sync-all))

  ;; Match all level >1, inherit tag from parent level
  (setq org-anki-default-match "LEVEL>1&+ANKI")

  ;; Set `org-anki-model-fields' to use my custom note type
  (setq org-anki-model-fields
        '(("Personal" "Front" "Back")
          ("Basic" "Front" "Back")
          ("Basic (and reversed card)" "Front" "Back")
          ("Basic (optional reversed card)" "Front" "Back")
          ("NameDescr" "Name" "Descr")
          ("Cloze" "Text" "Extra")))
  (setq org-anki-default-note-type "Personal")

  (use-package org-anki-fast-flashcards
    :ensure (:host github :repo "Stefanomarton/org-anki-fast-flash-cards")
    :bind
    (:map org-mode-map
          ("<escape>af" . oaff-create-flashcard)))
  )

(use-package org-transclusion
  :ensure t
  :after org-mode
  :bind (:map org-mode-map
              ("<leader>ota" . org-transclusion-add)
              ("<leader>otm" . org-transclusion-mode)))

;; Cool org mode
(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  (org-modern-mode . org-margin-mode)
  :config
  (setq org-modern-todo nil
        org-modern-hide-stars nil
        org-modern-horizontal-rule nil
        org-modern-keyword "‣ "
        org-modern-star nil
        org-modern-block-fringe nil
        org-modern-table nil)
  )

;; Cool margin annotations
(use-package org-margin
  :hook (org-mode . org-margin-mode)
  :ensure (:host github :repo "rougier/org-margin")
  :requires svg-lib)

(use-package org-ipe
  :ensure (:host github :repo "Stefanomarton/org-ipe"))

(provide 'orgconfig)

;;; org-config ends here
