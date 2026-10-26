;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode))

(use-package winner
  :ensure nil
  :config
  (winner-mode))

(use-package sudo-edit
  :ensure t
  :commands sudo-edit)

(use-package focus
  :ensure t
  :commands (focus-mode focus-read-only-mode))

(use-package term-toggle
  ;; Solo term-toggle.el: helm-term-toggle.el tira dentro helm (che rompe package-vc) e
  ;; term-toggle-animate.el richiede term-toggle stesso (dipendenza circolare)
  :vc (:url "https://github.com/amno1/emacs-term-toggle" :branch "master"
       :main-file "term-toggle.el"
       :ignored-files ("helm-term-toggle.el" "term-toggle-animate.el"))
  :bind (("<escape>." . term-toggle-eshell)))

(use-package eshell
  :commands eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  (setq  eshell-scroll-to-bottom-on-input 'all
         eshell-error-if-no-glob t
         eshell-hist-ignoredups t
         eshell-save-history-on-exit t
         eshell-prefer-lisp-functions nil
         eshell-destroy-buffer-when-process-dies t)
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize "[" 'face `(:foreground "#2aa198"))
           (propertize (user-login-name) 'face `(:foreground "#dc322f"))
           (propertize "@" 'face `(:foreground "#2aa198"))
           (propertize (system-name) 'face `(:foreground "#268bd2"))
           (propertize "]──[" 'face `(:foreground "#2aa198"))
           (propertize (concat (eshell/pwd)) 'face `(:foreground "#93a1a1"))
           (propertize "]\n" 'face `(:foreground "#2aa198"))
           (propertize (if (= (user-uid) 0) " # " " λ ") 'face `(:foreground "#2aa198"))
           ))))

(use-package visual-regexp
  :vc (:url "https://github.com/benma/visual-regexp.el" :branch "master")
  :bind (              ;; ([remap isearch-forward] . vr/isearch-forward)
         ;; ([remap isearch-backward] . vr/isearch-backward)
         ("C-M-s" . vr/replace)
         )
  :config
  (setq vr/auto-show-help nil)

  (use-package visual-regexp-steroids
    :after visual-regexp
    :vc (:url "https://github.com/benma/visual-regexp-steroids.el" :branch "master"))
  )


(use-package calc
  :ensure nil
  :bind
  ("<escape> c c" . calc)
  :config


  (defun eval-math-expr (beg end)
    (interactive "r")
    (require 'calc)
    (let ((result
           (calc-eval
            (buffer-substring beg end))))
      (save-excursion
        (goto-char end) (insert " = " result))))
  )

(use-package diff
  :ensure nil
  :bind
  ("C-c u d" . diff-current-buffer-with-file)
  :config
  (defun diff-current-buffer-with-file ()
    (interactive)
    (diff-buffer-with-file (current-buffer))))


(use-package esup
  :ensure t
  :commands esup
  :config
  (setq esup-depth 0))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :bind ("C-c r" . gptel-rewrite)
  :init
  ;; In :init so Typst buffers opened before gptel loads get it too.
  (add-hook 'typst-ts-mode-hook
            (lambda () (add-hook 'gptel-post-rewrite-functions #'my/gptel-typst-postfix nil t)))
  :config
  (setq gptel-backend
        (gptel-make-openai "homelab"
          :host "192.168.1.220:8080"
          :protocol "http"
          :stream t
          :key (lambda ()
                 (string-trim
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "~/.secrets/llama-swap-key"))
                    (buffer-string))))
          :models '(gemma-moe:nothink qwen38:nothink))
        gptel-model 'gemma-moe:nothink
        gptel-include-reasoning nil
        gptel-rewrite-default-action 'accept)

  ;; Typst buffers use Claude Haiku (API key from the vault, file by secret-put);
  ;; everything else, dictation included, stays on the homelab model.
  (defvar my/gptel-claude
    (gptel-make-anthropic "Claude"
      :stream t
      :key (lambda ()
             (string-trim
              (with-temp-buffer
                (insert-file-contents (expand-file-name "~/.secrets/anthropic-key"))
                (buffer-string))))
      :request-params '(:output_config (:effort "low"))))

  (defun my/gptel-use-claude ()
    (setq-local gptel-backend my/gptel-claude
                gptel-model 'claude-haiku-5-5))
  (add-hook 'typst-ts-mode-hook #'my/gptel-use-claude)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'typst-ts-mode) (my/gptel-use-claude))))

  (defconst my/gptel-typst-prompt
    "You edit Typst (NOT LaTeX, NOT Markdown) for an Italian chemist. By default, unless the instruction says otherwise:
fix errors and return the region as clean, idiomatic, compilable Typst, math included.

Output:
- ONLY the rewritten region. No code fences, no explanations, no preamble.
- Keep language (Italian), meaning, numbers, heading levels (= == ===), labels <...> and references @... unchanged.
  Never invent labels.
- Fix typos, grammar, accents (è, perché, più; never e' or perche'), broken Typst syntax, LaTeX or Markdown leftovers.
- When the region already follows a convention (chemistry notation, helper functions, decimal style), keep it.
  The rules below are the defaults.
- Format like typstyle: 2-space indent, long argument lists one per line with a trailing comma.

Markup:
- Headings `= `, `== `, sentence case, no final period. Bold *x* for key terms, italic _x_ for foreign or
  technical words. Never #strong or #emph.
- Lists `- `, numbered `+ `, terms `/ Term: desc`. Item with a title: `+ *Titolo* \\` with the description on the next line.
- Link #link(\"url\")[text]. Footnote #footnote[...]. Cite/ref @key. Comments //.
- Literal # $ * _ @ < ~ in text: escape with \\. A bare ~ is a non-breaking space: \"about 120\" is \\~120.
- No colors, fill, stroke, #text(size: ...) or hex values: the templates style everything.

Symbols and units, written in native Typst, never as Unicode characters:
- Quantity in prose: number, space, unit: 25 #sym.degree;C, 0,45 #sym.mu;m, 9,81 m/s#super[2], 1146 cm#super[-1],
  2 #sym.times 10#super[5]. The ; ends the symbol before a letter.
- Symbols in prose: #sym.plus.minus #sym.approx #sym.arrow.r #sym.lt.eq #sym.gt.eq #sym.dot.c, Greek #sym.alpha.
  Ranges with --: 50--60 #sym.degree;C. Dash ---.
- Decimal comma in prose and tables (0,45); decimal point inside $...$.
- Variables and formulas go in math: the angle $theta$, $C_\"tap\"$, $gamma_(l v)$.
- Chemistry: keep the region's notation (#ce(\"...\"), #ch(\"...\"), $\"H\"_2\"O\"$); otherwise H#sub[2]O,
  SO#sub[4]#super[2-], Fe#super[3+]. Never H_2O outside math (_ starts italic).

Layout:
- Two columns side by side, always a grid: #grid(columns: 2, column-gutter: 1em, [left], [right]);
  uneven widths columns: (60%, 40%). Move the existing content in, drop nothing.
- Figure: #figure(image(\"file.png\", width: 50%), caption: [Frase completa con punto finale.]). Widths in %.
- Tables: plain, the template draws the rules: no stroke, fill or hline, no bold in the header.
  #table(columns: (auto, 1fr), align: (left, right), table.header([Sostanza], [Conc. (mg/l)]), [a], [b], [c], [d])
  One [cell] per cell, cells separated by commas. Units in the header in parentheses. Empty cell [---].
  Merged cell: table.cell(colspan: 2)[...]. Caption wanted: #figure(table(...), caption: [...]).
- Box-drawing or ASCII tables (┌ │ ├ ─ + |): convert to #table, first row in table.header,
  one cell per box, text wrapped over several lines joined into one cell.

Math (no backslash commands EVER; \\ in math is only a line break):
- Inline $x$. Display: $ on its own line, body indented 2 spaces, closing $ on its own line.
- Greek: alpha beta gamma Gamma delta Delta epsilon theta lambda mu pi rho sigma phi omega.
  Never \\alpha.
- Multi-letter text: \"text\". Products need spaces: a b, not ab.
- Subscript or superscript words go in quotes: $C_\"tap\"$, $P_\"ow\"$, $T_\"max\"$ (one letter needs none: $S_w$).
- Grouping uses parentheses, not braces: x_(i j), x^(n+1), sum_(i=1)^n, integral_0^oo f(x) dif x, lim_(x -> 0).
- Fractions: a/b, (a+b)/(c+d), frac(a, b). Roots: sqrt(x), root(3, x). Units in math: 9.81 \"m/s\"^2.
- Align with & and break lines with \\:  a &= b + c \\
  &= d
- LaTeX -> Typst:
  \\frac{a}{b} -> (a)/(b) | \\cdot -> dot | \\times -> times | \\pm -> plus.minus
  \\leq \\geq \\neq -> <= >= != | \\approx -> approx | \\to -> -> | \\Rightarrow -> =>
  \\infty -> oo | \\partial -> partial | \\nabla -> nabla | \\in -> in | \\cup \\cap -> union inter
  \\mathbb{R} -> RR | \\text{x} -> \"x\" | \\mathrm{d}x -> dif x | \\operatorname{tr} -> op(\"tr\")
  \\vec{v} -> arrow(v) | \\hat{x} -> hat(x) | \\bar{x} -> overline(x) | |x| -> abs(x)
  \\left( \\right) -> ( ) (auto-sized) | pmatrix -> mat(1, 2; 3, 4) | cases -> cases(x \"if\" x > 0, 0 \"else\")
  $$...$$ or \\[...\\] -> display math | \\ce{H2O} -> H#sub[2]O
- Built-in functions stay plain: sin x, cos x, log x, ln x, exp(x), $log P_\"ow\"$.

Before answering, check:
- every ~ meaning \"about\" is written \\~;
- a space between number and unit (40 #sym.degree;C), ranges written 50--60;
- symbols, units and powers in prose use #sym, #super and #sub, not Unicode characters;
- every table cell is its own [...] followed by a comma.")

  (defun my/gptel-typst-directive ()
    (when (derived-mode-p 'typst-ts-mode 'typst-mode)
      (let ((helpers (my/typst-helpers)))
        (concat my/gptel-typst-prompt "\n\n"
                (if helpers
                    (concat "Custom functions available in this file (from its templates). Use one only when the
instruction asks for what it does or the region already uses it; never call custom functions not listed here:\n"
                            (mapconcat (lambda (h) (concat "- " (cdr h))) helpers "\n"))
                  "This file has no custom functions: use only built-in Typst.")))))

  ;; Custom functions the buffer can call: its own top-level #let functions plus
  ;; those of the files it imports (recursively), filtered by the imported names.
  (defun my/typst--import-path (spec dir)
    (if (string-prefix-p "/" spec)
        (when-let* ((root (locate-dominating-file
                           dir (lambda (d) (file-exists-p (expand-file-name (substring spec 1) d))))))
          (expand-file-name (substring spec 1) root))
      (let ((f (expand-file-name spec dir))) (and (file-exists-p f) f))))

  (defun my/typst--params ()
    (let ((beg (point)))
      (ignore-errors
        (forward-sexp)
        (truncate-string-to-width
         (replace-regexp-in-string
          "[ \t\n]+" " " (replace-regexp-in-string
                           "//[^\n]*" "" (buffer-substring-no-properties beg (point))))
         160 nil nil "…"))))

  (defun my/typst--scan (dir seen)
    "Alist (NAME . SIGNATURE) of the functions the current buffer defines or imports."
    (let (out)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^#import \"\\([^\"]+\\)\"\\(?: +as +[[:alnum:]_-]+\\)?\\(?: *: *\\([^/\n]+\\)\\)?" nil t)
          (let* ((spec (match-string 1))
                 (names (and (match-string 2) (not (string-prefix-p "*" (match-string 2)))
                             (split-string (match-string 2) "[ ,()]+" t)))
                 (file (unless (string-prefix-p "@" spec) (my/typst--import-path spec dir)))
                 (found (if file
                            (unless (member file seen)
                              (push file seen)
                              (with-temp-buffer
                                (insert-file-contents file)
                                (my/typst--scan (file-name-directory file) seen)))
                          (mapcar (lambda (n) (cons n (format "%s (package %s)" n spec))) names))))
            (dolist (h found)
              (when (or (null names) (member (car h) names)) (push h out)))))
        (goto-char (point-min))
        (while (re-search-forward "^#let \\([[:alnum:]_-]+\\)" nil t)
          (let ((name (match-string 1)) sig)
            (cond ((looking-at "(") (setq sig (my/typst--params)))
                  ((looking-at " *= *(")
                   (goto-char (1- (match-end 0)))
                   (let ((p (my/typst--params))) (when (looking-at " *=>") (setq sig p))))
                  ((looking-at " *= *[[:alnum:]_.-]+\\.with(") (setq sig "(…)")))
            (when sig (push (cons name (concat name sig)) out)))))
      out))

  (defun my/typst-helpers ()
    (seq-uniq (my/typst--scan default-directory (and buffer-file-name (list buffer-file-name)))
              (lambda (a b) (equal (car a) (car b)))))

  (add-hook 'gptel-rewrite-directives-hook #'my/gptel-typst-directive)

  ;; The local model still writes $\log x$ now and then: Typst math has no
  ;; backslash commands, so drop a backslash before a letter inside $...$.
  (defun my/gptel-typst-postfix (beg end)
    (save-excursion
      (let ((end (copy-marker end)))
        (goto-char beg)
        (when (looking-at "What is the required change\\?.*\n+") (replace-match ""))
        (while (re-search-forward "\\$[^$]+\\$" end t)
          (let ((math (match-string 0)))
            (pcase-dolist (`(,re . ,rep) my/gptel-typst-math-fixes)
              (setq math (replace-regexp-in-string re rep math t)))
            (replace-match math t t))))))

  ;; LaTeX leftovers the local model still writes inside $...$, in order.
  (defconst my/gptel-typst-math-fixes
    '(("\\\\?\\(?:text\\|mathrm\\|rm\\){\\([^{}]*\\)}" . "\"\\1\"") ; \text{m} -> "m"
      ("\\(\"\\(?:\\^[0-9]+\\)?\\)/\"" . "\\1\"/")                     ; "m"^2/"s" -> "m"^2"/s"
      ("\\([_^]\\){\\([[:alpha:]][[:alpha:]]+\\)}" . "\\1\"\\2\"")      ; C_{tap} -> C_"tap"
      ("\\([_^]\\){\\([^{}]*\\)}" . "\\1(\\2)")                          ; x^{n+1} -> x^(n+1)
      ("\\\\\\([[:alpha:]]\\)" . "\\1"))))                               ; \log -> log

;; eat: terminale in puro elisp, niente modulo nativo da compilare come vterm
(use-package eat
  :ensure t
  :commands eat)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-window-width 60)
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'tools)

;;; tools.el ends here
