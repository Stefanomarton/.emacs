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

(use-package term-toggle
  :vc (:url "https://github.com/amno1/emacs-term-toggle" :branch "master")
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
  :config
  (setq esup-depth 0))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :bind ("C-c r" . gptel-rewrite)
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
        gptel-rewrite-default-action nil)

  (defun my/gptel-typst-directive ()
    (when (derived-mode-p 'typst-ts-mode 'typst-mode)
      "You edit Typst (NOT LaTeX, NOT Markdown). By default, unless the instruction says otherwise:
fix errors and return the region as clean, idiomatic, compilable Typst, math included.

Output:
- ONLY the rewritten region. No code fences, no explanations, no preamble.
- Keep language, meaning, numbers, units, labels <...> and references @... unchanged.
- Fix typos, grammar, broken Typst syntax, and any LaTeX or Markdown leftovers.

Markup:
- Headings `= `, `== `. Bold *x*, italic _x_. Lists `- `, numbered `+ `, terms `/ Term: desc`.
- Link #link(\"url\")[text]. Footnote #footnote[...]. Cite/ref @key. Comments //.
- Literal # $ * _ @ < in text: escape with \\.
- Every symbol or variable in prose goes inside dollars, even one letter: the angle $theta$, $gamma_(l v)$.
- Chemistry in text: H#sub[2]O, CO#sub[2], 10#super[3]. Never H_2O outside math (_ starts italic).
- Tables: table(columns: (auto, 1fr), table.header[*A*][*B*], [a], [b]); table.cell(colspan: 2)[...].

Math (no backslash commands EVER; \\ in math is only a line break):
- Inline $x$, display $ x $ (spaces inside the dollars).
- Greek: alpha beta gamma Gamma delta Delta epsilon theta lambda mu pi rho sigma phi omega.
  Never \\alpha.
- Multi-letter text: \"text\". Products need spaces: a b, not ab.
- Grouping uses parentheses, not braces: x_(i j), x^(n+1), sum_(i=1)^n, integral_0^oo f(x) dif x, lim_(x -> 0).
- Fractions: a/b, (a+b)/(c+d), frac(a, b). Roots: sqrt(x), root(3, x).
- Align with & and break lines with \\:  $ a &= b + c \\
  &= d $
- LaTeX -> Typst:
  \\frac{a}{b} -> (a)/(b) | \\cdot -> dot | \\times -> times | \\pm -> plus.minus
  \\leq \\geq \\neq -> <= >= != | \\approx -> approx | \\to -> -> | \\Rightarrow -> =>
  \\infty -> oo | \\partial -> partial | \\nabla -> nabla | \\in -> in | \\cup \\cap -> union inter
  \\mathbb{R} -> RR | \\text{x} -> \"x\" | \\mathrm{d}x -> dif x | \\operatorname{tr} -> op(\"tr\")
  \\vec{v} -> arrow(v) | \\hat{x} -> hat(x) | \\bar{x} -> overline(x) | |x| -> abs(x)
  \\left( \\right) -> ( ) (auto-sized) | pmatrix -> mat(1, 2; 3, 4) | cases -> cases(x \"if\" x > 0, 0 \"else\")
  $$...$$ or \\[...\\] -> $ ... $ (display, spaces inside) | align -> $ ... & ... \\ ... $
  \\ce{H2O} or H2O in prose -> H#sub[2]O (not \"H\"_2\"O\") | units: 9.81 \"m/s\"^2, 25 °C in prose
- Built-in functions stay plain: sin x, cos x, log x, ln x, exp(x)."))

  (add-hook 'gptel-rewrite-directives-hook #'my/gptel-typst-directive))

(provide 'tools)

;;; tools.el ends here
