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
        gptel-rewrite-default-action 'diff)

  (defun my/gptel-typst-directive ()
    (when (derived-mode-p 'typst-ts-mode 'typst-mode)
      "You edit Typst markup. Rules:
- Output ONLY the rewritten region. No code fences, no explanations.
- Never change text, numbers, units or references inside cells or content blocks.
- Only change structure, alignment and whitespace, unless the instruction says otherwise.
- Use idiomatic current Typst: table.header, table.cell, columns as tuples or auto.
- Keep the code compilable."))

  (add-hook 'gptel-rewrite-directives-hook #'my/gptel-typst-directive))

(provide 'tools)

;;; tools.el ends here
