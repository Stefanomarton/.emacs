;;; editor-config.el -*- lexical-binding: t; -*-

;; Editorconfig, auto set indenting
(use-package editorconfig
  :ensure t
  :after find-file
  :config
  (editorconfig-mode 1)
  )

;; Autopair parenthesis
(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-layout-mode)
  (org-mode . electric-layout-mode)
  :config
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil) ;; more annoying than useful
  (setq electric-pair-delete-adjacent-pairs nil) ;; more annoying than useful
  )

(use-package paren
  :ensure nil
  :hook
  (prog-mode . show-paren-mode)
  (text-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)

  ;; Highlight parenthesis when inside it
  (define-advice show-paren-function (:around (fn) fix)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn)))))
  )

;; Highlight nested parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :inherit 'error
                      :box t)
  )

;; Highlight colorstring with the right color
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  )

(use-package avy
  :ensure t
  :bind
  ("<escape> f" . avy-goto-char-in-line-end)
  ("<escape> F" . avy-goto-char-in-line-beg)
  ("<escape> j" . avy-goto-char-timer)
  ("C-c k" . pop-global-mark)
  :preface

  (defun avy-goto-char-in-line-end (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (let ((current-point (point)))
      (avy-with avy-goto-char
        (avy-jump
         (regexp-quote (string char))
         :beg current-point
         :end (line-end-position)
         ))))

  (defun avy-goto-char-in-line-beg (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (let ((current-point (point)))
      (avy-with avy-goto-char
        (avy-jump
         (regexp-quote (string char))
         :beg (line-beginning-position)
         :end current-point
         ))))
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; Home row only (the default).
  (setq avy-words
        '("am" "by" "if" "is" "it" "my" "ox" "up"
          "ace" "act" "add" "age" "ago" "aim" "air"
          "ale" "all" "and" "ant" "any" "ape" "apt")))


(use-package expand-region
  :ensure t
  :bind
  (:map global-map
        ("C-<escape>" . er/expand-region))
  :config
  (set-default 'er--show-expansion-message nil)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode)
  (setq expand-region-subword-enabled nil)
  (setq expand-region-smart-cursor t)
  (setq expand-region-show-usage-message nil
        expand-region-fast-keys-enabled t
        expand-region-contract-fast-key "-"
        expand-region-reset-fast-key "r")
  )


;; Visual indicator when recording macros
(use-package kmacro
  :ensure nil
  :defer t
  :config
  (defsubst my/mode-line-macro-recording ()
    "Display macro being recorded."
    (when (or defining-kbd-macro executing-kbd-macro)
      (let ((sep (propertize " " 'face 'highlight ))
            (vsep (propertize " " 'face '(:inherit variable-pitch))))
        ;; "●"
        (propertize (concat sep "MACRO" vsep
                            (number-to-string kmacro-counter) vsep
                            "▶" sep)
                    'face 'highlight))))

  (setq-default mode-line-format
                (cl-pushnew '(:eval (my/mode-line-macro-recording))
                            (default-value 'mode-line-format)
                            :test 'equal)))

(provide 'editor-config)

;;; editor-config.el ends here
