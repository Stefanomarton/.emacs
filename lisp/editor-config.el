;;; editor-config.el -*- lexical-binding: t; -*-

;; Editorconfig, auto set indenting
(use-package editorconfig
  :ensure t
  :after find-file
  :config
  (editorconfig-mode 1)
  )

;; Code cleanup
(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      (lambda() (delete-trailing-whitespace))
		      )))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      'whitespace-cleanup
		      )))

;; Do not remove white spaces in markdown
(unless (derived-mode-p 'markdown-mode)
  (setq nuke-trailing-whitespace-p t))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      (lambda() (delete-trailing-whitespace))
		      )))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      'whitespace-cleanup
		      )))


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
  (add-hook 'prog-mode #'rainbow-mode)
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
	      "ale" "all" "and" "ant" "any" "ape" "apt"))

  (defun avy-action-kill-whole-line (pt)
    ;; Kill action for avy
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?x avy-dispatch-alist) 'avy-action-kill-stay
	    (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
	      (bounds-of-thing-at-point 'line)
	    (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
	    (alist-get ?c avy-dispatch-alist) 'avy-action-copy
	    (alist-get ?C avy-dispatch-alist) 'avy-action-copy-whole-line
	    (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line))

(use-package expand-region
  :ensure t
  :bind
  (:map global-map
        ("C-<escape>" . er/expand-region))
  :hook
  (org-mode . er/add-org-mode-expansions)
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

  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode)

  (defun er/mark-latex-text-sentence ()
    (unless (texmathp) (er/mark-text-sentence)))
  (defun er/mark-latex-text-paragraph ()
    (unless (texmathp) (er/mark-text-paragraph)))

  (defun my/find-bounds-of-regexps (open close)
    (let ((start (point))
          (parity 0)
          (open-close (concat "\\(?:" open "\\|" close "\\)"))
          end)
      (save-excursion
        (while (and (not (= parity -1))
                    (re-search-backward open-close nil t))
          (if (looking-at open)
              (setq parity (1- parity))
            (setq parity (1+ parity))))
        (setq end (point))
        (goto-char start)
        (while (and (not (= parity 0))
                    (re-search-forward open-close nil t))
          (if (looking-back
               close
               (- (point) (length (match-string-no-properties 0))))
              (setq parity (1+ parity))
            (setq parity (1- parity))))
        (when (= parity 0) (cons end (point))))
      ))

  (defun er/mark-LaTeX-inside-math ()
    "Mark text inside LaTeX math delimiters. See `er/mark-LaTeX-math'
  for details."
    (when (texmathp)
      (let* ((string (car texmathp-why))
             (pos (cdr texmathp-why))
             (reason (assoc string texmathp-tex-commands1))
             (type (cadr reason)))
        (cond
         ((eq type 'sw-toggle) ;; $ and $$
          (goto-char pos)
          (set-mark (1+ (point)))
          (forward-sexp 1)
          (backward-char 1)
          (exchange-point-and-mark))
         ((or (eq type 'sw-on)
              (equal string "Org mode embedded math")) ;; \( and \[
          (re-search-forward texmathp-onoff-regexp)
          (backward-char 2)
          (set-mark (+ pos 2))
          (exchange-point-and-mark))
         (t (error (format "Unknown reason to be in math mode: %s" type)))))))

  (defun er/mark-latex-inside-pairs ()
    (if (texmathp)
        (cl-destructuring-bind (beg . end)
            (my/find-bounds-of-regexps " *[{([|<]"
                                       " *[]})|>]")
          (when-let ((n (length (match-string-no-properties 0))))
            (set-mark (save-excursion
                        (goto-char beg)
                        (forward-char n)
                        (skip-chars-forward er--space-str)
                        (point)))
            (goto-char end)
            (backward-char n)
            (if (looking-back "\\\\right\\\\*\\|\\\\" (- (point) 7))
                (backward-char (length (match-string-no-properties 0)))))
          (skip-chars-backward er--space-str)
          (exchange-point-and-mark))
      (er/mark-inside-pairs)))

  (defun er/mark-latex-outside-pairs ()
    (if (texmathp)
        (cl-destructuring-bind (beg . end)
            (my/find-bounds-of-regexps " *[{([|<]"
                                       " *[]})|>]")
          (set-mark (save-excursion
                      (goto-char beg)                        ;; (forward-char 1)
                      (if (looking-back "\\\\left\\\\*\\|\\\\" (- (point) 6))
                          (backward-char (length (match-string-no-properties 0))))
                      (skip-chars-forward er--space-str)
                      (point)))
          (goto-char end)
          (skip-chars-backward er--space-str)
          ;; (backward-char 1)
          (exchange-point-and-mark))
      (er/mark-outside-pairs)))

  (defun mark-frac ()
    "Select the \\frac command and its contents."
    (interactive)
    (let ((current-point (point))
          (nearest-frac-pos (save-excursion
                              (re-search-backward "\\\\frac" nil t)
                              (point))))
      (when nearest-frac-pos
        (goto-char nearest-frac-pos)
        (let ((start (point))
              (end (progn
                     (forward-sexp)
                     (forward-sexp)
                     (forward-sexp))))
          (set-mark start)
          (goto-char end))
        )))


  (defun er/add-org-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list '(er/mark-word
                               er/mark-latex-inside-pairs
                               er/mark-latex-outside-pairs
                               er/mark-LaTeX-inside-math
                               er/mark-comment
                               er/mark-url
                               er/mark-email
                               er/mark-latex-text-sentence
                               er/mark-latex-text-paragraph
                               ))))


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
