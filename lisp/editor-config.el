;;; editor-config.el -*- lexical-binding: t; -*-

;; Editorconfig, auto set indenting
(use-package editorconfig
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
  :straight (:type built-in)
  :hook
  (prog-mode . electric-layout-mode)
  (org-mode . electric-layout-mode)
  :config
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil) ;; more annoying than useful
  (setq electric-pair-delete-adjacent-pairs nil) ;; more annoying than useful
  )

(use-package paren
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
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
		              :foreground "red"
		              :inherit 'error
		              :box t)
  )

;; Highlight colorstring with the right color
(use-package rainbow-mode
  :commands rainbow-mode
  :config
  (add-hook 'prog-mode #'rainbow-mode)
  )

(use-package avy
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

(use-package expreg
  :bind
  (:map global-map
        ("C-<escape>" . expreg-expand)))

;; (use-package expand-region
;;   :hook
;;   (org-mode . er/add-latex-in-org-mode-expansions)
;;   :config
;;   (set-default 'er--show-expansion-message nil)
;;   (setq expand-region-subword-enabled nil)
;;   (setq expand-region-smart-cursor t)
;;   (setq expand-region-show-usage-message nil
;;         expand-region-fast-keys-enabled t
;;         expand-region-contract-fast-key "-"
;;         expand-region-reset-fast-key "r")

;;   ;; (evil-define-key 'normal 'global (kbd "<backspace>") 'er/expand-region)
;;   ;; (evil-define-key 'emacs 'global (kbd "C-<backspace>") 'er/expand-region
;;   ;; )

;;   (define-key global-map (kbd "C-<escape>") 'er/expand-region)


(provide 'editor-config)

;;; editor-config.el ends here
