;;; base-packages.el -*- lexical-binding: t; -*-

;; Recent file list
(use-package recentf
  :ensure nil
  :config
  (add-hook 'emacs-startup-hook 'recentf-mode)
  (add-hook 'after-init-hook
            (lambda ()
	          (setq inhibit-message t)
	          (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 25))

;; Embark
(use-package embark
  :ensure t
  :demand t
  :bind
  (:map global-map
        ("<escape> <escape>" . embark-minimal-act)
        ("C-." . embark-minimal-act)
        )
  :config
  ;; Which-key style indicator
  (defun embark-minimal-act (&optional arg)
    (interactive "P")
    (let ((embark-indicators
	       '(embark-which-key-indicator
	         embark-highlight-indicator
	         embark-isearch-highlight-indicator)))
      (embark-act arg)))

  (defun embark-minimal-act-noexit ()
    (interactive)
    (embark-minimal-act 4))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		         nil
		         (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-indicators #'embark-which-key-indicator)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	      (which-key--hide-popup-ignore-command)
	    (which-key--show-keymap
	     (if (eq (caar targets) 'embark-become)
	         "Become"
	       (format "Act on %s '%s'%s"
		           (plist-get (car targets) :type)
		           (embark--truncate-target (plist-get (car targets) :target))
		           (if (cdr targets) "…" "")))
	     (if prefix
	         (pcase (lookup-key keymap prefix 'accept-default)
	           ((and (pred keymapp) km) km)
	           (_ (key-binding prefix 'accept-default)))
	       keymap)
	     nil nil t))))

  (setq embark-cycle-key "SPC")
  (setq embark-quit-after-action t)
  ;; evil keybindings
  ;; (if (featurep 'evil)
  ;;     (progn
  ;;       ;; Code to execute when Evil mode is active
  ;;       (evil-define-key 'normal 'global (kbd "<leader>SPC") 'embark-minimal-act)
  ;;       (evil-define-key 'normal 'global (kbd "C-.") 'embark-dwim)
  ;;       (evil-define-key 'insert 'global (kbd "C-.") 'embark-minimal-act)
  ;;       (evil-define-key 'visual 'global (kbd "<leader>SPC") 'embark-minimal-act)
  ;;       ))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  ;; :ensure (:host github :repo "oantolin/embark" :files ("embark-consult.el"))
  ;; :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :after (embark consult)
  :bind (:map embark-become-file+buffer-map
	          ("m" . consult-bookmark)
	          ("b" . consult-buffer)
	          ("j" . consult-find)))


;; Nice auto formatting
;; Format-all
;; (use-package format-all
;;   :ensure t
;;   :hook ((prog-mode        . format-all-mode)      ; run on every code buffer
;;          (LaTeX-mode       . format-all-mode)
;;          (markdown-mode    . format-all-mode)
;;          (format-all-mode  . format-all-ensure-formatter))
;;   :config
;;   ;; 3 ▸ make typstyle the default formatter for Typst files
;;   (setq-default format-all-formatters
;;                 '(("Typst" typstyle)           ; symbol, not (typstyle)
;;                   ("TOML"  taplo-fmt)))

;;   ;; show errors only when the formatter really fails
;;   (setq format-all-show-errors 'error))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

;;Outline
(use-package outline
  :ensure nil
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (python-ts-mode . outline-minor-mode))


;;Scratchbuffer
(use-package scratch
  :ensure t
  ;;   :preface
  ;;   (defun my/scratch-buffer-setup ()
  ;;     "Add contents to `scratch' buffer and name it accordingly.
  ;; If region is active, add its contents to the new buffer."
  ;;     (let* ((mode major-mode))
  ;;       (rename-buffer (format "*Scratch for %s*" mode) t)))
  ;;   :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

;;Hydra
(use-package hydra
  :ensure t
  :defer 0.5)

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

(use-package selected
  :ensure t
  :init
  (selected-global-mode)
  :bind (:map selected-keymap
              ("U" . upcase-region)
              ("D" . downcase-region)
              ("C" . capitalize-region)

              ("u" . undo-in-region)

              ("p" . surround-region-with-parethesis)
              ("s" . surround-region-with-square-brackets)
              ("c" . surround-region-with-curly-brackets)

              ("{" . surround-region-newline-with-curly-brackets)

              (";" . comment-dwim)


              ("a" . back-to-indentation)
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

        ("a" . back-to-indentation)

        ("y" . kill-ring-save)
        ("J" . fix-pasted-text)

        ("q" . er/mark-inside-quotes)
        ("\(" . er/mark-outside-pairs))

  :config

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

  (selected-global-mode))

(use-package tab-jump-out
  :ensure t
  :hook after-init
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1)))

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))

(use-package undo-fu
  :ensure t
  :commands (undo))

;; ;; Persistent undo
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-linear t)
  (undo-fu-session-global-mode))

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (undo-tree-mode))

(use-package simple
  :ensure nil
  :config
  (setq save-interprogram-paste-before-kill t)
  :init
  (defun pulse-current-region (&rest _)
    (if mark-active
        (pulse-momentary-highlight-region (region-beginning) (region-end))
      (pulse-momentary-highlight-region (mark) (point))))
  (advice-add #'kill-ring-save :before #'pulse-current-region))

(defun my-replace-region-with-regex-in-buffer ()
  "Search and replace the text in the region under the cursor throughout the entire file using regex."
  (interactive)
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (goto-char (point-min))
        (let ((search-text (read-string (format "Search for regex (default %s): " region-text) nil nil region-text))
              (replace-text (read-string (format "Replace '%s' with: " region-text))))
          (query-replace-regexp search-text replace-text)))
    (message "No region selected")))

(global-set-key (kbd "C-c r") 'my-replace-region-with-regex-in-buffer)

(provide 'base-packages)
