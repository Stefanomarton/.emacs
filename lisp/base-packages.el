;;; base-packages.el -*- lexical-binding: t; -*-

;; Recent file list
(use-package recentf
  :config
  (add-hook 'emacs-startup-hook 'recentf-mode)
  (add-hook 'after-init-hook
            (lambda ()
	          (setq inhibit-message t)
	          (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 25))


;; Zoxide
(use-package zoxide
  :bind (:map evil-normal-state-map
              ("gz" . zoxide-find-file)
              ))


;;Which-key
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))


;; Embark
(use-package embark
  :config

  ;; Base keybindings
  ;; (evil-define-key 'normal 'global (kbd "<leader>SPC") 'embark-minimal-act)
  ;; (evil-define-key 'normal 'global (kbd "C-.") 'embark-dwim)
  ;; (evil-define-key 'insert 'global (kbd "C-.") 'embark-minimal-act)
  ;; (evil-define-key 'visual 'global (kbd "<leader>SPC") 'embark-minimal-act)

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

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight (:host github :repo "oantolin/embark"
		           :files ("embark-consult.el"))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :after (embark consult)
  :bind (:map embark-become-file+buffer-map
	          ("m" . consult-bookmark)
	          ("b" . consult-buffer)
	          ("j" . consult-find)))


;; Nice auto formatting
;; Format-all
(use-package format-all
  :hook
  (format-all-mode . format-all-ensure-formatter)
  (prog-mode . format-all-mode)
  (LaTeX-mode . format-all-mode)
  (markdown-mode . format-all-mode)
  :config
  (setq format-all-show-errors 'error))


;;Outline
(use-package outline
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (python-ts-mode . outline-minor-mode))


;;Scratchbuffer
(use-package scratch
  :straight t
  :preface
  (defun my/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))


;;Hydra
(use-package hydra
  :defer 0.5)

;; Embrace
(use-package embrace
  :bind
  (:map global-map
        ("C-," . embrace-commander))
  :config
  (add-hook 'org-mode-hook #'embrace-org-mode-hook)

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (embrace-add-pair ?m "\\[\n" "\\]\n" nil t)))
  )
;; (define-key global-map (kbd "C-,") #'embrace-commander))


(use-package selected
  :init
  (add-hook 'after-init-hook 'selected-global-mode)
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

        ("u" . undo-in-region)

        ("p" . surround-region-with-parethesis)
        ("s" . surround-region-with-square-brackets)
        ("c" . surround-region-with-curly-brackets)

        ("{" . surround-region-newline-with-curly-brackets)

        ;; Environments
        ("ec" . (lambda () (interactive) (LaTeX-insert-environment "center")))
        ("eb" . (lambda () (interactive) (LaTeX-insert-environment "bx")))
        ("ee" . (lambda () (interactive) (LaTeX-insert-environment "equation*")))
        ("eE" . (lambda () (interactive) (LaTeX-insert-environment "equation")))
        ("eg" . (lambda () (interactive) (LaTeX-insert-environment "gather*")))
        ("eG" . (lambda () (interactive) (LaTeX-insert-environment "gather")))

        (";" . comment-dwim)

        ("m" . surround-region-with-math)
        ("M" . surround-region-newline-with-math)
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

  (defun surround-region-newline--surround (opening-delimiter closing-delimiter)
    "Surround the active region with hard-coded strings"
    (when (region-active-p)
      (save-excursion
        (let ((beginning (region-beginning))
              (end (region-end)))

          (goto-char beginning)
          (insert opening-delimiter)

          (goto-char (+ end (length closing-delimiter)))
          (insert closing-delimiter)

          (goto-char beginning)
          (newline)
          ))))

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
    (surround-region-newline--surround "{" "}"))

  (defun surround-region-with-math ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region--surround "\\\(" "\\\)"))

  (defun surround-region-newline-with-math ()
    "Surround the active region with hard-coded strings"
    (interactive)
    (surround-region-newline--surround "\\[" "\\]"))

  (selected-global-mode))

(use-package tab-jump-out
  :hook after-init
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1)))

(provide 'base-packages)
