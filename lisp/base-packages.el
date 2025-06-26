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
  :bind
  (:map global-map
        ("<escape> <escape>" . embark-minimal-act)
        ("C-." . embark-minimal-act))
  (:map vertico-map
	    ("C-." . embark-minimal-act))
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

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :after (embark consult)
  :bind (:map embark-become-file+buffer-map
	          ("m" . consult-bookmark)
	          ("b" . consult-buffer)
	          ("j" . consult-find)))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

                                        ;Outline
(use-package outline
  :ensure nil
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (python-ts-mode . outline-minor-mode)
  (typst-ts-mode . (lambda () (setq-local outline-default-state 1)))
  )

;;Scratchbuffer
(use-package scratch
  :ensure t
  :bind ("C-c s" . scratch))

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

(use-package vundo
  :ensure t)

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

;; (use-package meow
;;   :ensure t
;;   :config
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-overwrite-define-key
;;      '("k" . meow-next)
;;      '("l" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; SPC j/k will run the original command in MOTION state.
;;      '("j" . "H-j")
;;      '("k" . "H-k")
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("o" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("j" . meow-left)
;;      '("J" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("O" . meow-open-above)
;;      '("k" . meow-next)
;;      '("K" . meow-next-expand)
;;      '("l" . meow-prev)
;;      '("L" . meow-prev-expand)
;;      '("/" . meow-right)
;;      '("?" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("h" . meow-block)
;;      '("H" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;;   (meow-setup)
;;   (meow-global-mode 1)
;;   )


(provide 'base-packages)
