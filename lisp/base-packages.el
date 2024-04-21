;;; base-packages.el -*- lexical-binding: t; -*-

;; Recent file list
(use-package recentf
  :defer 0.5
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
  :commands (embark-minimal-act embark-dwim embark-act)
  :config

  ;; Base keybindings
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'embark-minimal-act)
  (evil-define-key 'normal 'global (kbd "C-.") 'embark-dwim)
  (evil-define-key 'insert 'global (kbd "C-.") 'embark-minimal-act)
  (evil-define-key 'visual 'global (kbd "<leader>SPC") 'embark-minimal-act)

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
  :config
  (add-hook 'org-mode-hook #'embrace-org-mode-hook)
  (define-key global-map (kbd "C-,") #'embrace-commander))

(provide 'base-packages)
