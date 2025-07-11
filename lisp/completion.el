;;; completion.el --- Useful Tools -*- lexical-binding: t; -*-

;; Enable vertico for the best vertical completion experience
(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
	    ("C-k" . vertico-next)
	    ("C-l" . vertico-previous)
	    ("<escape>" . keyboard-escape-quit))
  :config

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Indicated in the documentation
  (setq fast-but-imprecise-scrolling t
	    jit-lock-defer-time 0)

  ;; Ignore case when completing
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; Different scroll margin
  (setq vertico-scroll-margin 2)

  ;; Show more candidates
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (vertico-mode)
  )

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (vertico-multiform-mode)

  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))
               )

  (setq vertico-multiform-commands
        '((execute-extended-command posframe)
          (consult-projectile posframe)))
  )


(use-package vertico-posframe
  :ensure t
  :config
  ;; (setq vertico-posframe-width 120)
  (setq vertico-posframe-border-width 3)
  (setq vertico-posframe-parameters
        '((left-fringe . 15)
          (right-fringe . 20)))
  )

;; orderless completion method
(use-package orderless
  :ensure t
  :config
  (setq orderless-affix-dispatch-alist nil)
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles partial-completion)))))

;; i like some help while searching
(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

;; musthave
(use-package consult
  :ensure t
  :init
  (setq consult-preview-allowed-hooks '(global-font-lock-mode-check-buffers save-place-find-file-hook display-line-numbers-mode))

  :preface
  (defun my/consult-outline ()
    "Execute `consult-org-heading' if `org-mode' is active, else `consult-outline'."
    (interactive)
    (if (derived-mode-p 'org-mode)
        (consult-org-heading)
      (consult-outline)))

  (defun my/consult-ripgrep (prefix)
    "Execute different commands based on the universal prefix argument PREFIX."
    (interactive "P")
    (if (equal prefix '(4))  ; Check if the prefix is C-u (universal prefix argument)
        (progn
          ;; Command to execute when C-u is used
          (consult-git-grep))
      (progn
        ;; Command to execute when no prefix or other prefix is used
        (consult-ripgrep default-directory)
        )))

  :bind
  ("<escape> p" . consult-yank-from-kill-ring)
  ("C-x C-b" . consult-buffer)
  ("<escape> c l" . consult-line)
  ("<escape> R" . consult-recent-file)
  ("<escape> g" . my/consult-ripgrep)
  ("<escape> G" . magit)
  ("<escape> c x" . consult-complex-command)
  ("<escape> t t" . consult-todo)
  ("<escape> t k" . consult-keep-lines)
  ("<escape> t f" . consult-focus-lines)
  ("<escape> o o" . my/consult-outline)
  ("<escape> b" . consult-bookmark)
  ("<escape> r r" . consult-register)
  ("<escape> r a" . consult-register-store)
  ("<escape> r i" . consult-register-load)
  ("<escape> i" . consult-imenu)
  ("<escape> I" . consult-imenu-multi)
  ("<escape> x f" . ido-find-file)
  ("<escape> x b" . consult-buffer)
  ( "M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)

  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-dir
  :ensure t
  :after consult)

                                        ;(use-package consult-todo
                                        ;  :ensure (:host github :repo "liuyinz/consult-todo")
                                        ;  :after consult
                                        ;  :config
                                        ;  (setq consult-todo-narrow
                                        ;        '((?t . "TODO")
                                        ;          (?f . "FIX")
                                        ;          (?b . "BUG")
                                        ;          (?h . "ASK"))))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq cape-dict-file '("/usr/share/dict/italian" "/usr/share/dict/british-english")))

(use-package yasnippet-capf
  :ensure t
  :after cape
  :vc (:url "https://github.com/elken/yasnippet-capf" :branch "master")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package corfu
  :defer 2
  :ensure t
  :bind
  :config
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-auto-prefix 3)
  (setq corfu-auto-delay 0)
  (setq corfu-bar-width 0)
  (setq corfu-right-margin-width 0)
  (setq corfu-left-margin-width 0)
  (setq corfu-min-width 10)
  (setq corfu-max-width 80)
  (setq corfu-separator nil)          ;; Orderless field separator
  (setq corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (setq corfu-preview-current nil)    ;; Disable current candidate preview
  (setq corfu-preselect 'first)      ;; Preselect the prompt

  ;; Corfu for org mode setup
  (add-hook 'org-mode-hook
            (lambda ()
              ;; (setq-local corfu-auto-prefix 1)
              (setq-local completion-at-point-functions
                          '(
                            cape-file
                            yasnippet-capf
                            ;; cape-elisp-block
                            ;; cape-dict
                            ;; citar-capf
                            ;; cape-dabbrev
                            ))
              (setq-local completion-at-point-functions
                          (list
                           ;; (cape-capf-prefix-length #'cape-dict 3)
                           (cape-capf-prefix-length #'cape-file 1)
                           (cape-capf-prefix-length #'yasnippet-capf 2)
                           (cape-capf-prefix-length #'cape-dabbrev 5)
                           ))))

  ;; Eglot and corfu setup

  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-file
                       ))))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


  ;; Setup for emacs lisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local corfu-auto-prefix 3)
              (setq-local completion-at-point-functions
                          '(cape-file
                            yasnippet-capf
                            cape-keyword
                            cape-elisp-symbol
                            cape-dabbrev))))

  ;; Enable corfu in the minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-auto-prefix 3
                  corfu-preselect 'valid
                  corfu-min-width 10
                  corfu-max-width 80
                  )
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :init
  (global-corfu-mode)
  )

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion)

;;; completion.el ends here
