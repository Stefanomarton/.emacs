;;; programming.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package treesit
  :ensure nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (make . ("https://github.com/alemuller/tree-sitter-make"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;;Tree-sitter enabled major modes are
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (sh-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (conf-toml-mode . toml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars))

(use-package macrursors
  :ensure (:host github
                   :repo "corytertel/macrursors")
  :bind
  (:map global-map
        ("C-c SPC" . macrursors-select)
        ("C->" . macrursors-mark-next-instance-of)
        ("C-<" . macrursors-mark-previous-instance-of)
        ("C-;" . macrursors-mark-map))
  (:map macrursors-mark-map
        ("C-;" . macrursors-mark-all-lines-or-instances)
        (";" . rursors-mark-all-lines-or-instances)
        ("l" . rursors-mark-all-lists)
        ("s" . macrursors-mark-all-symbols)
        ("e" . macrursors-mark-all-sexps)
        ("f" . macrursors-mark-all-defuns)
        ("n" . macrursors-mark-all-numbers)
        ("." . macrursors-mark-all-sentences)
        ("r" . macrursors-mark-all-lines))
  :init
  (define-prefix-command 'macrursors-mark-map)
  :config
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode)))

;; Lua setup
(use-package lua-mode
  :ensure t
  :mode ("\\.lua?\\'" . lua-mode)
  )

(use-package lisp-mode
  :ensure nil
  :hook
  (after-save-hook . auto-byte-recompile)
  :config
  (defun auto-byte-recompile ()
    "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))

  (add-hook 'after-save-hook 'auto-byte-recompile)

  (add-to-list 'display-buffer-alist
               '("\\*Compile-Log\\*"
                 (display-buffer-in-direction)
                 (direction . down)
                 (window-width . 0.1)
                 (window-height . 0.2)))

  (use-package lisp-extra-font-lock
    :ensure t
    :config
    (lisp-extra-font-lock-mode))

  (use-package eros
    :ensure t
    :config
    (eros-mode))

  (use-package lispy
    :ensure t
    :config
    (lispy-mode)
    ))



;; Highlight kmonad files
(use-package kbd-mode
  :ensure (:host github
                   :repo "kmonad/kbd-mode")
  :mode ("\\.kbd\\'" . kbd-mode))


(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  )

(use-package yuck-mode
  :ensure t
  :mode "\\.yuck\\'"
  )

(use-package gnuplot-mode
  :ensure t
  :mode "\\.plt\\'"
  )

(provide 'programming)

;;; programming.el ends here
