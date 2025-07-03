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
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
               (nix . ("https://github.com/nix-community/tree-sitter-nix"))
               (hyprlang . ("https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))
               (mermaid . ("https://github.com/monaqa/tree-sitter-mermaid"))
               (typst . ("https://github.com/uben0/tree-sitter-typst"))
               ))
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
  (setq treesit-font-lock-level 4)
  (mp-setup-install-grammars))

;; (use-package macrursors
;;   :ensure (:host github
;;                  :repo "corytertel/macrursors")
;;   :bind
;;   (:map global-map
;;         ("C-c SPC" . macrursors-select)
;;         ("C->" . macrursors-mark-next-instance-of)
;;         ("C-<" . macrursors-mark-previous-instance-of)
;;         ("C-;" . macrursors-mark-map))
;;   (:map macrursors-mark-map
;;         ("C-;" . macrursors-mark-all-lines-or-instances)
;;         (";" . rursors-mark-all-lines-or-instances)
;;         ("l" . rursors-mark-all-lists)
;;         ("s" . macrursors-mark-all-symbols)
;;         ("e" . macrursors-mark-all-sexps)
;;         ("f" . macrursors-mark-all-defuns)
;;         ("n" . macrursors-mark-all-numbers)
;;         ("." . macrursors-mark-all-sentences)
;;         ("r" . macrursors-mark-all-lines))
;;   :init
;;   (define-prefix-command 'macrursors-mark-map)
;;   :config
;;   (dolist (mode '(corfu-mode goggles-mode beacon-mode))
;;     (add-hook 'macrursors-pre-finish-hook mode)
;;     (add-hook 'macrursors-post-finish-hook mode)))

;; Lua setup
(use-package lua-mode
  :ensure t
  :mode ("\\.lua?\\'" . lua-mode)
  )

(use-package lisp-mode
  :ensure nil
  :config
  

  (use-package lisp-extra-font-lock
    :ensure t
    :config
    (lisp-extra-font-lock-mode))

  (use-package eros
    :ensure t
    :config
    (eros-mode)))


;; Highlight kmonad files
(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest)
  :mode ("\\.kbd\\'" . kbd-mode))

(use-package yuck-mode
  :ensure t
  :mode "\\.yuck\\'"
  )

(use-package gnuplot-mode
  :ensure t
  :mode "\\.plt\\'"
  )

(use-package yaml-pro
  :ensure t
  :mode "\\.yml\\'"
  :bind
  (:map yaml-pro-ts-mode-map
        ("C-M-n" . yaml-pro-ts-next-subtree)
        ("C-M-p" . yaml-pro-ts-prev-subtree)
        ("C-M-u" . yaml-pro-ts-up-level)
        ("C-M-d" . yaml-pro-ts-down-level)
        ("C-M-k" . yaml-pro-ts-kill-subtree)
        ("C-M-<backspace>" . yaml-pro-ts-kill-subtree)
        ("C-M-a" . yaml-pro-ts-first-sibling)
        ("C-M-e" . yaml-pro-ts-last-sibling))

  :config
  ;; Enable auto format for yaml-pro-mode-hook
  (add-hook 'yaml-pro-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'yaml-pro-format nil 'local)))

  ;; Define repeat-keymap for yaml-pro-mode
  (defvar-keymap my/yaml-pro/tree-repeat-map
    :repeat t
    "n" #'yaml-pro-ts-next-subtree
    "p" #'yaml-pro-ts-prev-subtree
    "u" #'yaml-pro-ts-up-level
    "d" #'yaml-pro-ts-down-level
    "m" #'yaml-pro-ts-mark-subtree
    "k" #'yaml-pro-ts-kill-subtree
    "a" #'yaml-pro-ts-first-sibling
    "e" #'yaml-pro-ts-last-sibling
    "SPC" #'my/yaml-pro/set-mark)

  (defun my/yaml-pro/set-mark ()
    (interactive)
    (my/region/set-mark 'my/yaml-pro/set-mark))

  (defun my/region/set-mark (command-name)
    (if (eq last-command command-name)
        (if (region-active-p)
            (progn
              (deactivate-mark)
              (message "Mark deactivated"))
          (activate-mark)
          (message "Mark activated"))
      (set-mark-command nil)))
  )

(use-package toml-ts-mode
  :mode "\\.toml'"
  :ensure nil
  :hook
  (toml-ts-mode . format-all-mode))

(use-package pyvenv
  :ensure t)

(use-package hyprlang-ts-mode
  :vc (:url "https://github.com/Nathan-Melaku/hyprlang-ts-mode")
  :custom
  (hyprlang-ts-mode-indent-offset 4))

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(provide 'programming)

;;; programming.el ends here
