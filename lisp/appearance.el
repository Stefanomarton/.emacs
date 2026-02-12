;;; appearance.el -*- lexical-binding: t; -*-

;; I keep losing the curson
(blink-cursor-mode 0)

;; Enable `prettify-symbols' globally.
(global-prettify-symbols-mode t)
(setq prettify-symbols-unprettify-at-point 'right-edge)


(use-package display-line-numbers
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  (markdown-mode . display-line-numbers-mode)
  (latex-mode . display-line-numbers-mode)
  :config
  ;; Display line number relative and absolute
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start 15)
  (setq display-line-numbers-type 'relative)
  )


;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

;; Line spacing
(setq line-spacing 0.12)

;; I like icons
(use-package nerd-icons
  :ensure t
  :defer t
  )

;; Customize the divider beetween windows
(use-package frame
  :ensure nil
  :config

  (defun setup-margin ()
    (set-frame-parameter nil 'internal-border-width 15)
    (set-frame-parameter nil 'external-border-width 5)
    (setq-local line-spacing 0.12)
    ;; (set-window-margins nil 1 1)
    )
  ;; (add-to-list 'default-frame-alist '(alpha-background . 90))

  ;; (add-hook 'window-configuration-change-hook #'setup-margin)
  (add-hook 'text-mode-hook #'setup-margin)
  (add-hook 'prog-mode-hook #'setup-margin)

  (setq window-divider-default-places nil))

;; (use-package ewal
;;   :ensure (:host github :repo "cyruseuros/ewal")
;;   :init
;;   (setq ewal-use-built-in-always-p nil
;;         ewal-use-built-in-on-failure-p t
;;         ewal-built-in-palette "sexy-material")
;;   :config
;;   ;; Suppose all custom themes are safe
;;   (setq ewal-shade-percent-difference 10)
;;   (setq custom-safe-themes t)
;;   (add-to-list 'custom-theme-load-path "~/.config/emacs")
;;   (load-theme 'pywal))

;; (use-package mindre-theme
;;   :ensure t
;;   :config
;;   (setq mindre-use-more-bold t
;;         mindre-use-more-fading t
;;         mindre-use-faded-lisp-parens t
;;         mindre-faded-lisp-parens-modes '(emacs-lisp-mode lisp-mode scheme-mode racket-mode))
;;   (load-theme 'mindre t))

(use-package base16-theme
  :ensure t
  :custom
  (base16-theme-distinct-fringe-background nil)
  (base16-theme-256-color-source 'colors)
  :config
  (load-theme 'base16-black-metal-nile t)

  (set-face-attribute 'font-lock-comment-face nil :slant 'oblique)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'oblique)
  (set-face-attribute 'mode-line-active nil :height 1.2)
  (set-face-attribute 'header-line nil :height 1.2)
  )

;; (use-package mindre-theme
;;   :ensure t
;;   :config
;;   (load-theme 'mindre t))

;; Cool aspect
(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'line-number)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-link)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'corfu-default)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'corfu-current)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-cite)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'error)

  (setq mixed-pitch-set-height nil))

(use-package emacs
  :ensure nil
  :hook
  (text-mode . variable-pitch-mode)
  :config
  (set-face-attribute 'default nil
		              :family "JuliaMono"
		              :height 190
		              :weight 'normal
		              :width 'normal)
  (set-face-attribute 'variable-pitch nil
    	              :family "JuliaMono"
    	              :height 190
    	              :weight 'normal
    	              :width 'normal)
  (set-face-attribute 'fixed-pitch nil
		              :family "JuliaMono"
		              :height 190
		              :weight 'normal
		              :width 'normal)
  )

(use-package breadcrumb
  :defer t
  :ensure t
  :hook
  (typst-ts-mode . breadcrumb-local-mode)
  (Latex-Mode . breadcrumb-local-mode)
  (prog-mode . breadcrumb-local-mode)
  (conf-unix-mode . breadcrumb-local-mode))

(use-package focus
  :defer t
  :ensure t
  :commands (focus-mode))

(use-package goggles
  :defer t
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'appearance)
