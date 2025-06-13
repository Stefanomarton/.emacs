;;; appearance.el -*- lexical-binding: t; -*-
;;; Configuration for making Emacs look pretty.


;; I keep losing the curson
(blink-cursor-mode 0)

;; Enable `prettify-symbols' globally.
(global-prettify-symbols-mode t)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Display line number relative and absolute
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start 15)
(setq display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)

;; Highlight the current line
(add-hook 'prog-mode-hook 'hl-line-mode)
(setq hl-line-sticky-flag nil)          ; Avoid seeing the bar in all windows

;; Line spacing
(setq line-spacing 0.12)

;; I like icons
(use-package nerd-icons
  :ensure t
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
  (add-to-list 'default-frame-alist '(alpha-background . 90))

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
  :config
  (setq base16-theme-distinct-fringe-background nil)
  ;; (add-to-list 'custom-theme-load-path "~/.config/emacs")
  (setq base16-theme-256-color-source 'colors)
  ;; (load-theme 'base16-mountain t)
  (load-theme 'base16-everforest-dark-hard t)
  )

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
		              :height 150
		              :weight 'normal
		              :width 'normal)
  (set-face-attribute 'variable-pitch nil
    	              :family "JuliaMono"
    	              :height 150
    	              :weight 'normal
    	              :width 'normal)
  (set-face-attribute 'fixed-pitch nil
		              :family "JuliaMono"
		              :height 150
		              :weight 'normal
		              :width 'normal)
  )

(use-package breadcrumb
  :ensure t
  :hook
  (typst-ts-mode . breadcrumb-local-mode)
  (Latex-Mode . breadcrumb-local-mode)
  (prog-mode . breadcrumb-local-mode)
  (conf-unix-mode . breadcrumb-local-mode))

(use-package focus
  :ensure t
  :commands (focus-mode))

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'appearance)
