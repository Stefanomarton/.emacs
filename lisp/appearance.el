;;; appearance.el -*- lexical-binding: t; -*-
;;; Configuration for making Emacs look pretty.

(use-package emacs
  :config
  ;; Prefere visual line
  ;; (global-visual-line-mode t)

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
  (setq line-spacing 0.12))

;; I like icons
(use-package nerd-icons)

;; Customize the divider beetween windows
(use-package frame
  :straight (:type built-in)
  :config

  (defun setup-margin ()
    (set-frame-parameter nil 'internal-border-width 15)
    (set-frame-parameter nil 'external-border-width 5)
    (setq-local line-spacing 0.12)
    ;; (set-window-margins nil 1 1)
    )

  ;; (add-hook 'window-configuration-change-hook #'setup-margin)
  (add-hook 'text-mode-hook #'setup-margin)
  (add-hook 'prog-mode-hook #'setup-margin)

  (setq window-divider-default-places nil))

(use-package ewal
  :init
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material")
  :config
  ;; Suppose all custom themes are safe
  (setq ewal-shade-percent-difference 10)
  (setq custom-safe-themes t)
  (add-to-list 'custom-theme-load-path "~/.config/emacs")
  (load-theme 'pywal))


;; Cool aspect
(use-package mixed-pitch
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
  :hook
  (text-mode . variable-pitch-mode)
  :config
  (set-face-attribute 'default nil
		              :family "JuliaMono"
		              :height 135
		              :weight 'normal
		              :width 'normal)
  (set-face-attribute 'variable-pitch nil
    	              :family "JuliaMono"
    	              :height 135
    	              :weight 'normal
    	              :width 'normal)
  (set-face-attribute 'fixed-pitch nil
		              :family "JuliaMono"
		              :height 135
		              :weight 'normal
		              :width 'normal)
  )

(use-package breadcrumb
  :hook
  (prog-mode . breadcrumb-local-mode))

(use-package focus
  :commands (focus-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'appearance)
