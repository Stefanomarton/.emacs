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

;; Line spacing (buffer-local variable: setq-default to apply everywhere)
(setq-default line-spacing 0.12)

;; I like icons
(use-package nerd-icons
  :ensure t
  :defer t
  )

;; Customize the divider beetween windows
(use-package frame
  :ensure nil
  :config

  ;; frame borders are in default-frame-alist (early-init.el), not reset on every buffer
  (setq window-divider-default-places nil))

(use-package base16-theme
  :ensure t
  :custom
  (base16-theme-distinct-fringe-background nil)
  (base16-theme-256-color-source 'colors)
  :config
  ;; tweaks on top of any theme, set again each time one is enabled
  (defun my/theme-faces (&rest _)
    (set-face-attribute 'font-lock-comment-face nil :slant 'oblique)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'oblique)
    (set-face-attribute 'mode-line-active nil :height 1.2)
    (set-face-attribute 'header-line nil :height 1.2))
  (add-hook 'enable-theme-functions #'my/theme-faces)

  ;; the desktop's theme (~/.local/bin/theme writes desk-theme.el there and then `name'):
  ;; followed live, black-metal-nile without it
  (defvar my/desk-theme-dir (expand-file-name "~/.local/state/theme/"))
  (add-to-list 'custom-theme-load-path my/desk-theme-dir)
  (defun my/desk-theme-load ()
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (if (file-exists-p (expand-file-name "desk-theme.el" my/desk-theme-dir))
                    'desk 'base16-black-metal-nile)
                t))
  (my/desk-theme-load)
  (when (file-directory-p my/desk-theme-dir)
    (file-notify-add-watch
     my/desk-theme-dir '(change)
     (lambda (e)   ; (descriptor action file [file1]): `name' written or renamed into place
       (when (member "name" (mapcar #'file-name-nondirectory (seq-filter #'stringp (cddr e))))
         (my/desk-theme-load))))))

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
  :config   ; mixed-pitch-mode handles text buffers
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
  (LaTeX-mode . breadcrumb-local-mode)
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

(use-package winpulse
  :vc (:url "https://github.com/xenodium/winpulse"
            :rev :newest)
  :config
  (winpulse-mode +1))

(provide 'appearance)

