;; init.el -*- lexical-binding: t; -*-

;; Startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; Uncommented this sometimes for debugging
;; (setq use-package-verbose t)
(setq debug-on-message t)

;; But we do want to reset the garbage collector settings eventually. When we
;; do, we'll use the GCMH [1] package to schedule the garbage collector to run
;; during idle time, rather than the haphazard "whenever some threshold is
;; reached".
;; [1]: https://gitlab.com/koral/gcmh/

(use-package gcmh :ensure t :defer t)

(add-hook 'emacs-startup-hook
	      (lambda ()
	        (setq gc-cons-threshold 33554432) ; 16mb
	        (setq gc-cons-percentage 0.1)
	        (require 'gcmh)
	        (gcmh-mode 1)))


;; ;; As stated https://github.com/jwiegley/use-package?tab=readme-ov-file#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar drive-folder "~/.marton-drive/")
(defvar notes-folder (concat drive-folder "notes"))

(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'load-path "~/.config/emacs/lisp/org")

(require 'core)
(require 'file-management)
(require 'completion)
(require 'appearance)
(require 'modeline)
(require 'headerline)
(require 'base-packages)
(require 'surround)
(require 'editor-config)
(require 'tools)
(require 'project-management)
(require 'programming)
(require 'lsp)
(require 'document-production)

(require 'org-config)
(require 'org-beamer)
(require 'org-navigation)
(require 'org-citations)
(require 'org-export)
(require 'org-appearance)
(require 'org-extensions)
(require 'org-attachments)

(require 'denote-config)
(require 'typst)
(require 'server-config)
