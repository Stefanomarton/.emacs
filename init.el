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
  ;; Massively increase the chunk size for reading data from external
  ;; processes. Essential for preventing lag when using LSP servers
  ;; (like Typst/Eglot), which send massive JSON payloads. 1MB in
  ;; bytes, default is 4096 bytes.
  (setq read-process-output-max 1048576))

;; Uncommented this sometimes for debugging
(setq use-package-verbose t)
(setq debug-on-error t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package gcmh
    :ensure t
    :config
    ;; GCMH si attiva all'avvio e gestisce lui la memoria dinamicamente.
    ;; Di default abbassa il threshold a 16MB quando Emacs è a riposo.
    (gcmh-mode 1))

(defvar notes-folder "~/.marton-drive/")

(let ((default-directory "~/.config/emacs/lisp/"))
  ;; This adds the main folder to the load-path
  (add-to-list 'load-path default-directory)
  ;; This automatically finds all subfolders and adds them too
  (normal-top-level-add-subdirs-to-load-path))

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
(require 'markdown)
(require 'config-latex)

(require 'denote-config)
(require 'typst-config)
