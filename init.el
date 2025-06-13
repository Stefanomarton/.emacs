;; init.el -*- lexical-binding: t; -*-
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; Uncommented this sometimes for debugging
;; (setq use-package-verbose t)
;; (setq debug-on-message t)

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

;; ;; Everything is what I classify as the "early-load" is in the early-init.el file.
;; ;; The rest of my configuration is broken into "modules", which I include into
;; ;; the init.el at macro expansion time.

;; ;; Directory containing configuration 'modules'.
(defvar module-directory "~/.config/emacs/lisp")

;; ;; Multiples macros to properly load submodules

(defmacro insert-code-from-file (path)
  "Read the forms in the file at PATH into a progn."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let (forms (eof nil))
      (while (not eof)
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file (setq eof t))))
      `(progn ,@(reverse forms)))))

(defmacro load-module (name &optional condition)
  "Locate the module NAME and insert its contents as a progn."
  (let* ((file-name (concat name ".el"))
         (path (expand-file-name file-name module-directory)))
    (if condition
        `(expand-when ,condition (insert-code-from-file ,path))
      `(insert-code-from-file ,path))))

(defmacro expand-when (conditional &rest form)
  "Expand if and only if `CONDITIONAL' is truthy at compile-time."
  (if (eval conditional)
      `(progn ,@form)
    '(progn)))

;; Enable native compilation for all files
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; ;; As stated https://github.com/jwiegley/use-package?tab=readme-ov-file#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar drive-folder "~/.marton-drive/")
(defvar notes-folder (concat drive-folder "notes"))

(load-module "core")
;; ;; (load-module "evil")
;; ;; (load-module "keybindings")

(load-module "file-management")
(load-module "completion")
(load-module "appearance")
(load-module "modeline")
(load-module "headerline")
(load-module "base-packages")
(load-module "editor-config")
(load-module "tools")
(load-module "project-management")
(load-module "programming")
(load-module "lsp")
(load-module "document-production")

(load-module "org/org-config")
(load-module "org/org-beamer")
(load-module "org/org-navigation")
(load-module "org/org-citations")
(load-module "org/org-export")
(load-module "org/org-appearance")
(load-module "org/org-extensions")
(load-module "org/org-attachments")

(load-module "denote")
(load-module "typst")
(load-module "server")

;;; init.el ends here
(put 'eshell 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aas altcaps apheleia auto-yasnippet avy base16-theme breadcrumb bug-hunter
         cape casual consult consult-dir consult-projectile corfu csv-mode
         dirvish ebuku ellama embark embark-consult embrace eros esup focus gcmh
         git-gutter git-gutter-fringe gnuplot-mode goggles google-this goto-chg
         gptel helpful hl-todo hydra lisp-extra-font-lock lua-mode magit
         magit-delta marginalia markdown-mode mermaid-mode mermaid-ts-mode
         mixed-pitch nerd-icons-corfu orderless org-anki org-download org-modern
         org-transclusion pdf-tools pkg-info plantuml-mode projectile pyvenv
         rainbow-delimiters rainbow-mode scratch selected sudo-edit tab-jump-out
         undo-fu undo-fu-session undo-tree vertico vertico-posframe
         visual-regexp wgrep yaml-pro yasnippet yuck-mode zoxide)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

