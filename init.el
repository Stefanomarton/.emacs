;; init.el -*- lexical-binding: t; -*-
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; Straight is my package manager of choice
;; Avoid check for modification at startup, save up to ~0.2 s.
(setq straight-check-for-modifications 'live-with-find)

;; Boostrapping function
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")

;; Use shallow cloning as I don't need all the package branches
(setq straight-vc-git-default-clone-depth '(1 single-branch))

;; Install use-package and ensure listed packages are installed
(setq straight-use-package-by-default t)

;; Uncommented this sometimes for debugging
;; (setq use-package-verbose t)
;; (setq debug-on-message t)

;; But we do want to reset the garbage collector settings eventually. When we
;; do, we'll use the GCMH [1] package to schedule the garbage collector to run
;; during idle time, rather than the haphazard "whenever some threshold is
;; reached".
;; [1]: https://gitlab.com/koral/gcmh/

(use-package gcmh :defer t)

(add-hook 'emacs-startup-hook
	      (lambda ()
	        (setq gc-cons-threshold 33554432) ; 16mb
	        (setq gc-cons-percentage 0.1)
	        (require 'gcmh)
	        (gcmh-mode 1)))

;; Everything is what I classify as the "early-load" is in the early-init.el file.
;; The rest of my configuration is broken into "modules", which I include into
;; the init.el at macro expansion time.

;; Directory containing configuration 'modules'.
(defvar module-directory "~/.config/emacs/lisp")

;; Multiples macros to properly load submodules

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

;; As stated https://github.com/jwiegley/use-package?tab=readme-ov-file#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(load-module "core")
;; (load-module "evil")
;; (load-module "keybindings")
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
(load-module "orgconfig")
(load-module "server")

;; Show startup time in message
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.2fs) [after-init]"
                        ,load-file-name elapsed))) t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here
