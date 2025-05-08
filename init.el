;; init.el -*- lexical-binding: t; -*-
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(setq elpaca-core-date '(20241219))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

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
(load-module "org-config")
(load-module "org-beamer")
(load-module "org-navigation")
(load-module "denote")
(load-module "server")

;;; init.el ends here
(put 'eshell 'disabled nil)
