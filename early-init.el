(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-buffer-choice nil
      inhibit-x-resources t
      inhibit-default-init t
      initial-scratch-message nil
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      initial-major-mode 'fundamental-mode)

;; More than half of the packages I use regularly produce compile warnings. It
;; gets to be quite annoying when the `*Warnings*' window pops up while I'm
;; trying to do work, so we will disable native-comp reporting.
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation t)
(setq native-compile-prune-cache t)

(defvar me/gc-cons-threshold 100000000)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)

(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

;; Fill whatever space the window manager has given us.
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 800))
                  (height . (text-pixels . 900))
                  (horizontal-scroll-bars . nil)
                  (vertical-scroll-bars . nil)
                  (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                  (tool-bar-lines . 0)))) ; alternative to disabling `tool-bar-mode'

(setq inhibit-compacting-font-caches t)

;; Enable packages at startup
(setq package-enable-at-startup t)

;; fix delay in pgtk build
(setq-default pgtk-wait-for-event-timeout 0)
