;;; core.el -*- lexical-binding: t; -*-;;;

;;; Configuration for the "core" behavior of GNU Emacs. Generally, anything
;;; which does not involve a third-party package.

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Having Emacs open to `*scratch*' feels as though it's inviting me to punch
;; out some Lisp forms and evaluate them. I like to pretend that my computers
;; are Lisp machines.
(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      initial-buffer-choice nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-default-init t
      initial-major-mode 'fundamental-mode)

;; favor vertical splits over horizontal ones?
(setq split-width-threshold 140
      split-height-threshold 80)

;; Show me what I type, immediately.
(setq echo-keystrokes 0.01)

;; Disable useless messsages
(setq inhibit-message nil)

;; Make easy ovveride settings
(setq site-run-file nil)

;; Not a good idea
(setq max-lisp-eval-depth 10000)

;; when quiting emacs, just kill processes
(setq confirm-kill-processes nil)

;; ask if local variables are safe once.
(setq enable-local-variables t)

;; Better resize
(setq frame-resize-pixelwise t)

;; Save backup files to one directory instead of making a mess of the filesystem.
(setq backup-directory-alist `(("." . "~/.cache/emacs")))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")

;; `auto-save', in addition to the actual files it saves, maintains another file
;; listing the files it's currently taking care of.
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-"
      backup-by-copying t
	  delete-old-versions t
      )

;; Disable the creation of locking symlinks in the current directory. This is a
;; very opinionated choice, and probably isn't a good idea for most.
(setq create-lockfiles nil)

;; Disable creation of backup files
(setq make-backup-files nil)

;; I don't use 'custom.el' to set variables, but a few of the packages I use do.
;; This snippet ensures that a massacre is not made of my init.el.

(setq custom-file "/dev/null")

;; Set default directory to home and abbreviate it
(setq default-directory "~/")

;; I care about having my history in minibuffers
(use-package savehist
  :hook after-init
  :init
  (savehist-mode))

;; Use a consistent confirmation dialog of "y or n".
;; Use RET to answer yes
(setq use-short-answers t)
(define-key y-or-n-p-map (kbd "<return>") 'y-or-n-p-insert-y)

;; Automatically revert buffers and dired listings when something on disk
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :init
  (add-hook 'after-init-hook 'global-auto-revert-mode))

;; Always prefer newer version of a file
(setq load-prefer-newer t)

;; "Command attempted to use minibuffer while in minibuffer" gets old fast.
(setq enable-recursive-minibuffers t)

;; Disable mouse on `y-or-n-p'
(setq use-dialog-box nil)

;; Select help windows when I pop them so that I can kill them with <q>.
(setq help-window-select t)

;; Most *NIX tools work best when files are terminated with a newline.
(setq require-final-newline t)

;; Sentences should be separated by a single space. Treat two sentences as such
;; when filling.
(setq sentence-end-double-space nil)

;; I use the tab key, but I generally prefer space characters to tabstops and
;; like a four-character width for indentation.
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq tab-always-indent t)

;; Modern conventions state that 80 characters is the standard width.
(setq fill-column 80)

;; Enable useful visual queues.
(column-number-mode t)

;; Unbind <C-z> and <C-x C-z> so that I'm not accidentally calling =suspend-frame=.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Unbind downcase region
(global-unset-key (kbd "C-x C-l"))

;; C-g is killing me
(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)

;; Don't do jerky jumps when the cursor reaches the end of the window. Instead,
;; just scroll by one line.

;; Better then global centered cursor mode
(setq scroll-conservatively 101
      indicate-empty-lines nil
      scroll-preserve-screen-position t
      scroll-margin 10
      maximum-scroll-margin 0.5
      next-screen-context-lines 5
      auto-window-vscroll nil
      )

;; always follow symlinks when opening files
(setq vc-follow-symlinks t)

;; overwrite text when selected, like we expect.
(setq delete-selection-mode t)

;;Useless
(setq ring-bell-function 'ignore)

;; Disable bidirectional text rendering for a modest performance boost. Just
;; need to remember to turn it on when displaying a right-to-left language!
(setq-default bidi-display-reordering 'left-to-right)

(use-package emacs
  :hook
  (after-make-frame-functions . my/new-frame-settings)

  :config
  ;; Autobyte recompile init.elc when exiting emacs
  (add-hook 'kill-emacs-hook (lambda () (byte-recompile-file user-init-file)))

  (setq server-client-instructions nil)

  (mapc
   (lambda (command)
     (put command 'disabled nil))
   '(list-timers dire-find-alternate-file narrow-to-region narrow-to-page upcase-region downcase-region))

  ;; And disable these
  (mapc
   (lambda (command)
     (put command 'disabled t))
   '(eshell project-eshell overwrite-mode iconify-frame diary))

  (defun my/new-frame-settings (frame)
    (if (daemonp)
        (setq evil-echo-state nil))))

;; I like standard sentence ending
(setq sentence-end-double-space nil)

;; Make me able to continue selection without keep the finger on the shift key
(setopt shift-select-mode 'permanent)

;; Fix clipboard in TTY
(use-package xclip
  :init
  (add-hook 'after-init-hook 'xclip-mode)
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-method (quote wl-copy)))

;; Save some key presses
(use-package repeat
  :config
  (defun my/repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode)))

  (my/repeat-mode)

  ;; Disable the built-in repeat-mode hinting
  (setq repeat-echo-function #'ignore)

  ;; Custom repeat-maps
  (defvar-keymap my/undo-repeat-map
    :repeat (:enter (undo))
    "u" #'undo-fu-only-undo
    "r" #'undo-fu-only-redo)

  (defvar-keymap org-heading-repeat-map
    :repeat (:enter (org-next-visible-heading org-previous-visible-heading))
    "n" #'org-next-visible-heading
    "p" #'org-previous-visible-heading)

  (defvar-keymap goto-last-change-repeat-map
    :repeat (:enter (goto-last-change goto-last-change-reverse))
    ";" #'goto-last-change
    ":" #'goto-last-change-reverse)

  ;; Use which-key to show help
  (use-package which-key
    :after which-key
    :config
    (advice-add 'repeat-post-hook :after
                (defun my/which-key-repeat ()
                  (when-let ((cmd (or this-command real-this-command))
                             (keymap (repeat--command-property 'repeat-map)))
                    (run-at-time
                     which-key-idle-delay nil
                     (lambda ()
                       (which-key--create-buffer-and-show
                        nil (symbol-value keymap)))))))

    (defun my/which-key-repeat-mode-dispatch ()
      (interactive)
      (setq this-command last-command)
      (when-let (keymap (repeat--command-property 'repeat-map))
        (which-key--create-buffer-and-show
         nil (symbol-value keymap))))

    (defun my/which-key-repeat-mode-binding ()
      (when repeat-mode
        (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
                    (keymap (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
          (set-transient-map
           (make-composed-keymap
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-h") #'my/which-key-repeat-mode-dispatch)
              map)
            keymap)))))

    (advice-add 'repeat-post-hook :after #'my/which-key-repeat-mode-binding)))

;; Rebind M-m to C-a
(define-key global-map (kbd "C-a ") 'back-to-indentation)
(define-key global-map (kbd "<escape> J") 'join-line)

;; Stop skipping words
(global-subword-mode)

;; Ignore useless messages
(defun filter-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-line, end-of-line, beginning-of-buffer, end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                mark-inactive
                                beginning-of-line
                                end-of-line
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'filter-command-error-function)

;; Highlight urls and make them clickable.
(global-goto-address-mode 1)

;; Overwrite selection on pasting
(delete-selection-mode 1)

(use-package goto-chg
  :ensure t
  :bind (("<escape> ;" . goto-last-change)
         ("<escape> :" . goto-last-change-reverse)))

;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (if (vc-registered filename)
            (vc-rename-file name new-name)
          (rename-file name new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(define-key global-map (kbd "<escape>u") 'rename-file-and-buffer)

;;;###autoload
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

(define-key global-map (kbd "<escape>m") 'move-buffer-file)

(define-key global-map (kbd "<escape>k") 'kill-buffer-and-window)

(use-package compile
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

(provide 'core)
