;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
            :branch "main")
  :hook
  (typst-ts-mode . display-line-numbers-mode)
  :bind (:map typst-ts-mode-map

              ("C-c <tab>" . outline-cycle-buffer)
              ("<backtab>" . outline-cycle)
              ("C-c n" . outline-next-heading)
              ("C-c p" . outline-previous-heading)
              ("C-c o" . consult-outline)

              ("C-c t" . typst-snippets-table)

              ("C-c e" . typst-consult-equations-jump)
              ("C-c E" . typst-consult-equations-insert)
              ("C-c l" . typst-consult-insert-ref)
              ("C-c L" . typst-consult-labels-jump)

              ("C-c m" . typst-select-math-after)
              ("C-c M" . typst-select-math-before)

              ("C-c c" . citar-insert-citation)

              ("C-c x" . sm/typst-extract-region-to-file)
              )


  :hook
  (typst-ts-mode . outline-indent-mode)

  ;; Dynamically set roots for BOTH watch and compile when launching typst-ts-mode.
  ;; Forces a synchronous shell check for direnv before falling back to denote.
  ;; On tm-laptop the root is always the home directory (no direnv check).
  (typst-ts-mode . (lambda ()
                     (let* ((tm-laptop (string= (system-name) "tm-laptop"))
                            (direnv-root
                             (unless tm-laptop
                              (with-temp-buffer
                               ;; 1. Run direnv allow
                               ;; 2. Wait for it, then execute printenv
                               ;; 3. Return 0 (success) only if the variable exists
                               (when (eq 0 (call-process-shell-command 
                                            "direnv allow . && direnv exec . printenv TYPST_PROJECT_ROOT" 
                                            nil t nil))
                                 (string-trim (buffer-string))))))

                            ;; 4. Check if we got a valid path, otherwise fallback
                            (active-root (cond (tm-laptop (expand-file-name "~"))
                                               ((and direnv-root (not (string-empty-p direnv-root)))
                                                direnv-root)
                                               (t (expand-file-name denote-directory)))))
                       
                       ;; Watch Options: List format for start-process (no shell)
                       (setq-local typst-ts-watch-options 
                                   (list "--root" active-root))
                       
                       ;; Compile Options: String format for shell process
                       (setq-local typst-ts-compile-options
                                   (format "--root=%s --pdf-standard=a-2b" (shell-quote-argument active-root)))

                       ;; Same root for the live preview (typst-preview)
                       (setq-local typst-preview-default-dir active-root))))

  :config
  ;; ;; (setq typst-ts-output-directory "/tmp/pdf")
  ;; (setenv "TYPST_PROJECT_ROOT" denote-directory)

  ;; (setq typst-ts-compile-options "--root=$TYPST_PROJECT_ROOT --pdf-standard=a-2b")
  ;; (setq typst-ts-watch-options (list "--root" (getenv "TYPST_PROJECT_ROOT")))



  (require 'typst-citar)
  (require 'typst-extensions)
  (require 'typst-movement)
  (require 'typst-select)
  (require 'typst-consult)
  (require 'typst-snippets)
  (require 'typst-surround)
  )

;; Live preview via tinymist with source <-> preview sync (SyncTeX-like).
;; Click in the preview jumps to the source; C-c j jumps the preview to point.
(use-package typst-preview
  :ensure t
  :after typst-ts-mode
  :custom
  (typst-preview-executable "tinymist")
  (typst-preview-ask-if-pin-main nil)   ; don't write file-local variables
  (typst-preview-invert-colors "never")
  :bind (:map typst-ts-mode-map
              ("C-c v" . typst-preview-mode)
              ("C-c j" . typst-preview-send-position)))

(provide 'typst-config)

;;; typst.el ends here
