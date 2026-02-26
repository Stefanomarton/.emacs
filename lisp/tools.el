;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

;; Startup time evaluation
(use-package esup
  :commands esup
  :ensure t
  )

(use-package eat
  :ensure t)

(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs")
  :bind (("<escape>ac" . claudemacs-transient-menu))
  )

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el")

  (setq client (acp-make-client :command "gemini"
                                :command-params '("--experimental-acp")
                                :environment-variables (when api-key
                                                         (list (format "GEMINI_API_KEY=%s" "your-api-key")))))

  (acp-send-request
   :client client
   :request (acp-make-initialize-request :protocol-version 1)
   :on-success (lambda (response)
                 (message "Initialize success: %s" response))
   :on-failure (lambda (error)
                 (message "Initialize failed: %s" error)))

  )



;; (use-package ellama
;;   :ensure t
;;   :bind (("<escape>ec" . ellama-chat)
;;          ("<escape>eiw" . ellama-improve-wording)
;;          ("<escape>eig" . ellama-improve-grammar)
;;          ("<escape>eic" . ellama-improve-conciseness)
;;          ("<escape>ea" . ellama-ask-about)
;;          )
;;   :init
;;   (setopt ellama-language "Italian")
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;;           (make-llm-ollama
;;            :chat-model "deepseek-r1:14b" :embedding-model "deepseek-r1:14b ")))

;; (use-package gptel
;;   :ensure t
;;   :config
;;   (setq gptel-default-mode 'org-mode)

;;   ;; scroll automatically
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   )

(use-package google-this
  :ensure t
  :commands google-this)

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode))

(use-package winner
  :ensure nil
  :config
  (winner-mode))

(use-package sudo-edit
  :ensure t
  :commands sudo-edit)

(use-package term-toggle
  :vc (:url "https://github.com/amno1/emacs-term-toggle" :branch "master")
  :bind (("<escape>." . term-toggle-eshell)))

(use-package eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  (setq  eshell-scroll-to-bottom-on-input 'all
         eshell-error-if-no-glob t
         eshell-hist-ignoredups t
         eshell-save-history-on-exit t
         eshell-prefer-lisp-functions nil
         eshell-destroy-buffer-when-process-dies t)
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize "[" 'face `(:foreground "#2aa198"))
           (propertize (user-login-name) 'face `(:foreground "#dc322f"))
           (propertize "@" 'face `(:foreground "#2aa198"))
           (propertize (system-name) 'face `(:foreground "#268bd2"))
           (propertize "]──[" 'face `(:foreground "#2aa198"))
           (propertize (concat (eshell/pwd)) 'face `(:foreground "#93a1a1"))
           (propertize "]\n" 'face `(:foreground "#2aa198"))
           (propertize (if (= (user-uid) 0) " # " " λ ") 'face `(:foreground "#2aa198"))
           ))))

(use-package visual-regexp
  :vc (:url "https://github.com/benma/visual-regexp.el" :branch "master")
  :bind (              ;; ([remap isearch-forward] . vr/isearch-forward)
         ;; ([remap isearch-backward] . vr/isearch-backward)
         ("C-M-s" . vr/replace)
         )
  :config
  (setq vr/auto-show-help nil))

(use-package visual-regexp-steroids
  :after visual-regexp
  :vc (:url "https://github.com/benma/visual-regexp-steroids.el" :branch "master"))

(use-package calc
  :ensure nil
  :bind
  ("<escape> c c" . calc)
  :config


  (defun eval-math-expr (beg end)
    (interactive "r")
    (require 'calc)
    (let ((result
           (calc-eval
            (buffer-substring beg end))))
      (save-excursion
        (goto-char end) (insert " = " result))))
  )

(use-package casual
  :ensure t
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))

(use-package diff
  :ensure nil
  :bind
  ("C-c u d" . diff-current-buffer-with-file)
  :config
  (defun diff-current-buffer-with-file ()
    (interactive)
    (diff-buffer-with-file (current-buffer))))

(provide 'tools)

;;; tools.el ends here
