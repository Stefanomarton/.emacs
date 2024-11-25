;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

(use-package altcaps
  :ensure t
  :commands altcaps)

;; Startup time evaluation
(use-package esup
  :ensure t
  :commands esup)


(use-package ellama
  :ensure t
  :bind (("<escape>ec" . ellama-chat)
         ("<escape>eiw" . ellama-improve-wording)
         ("<escape>eig" . ellama-improve-grammar)
         ("<escape>eic" . ellama-improve-conciseness)
         ("<escape>ea" . ellama-ask-about)
         )
  :init
  (setopt ellama-language "Italian")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "llama3.2-vision" :embedding-model "llama3.2-vision")))

(use-package google-this
  :ensure t
  :commands google-this)

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . kbd-mode))

(use-package pkg-info
  :ensure t
  :defer t)

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package explain-pause-mode
  :ensure (:host github :repo "lastquestion/explain-pause-mode")
  :defer t)

(use-package winner
  :ensure nil
  :hook after-init
  :config
  (winner-mode))

(use-package ebuku
  :ensure t
  :commands ebuku)

(use-package sudo-edit
  :ensure t
  :commands sudo-edit)

(use-package term-toggle
  :ensure (:host github :repo "amno1/emacs-term-toggle")
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

(use-package zoxide
  :ensure t
  :bind (:map global-map
              ("<escape>z" . zoxide-find-file)))

(use-package visual-regexp
  :ensure t
  :bind (:map global-map
              ;; ([remap isearch-forward] . vr/isearch-forward)
              ;; ([remap isearch-backward] . vr/isearch-backward)
              ("C-M-s" . vr/replace)
              )
  :config
  (setq vr/auto-show-help nil))

(use-package visual-regexp-steroids
  :ensure (:host github :repo "benma/visual-regexp-steroids.el"))

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
