;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

;; Startup time evaluation
(use-package esup
  :commands esup)

(use-package vterm
  :commands vterm
  :config
  ;; (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font"))
  :custom
  (setq term-toggle-no-confirm-exit t)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
	    vterm-internal-use-ligatures t
	    vterm-max-scrollback 10000
	    vterm-shell "zsh"
	    ))

(use-package ellama
  :init
  (setopt ellama-language "Italian")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "llama3" :embedding-model "llama3")))

(use-package google-this
  :commands google-this)

(use-package csv-mode
  :mode ("\\.csv\\'" . kbd-mode))

(use-package pkg-info
  :defer t)

(use-package bug-hunter
  :defer t)

(use-package explain-pause-mode
  :defer t)

(use-package golden-ratio
  :hook after-init
  :config
  (setq golden-ratio-exclude-modes '(emacs-lisp-compilation-mode)) ; Exclude modes from `golder-ratio-mode'
  (golden-ratio-mode))

(use-package winner
  :hook after-init
  :config
  (winner-mode))

(provide 'tools)

;;; tools.el ends here
