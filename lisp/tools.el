;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

(use-package fix-word
  :bind
  (([remap upcase-word] . fix-word-upcase))
  (([remap downcase-word] . fix-word-downcase))
  (([remap capitalize-word] . fix-word-capitalize))
  )

(use-package altcaps
  :commands altcaps)

;; Startup time evaluation
(use-package esup
  :commands esup)

;; (use-package vterm
;;   :commands vterm
;;   :bind (:map global-map
;;               ("<escape>v" . vterm-other-window))
;;   :config
;;   ;; (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font"))
;;   :custom
;;   (setq term-toggle-no-confirm-exit t)
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
;; 	    vterm-internal-use-ligatures t
;; 	    vterm-max-scrollback 10000
;; 	    vterm-shell "zsh"
;; 	    ))

(use-package ellama
  :init
  (setopt ellama-language "Italian")
  (require 'llm-ollama)
  (setopt ellama-provider
		  '(
            ("llama3" . (make-llm-ollama
				         :chat-model "llama3"
				         :embedding-model "llama3"))
            ("codegemma" . (make-llm-ollama
				            :chat-model "codegemma:instruct"
				            :embedding-model "codegemma:instruct")))))

;; (use-package elisa
;;   :init
;;   (setopt elisa-limit 5)
;;   ;; reranker increases answer quality significantly
;;   (setopt elisa-reranker-enabled t)
;;   ;; prompt rewriting may increase quality of answers
;;   ;; disable it if you want direct control over prompt
;;   (setopt elisa-prompt-rewriting-enabled t)
;;   (require 'llm-ollama)
;;   ;; gemma 2 works very good in my use cases
;;   ;; it also boasts strong multilingual capabilities
;;   (setopt elisa-chat-provider
;; 	      (make-llm-ollama
;; 	       :chat-model "gemma2:9b-instruct-q6_K"
;; 	       :embedding-model "chatfire/bge-m3:q8_0"
;; 	       ;; set context window to 8k
;; 	       :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   ;; this embedding model has stong multilingual capabilities
;;   (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model "chatfire/bge-m3:q8_0"))
;;   :config
;;   ;; searxng works better than duckduckgo in my tests
;;   (setopt elisa-web-search-function 'elisa-search-searxng))

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

;; (use-package golden-ratio
;;   :hook after-init
;;   :config
;;   (setq golden-ratio-exclude-modes '(emacs-lisp-compilation-mode)) ; Exclude modes from `golder-ratio-mode'
;;   (golden-ratio-mode))

(use-package winner
  :hook after-init
  :config
  (winner-mode))

(use-package ebuku
  :commands ebuku)


(use-package sudo-edit
  :commands sudo-edit)

(use-package popper
  :ensure t ; or :straight t
  :bind (("<escape>,"   . popper-toggle)
         ("<escape>."   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (setq popper-group-function #'popper-group-by-projectile)
  ;; (popper-echo-mode +1)
  ;; Match eshell, shell, term and/or vterm buffers
  )

(use-package zoxide
  :bind (:map global-map
              ("<escape>z" . zoxide-find-file)))

(use-package visual-regexp
  :bind (:map global-map
              ([remap isearch-forward] . vr/isearch-forward)
              ([remap isearch-backward] . vr/isearch-backward)
              ("C-M-s" . vr/replace)
              )
  :config
  (setq vr/auto-show-help nil)
  (use-package visual-regexp-steroids))

(provide 'tools)

;;; tools.el ends here
