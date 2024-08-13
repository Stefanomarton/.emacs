;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

(use-package fix-word
  :ensure t
  :bind
  (([remap upcase-word] . fix-word-upcase))
  (([remap downcase-word] . fix-word-downcase))
  (([remap capitalize-word] . fix-word-capitalize))
  )

(use-package altcaps
  :ensure t
  :commands altcaps)

;; Startup time evaluation
(use-package esup
  :ensure t
  :commands esup)


(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "Italian")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "llama3" :embedding-model "llama3")))


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

;; (use-package golden-ratio
;;   :hook after-init
;;   :config
;;   (setq golden-ratio-exclude-modes '(emacs-lisp-compilation-mode)) ; Exclude modes from `golder-ratio-mode'
;;   (golden-ratio-mode))

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
  )

(use-package zoxide
  :ensure t
  :bind (:map global-map
              ("<escape>z" . zoxide-find-file)))

(use-package visual-regexp
  :ensure t
  :bind (:map global-map
              ([remap isearch-forward] . vr/isearch-forward)
              ([remap isearch-backward] . vr/isearch-backward)
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

(use-package casual-calc
  :ensure t
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))


(provide 'tools)

;;; tools.el ends here
