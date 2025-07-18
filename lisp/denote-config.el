;;; denote.el -*- lexical-binding: t; -*-;;;
(setq custom-file (locate-user-emacs-file "custom.el"))

(use-package denote-journal
  :vc (:url https://github.com/protesilaos/denote-journal
            :branch "main")
  :bind
  ("<escape>dj" . denote-journal-new-or-existing-entry)
  :config
  (setq denote-journal-directory (expand-file-name "personal/journal" denote-directory))
  (setq denote-journal-title-format "%Y %m %d")
  )

(use-package denote
  :defer 0.5
  :vc (:url "https://github.com/protesilaos/denote")
  :init
  (setq denote-directory notes-folder)

  :bind
  ("<escape>do" . my/denote-open-or-create)

  ("<escape>dr" . denote-region)
  ("<escape>dg" . consult-denote-grep)
  ("<escape>dt" . denote-type)
  ("<escape>dl" . denote-link)
  ("<escape>dL" . denote-find-link)
  ("<escape>db" . denote-backlinks)
  ("<escape>dB" . denote-find-backlink)
  ("<escape>dr" . denote-rename-file)
  ("<escape>dR" . denote-rename-file-using-front-matter)

  :hook
  (dired-mode . denote-dired-mode)
  (text-mode-hook . denote-fontify-links-mode-maybe)

  :config
  (setq-default denote-directory (concat drive-folder "notes"))

  ;; (setq denote-templates
  ;;       `((journal . ,(concat "* Score giornaliero (0-10)\n\n"
  ;;                             "Serenità: \n"
  ;;                             "Energia: \n"
  ;;                             "\n\n"
  ;;                             "* Commento generale giornata"
  ;;                             "\n\n"
  ;;                             "* Riflessione del giorno\n"
  ;;                             ))))

  ;; Modify default command to use `denote-subdirectory' when creating TARGET
  (defun my/denote-open-or-create (target)
    (interactive (list (denote-file-prompt nil nil :dno-require-match)))
    (if (and target (file-exists-p target))
        (find-file target)
      (denote--command-with-features #'denote-subdirectory :use-last-input-as-def-title nil nil nil)))

  (setq denote-excluded-directories-regexp ".output")
  (setq denote-backlinks-show-context t)

  ;; link configuration
  (defun my/denote-link-description-with-signature-and-title (file)
    "Return link description for FILE.
    - If the region is active, use it as the description.
    - If the region is not active, ask for a prompt and use the input.
    - If the prompt is empty, check if FILE has a signature, then format the description
    as a sequence of the signature text and the title with two spaces between them.
    - If FILE does not have a signature, then use its title as the description.

    This is useful as the value of the user option
    `denote-link-description-function`."

    (let* ((file-type (denote-filetype-heuristics file))
           (signature (denote-retrieve-filename-signature file))
           (title (denote-retrieve-title-or-filename file file-type))
           (region-text (denote--get-active-region-content))
           (prompt-text (if (string-empty-p region-text)
                            (read-string "Enter description (leave blank for default): ")
                          nil)))  ; Don't ask for prompt if region is active
      (cond
       (region-text region-text)
       ((not (string-empty-p prompt-text)) prompt-text)
       ((and signature title) (format "%s  %s" signature title))
       (title (format "%s" title))
       (signature (format "%s" signature))
       (t ""))))



  (setq denote-link-description-format #'my/denote-link-description-with-signature-and-title)

  (defvar my-denote-typst-front-matter
    "#let title =      \"%s\"\n#let date=      \"%s\"\n#let tags=       \"%s\"\n#let identifier= \"%s\"\n")

  ;; Add typst as filetype
  (setq denote-file-types '((org :extension ".org" :front-matter denote-org-front-matter
                                 :title-key-regexp "^#\\+title\\s-*:" :title-value-function
                                 denote-format-string-for-org-front-matter
                                 :title-value-reverse-function denote-trim-whitespace
                                 :keywords-key-regexp "^#\\+filetags\\s-*:"
                                 :keywords-value-function
                                 denote-format-keywords-for-org-front-matter
                                 :keywords-value-reverse-function
                                 denote-extract-keywords-from-front-matter :signature-key-regexp
                                 "^#\\+signature\\s-*:" :signature-value-function
                                 denote-format-string-for-org-front-matter
                                 :signature-value-reverse-function denote-trim-whitespace
                                 :identifier-key-regexp "^#\\+identifier\\s-*:"
                                 :identifier-value-function
                                 denote-format-string-for-org-front-matter
                                 :identifier-value-reverse-function denote-trim-whitespace
                                 :date-key-regexp "^#\\+date\\s-*:" :date-value-function
                                 denote-date-org-timestamp :date-value-reverse-function
                                 denote-extract-date-from-front-matter :link
                                 denote-org-link-format :link-in-context-regexp
                                 denote-org-link-in-context-regexp)
                            (typst :extension ".typ"
                                   :front-matter my-denote-typst-front-matter
                                   :title-key-regexp "^#let\\s-+title\\s-*=\\s-*\""
                                   :title-value-function denote-format-string-for-org-front-matter
                                   :title-value-reverse-function denote-trim-whitespace
                                   :keywords-key-regexp "^#let\\s-+tags\\s-*=\\s-*\""
                                   :keywords-value-function denote-format-keywords-for-text-front-matter
                                   :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                                   :signature-key-regexp "^#let\\s-+signature\\s-*=\\s-*\""
                                   :signature-value-function denote-format-string-for-org-front-matter
                                   :signature-value-reverse-function denote-trim-whitespace
                                   :identifier-key-regexp "^#let\\s-+identifier\\s-*=\\s-*\""
                                   :identifier-value-function denote-format-string-for-org-front-matter
                                   :identifier-value-reverse-function denote-trim-whitespace
                                   :date-key-regexp "^#let\\s-+date\\s-*=\\s-*\""
                                   :date-value-function denote-date-iso-8601
                                   :date-value-reverse-function denote-extract-date-from-front-matter
                                   :link denote-org-link-format
                                   :link-in-context-regexp denote-org-link-in-context-regexp)
                            )))

(use-package consult-denote
  :requires (denote consult)
  :ensure t
  :bind
  ("<escape>dg" . consult-denote-grep)
  :config
  (setq consult-denote-grep-command #'consult-ripgrep) ; use ripgrep
  (consult-denote-mode))

(use-package citar-denote
  :after denote
  :ensure t
  :bind
  (:map global-map
        ("<escape>dpc" . citar-create-note)
        ("<escape>dpo" . citar-denote-open-note)
        ("<escape>dpn" . citar-denote-nocite))
  (:map org-mode-map
        ("<escape>dpk" . citar-denote-add-citekey)
        ("<escape>dpK" . citar-denote-remove-citekey)
        ("<escape>dpd" . citar-denote-dwim)
        ("<escape>dpe" . citar-denote-open-reference-entry))
  :config
  (setq citar-open-always-create-notes t)
  (setq citar-denote-subdir "/uni/papers")
  (citar-denote-mode))

(provide 'denote-config)
