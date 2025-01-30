;;; denote.el -*- lexical-binding: t; -*-;;;

(use-package denote
  :demand t
  :ensure (:wait t)

  :bind
  ("<escape>do" . my/denote-open-or-create)
  ("<escape>dj" . denote-journal-extras-new-or-existing-entry)
  ("<escape>dr" . denote-region)
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
  ;; Modify default command to use `denote-subdirectory' when creating TARGET
  (defun my/denote-open-or-create (target)
    (interactive (list (denote-file-prompt nil nil :no-require-match)))
    (if (and target (file-exists-p target))
        (find-file target)
      (denote--command-with-features #'denote-subdirectory :use-last-input-as-def-title nil nil nil)))


  (setq denote-directory (concat drive-folder "notes"))
  (setq denote-excluded-directories-regexp ".output")
  (setq denote-backlinks-show-context t)

  ;; journal config
  (require 'denote-journal-extras)
  (setq denote-journal-extras-directory (expand-file-name "personal/journal" denote-directory))
  (setq denote-journal-extras-title-format "%Y %m %d")

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

  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :requires (denote consult)
  :ensure t
  :bind
  ("<escape>dg" . consult-denote-grep)
  :config
  (setq consult-denote-grep-command #'consult-ripgrep) ; use ripgrep
  (consult-denote-mode))

(use-package citar-denote
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

(provide 'denote)
