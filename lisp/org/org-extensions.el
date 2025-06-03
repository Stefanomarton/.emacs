;;; org-extensions.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package anki-editor
  :bind
  (:map org-mode-map
        ("<escape>aP" . anki-editor-push-notes))
  :ensure (:repo "anki-editor/anki-editor")
  :config
  (setq anki-editor-default-note-type "Personal")
  (setq anki-editor-note-match "+LEVEL>1&+ANKI")
  (setq anki-editor-ignored-org-tags '("ANKI"))

  ;; ;; see anki-editor.el source file
  ;; (defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  ;;   (let ((org-use-property-inheritance t))
  ;;     (apply #'org-map-entries
  ;;            func
  ;;            anki-editor-note-match
  ;;            scope
  ;;            skip)))
  )

(use-package org-anki
  :ensure t
  :bind
  (:map org-mode-map
        ("<escape>as" . my/org-anki-sync-all)
        ("<escape>aS" . org-anki-sync-entry)
        ("<escape>ad" . org-anki-delete-entry)
        ("<escape>aD" . org-anki-delete-all)
        ("<escape>ab" . org-anki-browse-entry)
        ("<escape>aS" . org-anki-sync-entry)
        ("<escape>af" . oaff-create-flashcard)
        )

  :config
  (defun my/org-anki-sync-all ()
    (interactive)
    "set `org-use-property-inheritance' before `org-anki-sync-all'"
    (setq-local org-use-property-inheritance t)
    (org-anki-sync-all))

  ;; Match all level >1, inherit tag from parent level
  (setq org-anki-default-match "LEVEL>1&+ANKI")

  ;; Set `org-anki-model-fields' to use my custom note type
  (setq org-anki-model-fields
        '(("Personal" "Front" "Back")
          ("Basic" "Front" "Back")
          ("Basic (and reversed card)" "Front" "Back")
          ("Basic (optional reversed card)" "Front" "Back")
          ("NameDescr" "Name" "Descr")
          ("Cloze" "Text" "Extra")))
  (setq org-anki-default-note-type "Personal")

  (use-package org-anki-fast-flashcards
    :ensure (:host github :repo "Stefanomarton/org-anki-fast-flash-cards")
    :bind
    (:map org-mode-map
          ("<escape>af" . oaff-create-flashcard)))
  )

(use-package org-transclusion
  :ensure t
  :after org-mode
  :bind (:map org-mode-map
              ("<leader>ota" . org-transclusion-add)
              ("<leader>otm" . org-transclusion-mode))
  :config
  (defun denote-org-transclusion-add (link plist)
    (when (string= "denote" (org-element-property :type link))
      (let* ((denote-id (org-element-property :path link))     ;; get denote id from denote:<denote-id> link
             (file-path (denote-get-path-by-id denote-id))     ;; path resolved by the id
             (new-link (with-temp-buffer                       ;; create a [[file:/path/to/denote/note]] org link
                         (insert "file:")                      ;; and store it in 'new-link' variable
                         (insert file-path)
                         (beginning-of-buffer)
                         (org-element-link-parser))))
        (org-transclusion-add-org-file new-link plist))))      ;; re-use the org transclusion infrastructure for file: links
  (cl-pushnew 'denote-org-transclusion-add                     ;; register the org transclusion 'plugin'
              org-transclusion-add-functions)
  )

(use-package org-ipe
  :ensure (:host github :repo "Stefanomarton/org-ipe")
  :config
  (defun my/org-ipe-insert-drawing ()
    (interactive)
    (org-set-attachments-folder)
    (setq-local org-ipe-folder (concat org-attachments-folder "/ipe"))
    (org-ipe-insert-drawing))
  )

(use-package org-table-auto-align
  :hook
  (org-mode . org-table-auto-align-mode)
  :ensure (:host github :repo "Stefanomarton/org-table-auto-align-mode"))


(use-package plantuml-mode
  :ensure t)

(use-package org-plantuml-mindmap
  :after org
  :ensure (:host github :repo "Stefanomarton/org-plantuml-mindmap")
  :config
  (setq org-plantuml-mindmap-result-type "svg")
  (advice-add 'org-plantuml-mindmap-create :before #'custom/plantuml-mindmap-folder)

  (defun custom/plantuml-mindmap-folder ()
    (setq-local org-plantuml-mindmap-folder (concat
    			                             (org-set-attachments-folder)
    			                             (file-name-sans-extension (buffer-name))
    			                             "/"))))

(use-package mermaid-mode
  :ensure t)

(use-package mermaid-ts-mode
  :ensure t)

(provide 'org-extensions)

;;; org-extensions ends here
