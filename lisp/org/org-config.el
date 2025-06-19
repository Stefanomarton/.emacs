;;; orgconfig.el --- org-mode configuration -*- lexical-binding: t; -*-
(use-package org
  :ensure t
  :bind (:map org-mode-map
              ("C-," . embrace-commander)
              ("C-c o h" . consult-org-heading)
              ("<escape> >" . org-promote-subtree)
              ("<escape> <" . org-demote-subtree)
              ("<escape> J" . my-fix-text-region))
  :hook
  ;; (org-mode . org-margin-mode)
  (org-mode . yas-minor-mode-on)
  (org-mode . er/add-latex-in-org-mode-expansions)
  (org-mode . my/org-header-outline-path-mode)
  (org-mode . auto-fill-mode)

  :custom
  (org-use-speed-commands t)
  (org-adapt-indentation nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width 500)
  (org-hide-leading-stars nil)
  (org-special-ctrl-a/e t)
  (org-catch-invisible-edits 'show)
  (org-id-link-to-org-use-id nil)

  :config
  ;; set notes folder used around
  (setq org-directory notes-folder)
  (setq denote-directory notes-folder)

  (setq org-blank-before-new-entry
        '((heading . t)
          (plain-list-item . t)))
  (setq
   org-ellipsis " ↓"
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t)

  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)))

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)) ;; fix dimension of latex fragments

  (add-to-list 'org-file-apps '("\\.pdf" . "zathura %s")) ;; open pdf files with zathura

  (add-hook 'org-mode-hook (lambda ()
                             (setq-local fill-column 110)
                             (setq-local set-fill-column 115)))

  ;; all possible latex highlight
  (setq org-highlight-latex-and-related '(native))

  (setq org-startup-folded t)
  (setq org-pretty-entities nil)
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-use-sub-superscripts '{})

  ;; Make surround with latex env work nicely
  (require 'tex-site)

  (defun my-fix-text-region (pos1 pos2)
    "Replace strings within a region."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region pos1 pos2)
        (save-excursion
          (goto-char pos1)
          (while (and (< (point) pos2) (not (eobp)))
            (join-line 1)))
        (fill-paragraph t t)
        (replace-regexp "- \([A-z]+\)" "\1")
        (dolist (ele (list "`a" "`e" "`o" "`u" "`i" "’"))
          (setq elt ele)
          (goto-char (point-min))
          (while (search-forward elt nil t 1)
            (replace-match
             (char-to-string
              (pcase ele
                ("` a" ?à)
                ("’" ?')
                ("` i" ?ì)
                ("` e" ?è)
                ("` o" ?ò)
                ("` u" ?ù)
                ("`a" ?à)
                ("`i" ?ì)
                ("`e" ?è)
                ("`o" ?ò)
                ("`u" ?ù)
                ))))))))

  (defun sbr-org-insert-dwim (&optional arg)
    "Insert another entry of the same type as the current
entry. For example, if the point is on a list item, then add
another list item of the same type, and if the point is on a
checkbox
 list item, then add an empty checkbox item. If instead
the point is in a heading, then add another heading. If the point
is in a TODO heading, then add another TODO heading (set to the
TODO state).

By default, the new entry is inserted below the current
subtree/item. With a 'C-u' prefix, insert the entry above the
current heading/item instead."
    (interactive "P")
    (when (eq major-mode 'org-mode)
      (let ((org-special-ctrl-a/e t)
            (below? (unless  (equal arg '(4)) '(4))))
        ;; hack to ensure that the point is not after ellipses because
        ;; that would mess up org-at-item-p etc.
        (org-beginning-of-line)
        (cond ((org-at-item-p) ;; at list item or checkbox
               (let ((org-M-RET-may-split-line nil)
                     (org-enable-sort-checkbox nil))
                 ;; hack to make item be inserted after the current one
                 ;; doesn't work if we are on an empty item line
                 (when below?
                   (org-end-of-line))
                 (org-insert-item (org-at-item-checkbox-p))))
              ((org-before-first-heading-p) ;; above first heading
               (org-insert-heading))
              (t ;; in some kind of heading
               (org-back-to-heading)
               (if (org-get-todo-state)
                   ;; at TODO heading
                   (org-insert-todo-heading t below?)
                 ;; at non-TODO heading
                 (org-insert-heading below?)))))))

  (defun sbr-org-shift-return (&optional arg)
    "If point is at a table, copy the table cell downward (i.e.,
the usual effect of typing S-RET). Otherwise,  insert the same
kind of heading or item as the current entry containing the
point. "
    (interactive "P")
    (if (org-at-table-p)
        (org-table-copy-down (prefix-numeric-value arg))
      (sbr-org-insert-dwim arg)))

  (bind-keys :map org-mode-map ("<S-return>" . sbr-org-insert-dwim))

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (cl-set-difference er/try-expand-list
                                      '(er/mark-method-call
                                        er/mark-inside-pairs
                                        er/mark-outside-pairs))
                   '(LaTeX-mark-environment
                     er/mark-LaTeX-inside-math
                     er/mark-latex-inside-pairs
                     er/mark-latex-outside-pairs
                     er/mark-LaTeX-math)))))

  (setq org-capture-templates
        '(("b" "Bookmark" entry
           (file "~/.marton-drive/notes/personal/20250526T212627--bookmarks__bookmarks.org")
           "** [[%^{Link}][%^{Description}]] :%^{Tag}:\nAdded: %U\nDescription:%^{Description}")))
  )

(provide 'org-config)

;;; org-config ends here
