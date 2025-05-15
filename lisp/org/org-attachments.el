;;; org-attachments.el --- org-mode configuration -*- lexical-binding: t; -*-

(setq org-link-abbrev-alist
      `(("image-dir" . ,(format "file:%s%s" notes-folder "/.attachments/"))))

;; Dinamically define attachments folder
(defvar org-attachments-folder nil
  "Variable to store the attachments folder for Org mode files.")

(defun extract-timestamp (filename)
  "Extract the timestamp from a FILENAME in the format '20240622T150711--...'."
  (string-match "\\`\\([0-9T]+\\)--" filename)
  (match-string 1 filename))

(defun org-set-attachments-folder ()
  "Set the attachments folder for the current Org mode buffer."
  (let ((buf-dir (file-name-directory (buffer-file-name)))
        (org-dir (expand-file-name notes-folder)))
    (setq org-attachments-folder
          (if (string-prefix-p org-dir buf-dir)
              (concat org-dir "/.attachments/" (extract-timestamp (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
            ".attachments"))))

(use-package org-download
  :ensure t
  :commands (my/org-download-clipboard)
  :init
  (setq org-download-display-inline-images 'posframe)
  (setq org-download-method 'directory)
  (setq org-download-image-latex-width 7)
  (setq org-download-heading-lvl nil)

  :config
  (defun custom/org-download-dir ()
    "Download files `org-attachments-folder'"
    (setq-local org-download-image-dir (concat
                                        (org-set-attachments-folder)
    			         			    "/")))

  (advice-add 'my/org-download-clipboard :before #'custom/org-download-dir)

  ;; Modify function to avoid writing useless comment
  (defun my-org-download-annotate-default (link)
    "Annotate LINK with the time of download."
    (format ""
            (if (equal link org-download-screenshot-file)
                "screenshot"
              link)
            (format-time-string "%Y-%m-%d %H:%M:%S")))

  (setq org-download-annotate-function 'my-org-download-annotate-default)


  (defun my/org-download-link-format-function (filename)
    "Generate an Org link with a cleaned path, removing everything before the timestamp."
    (let ((regex ".*/\\([0-9]+T[0-9]+/.*\\)"))
      (format "[[image-dir:%s]]"
              (if (string-match regex filename)
                  (match-string 1 filename)
                filename))))

  (defun my/org-download-clipboard ()
    (interactive)
    (setq-local org-download-link-format-function 'my/org-download-link-format-function)
    (org-download-clipboard)))

(provide 'org-attachments)

;;; org-attachments ends here
