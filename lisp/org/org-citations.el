;;; org-citations.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package citar
  :ensure t
  :after org
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)

  (citar-file-note-extensions)
  :config
  (setq org-cite-global-bibliography '("~/.marton-drive/notes/.resources/bibliography.bib"))

  (setq citar-notes-paths (list (expand-file-name "uni/papers" notes-folder)))

  (setq citar-bibliography (expand-file-name ".resources/bibliography.bib" notes-folder))

  (defun citar-file-open (file)
	"Open FILE. Overwritten by hgi, to open pdf files from citar in external PDF viewer and not in internal one."
	(if (or (equal (file-name-extension file) "pdf") (equal (file-name-extension file) "html"))
		(citar-file-open-external (expand-file-name file))
	  (funcall citar-file-open-function (expand-file-name file))))

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after citar
  :config
  (citar-embark-mode))

(use-package org-ref
  :ensure t
  :after org)

(provide 'org-citations)

;;; org-citations ends here
