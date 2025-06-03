;;; document-production.el --- document production configuration -*- lexical-binding: t; -*-

;; Common fast pdf viewer inside emacs
(use-package pdf-tools
  :ensure t
  :after (LaTeX-mode markdown-mode org)
  :config
  (setq-default pdf-view-display-size 'fit-page) ; Fit page width
  (setq pdf-annot-activate-created-annotations t) ; Enable annotations
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
  (pdf-tools-install))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md?\\'" . markdown-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  (defun my/export-md-to-pdf ()
    "Export the current Markdown buffer to PDF using Pandoc with conditional flags."
    (interactive)
    (let* ((md-file (buffer-file-name))
           (output-file (concat (file-name-sans-extension md-file) ".pdf"))
           (default-directory (file-name-directory md-file))
           (config-file (concat default-directory "config.yaml"))
           (template-file (concat default-directory "template.latex"))
           (metadata-flag (if (file-exists-p config-file) (format "--metadata-file=%s" config-file) ""))
           (template-flag (if (file-exists-p template-file) (format "--template=%s" template-file) ""))
           (pandoc-command (format "pandoc -s %s %s %s -o %s --pdf-engine=xelatex"
                                   md-file metadata-flag template-flag output-file)))

      (message "Exporting Markdown file to PDF")
      (start-process-shell-command "pandoc-export" nil pandoc-command)))

  (defun my/open-pdf-with-zathura ()
    "Open the PDF file associated with the current buffer in Zathura."
    (interactive)
    (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
      (start-process "zathura" nil "zathura" pdf-file)))

  (define-key markdown-mode-map (kbd "C-c C-e") 'my/export-md-to-pdf)
  (define-key markdown-mode-map (kbd "C-c C-v") 'my/open-pdf-with-zathura)

  ;; Math and fontify
  (setq markdown-fontify-code-blocks-natively t)
  :init
  (setq markdown-enable-math t
        markdown-enable-highlighting-syntax t))


(use-package tex
  :ensure
  (auctex :pre-build ((or (executable-find "make") (error "No make executable found"))
                      ("./autogen.sh")
                      ("./configure" "--without-texmf-dir"
                       "--with-packagelispdir=./"
                       "--with-packagedatadir=./")
                      ("make"))
          :build (:not elpaca--compile-info) ; Make will take care of this step
          :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
          :version (lambda (_) (require 'tex-site) AUCTeX-version)
          :depth nil
          )
  :mode ("\\.tex?\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode-hook . prettify-symbols-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(latex-mode . LaTeX-mode))
  (advice-add #'TeX-completing-read-multiple :around #'vertico--advice)

  (setq TeX-save-query nil
	    TeX-clean-confirm nil
        TeX-command-extra-options "--shell-escape"
	    TeX-source-correlate-start-server t
	    TeX-source-correlate-method 'synctex)

  (TeX-source-correlate-mode 1)
  (add-to-list 'TeX-view-program-selection
	           '(output-pdf "Zathura"))

  (defun my-export-to-pdf ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean))

  (defun my-export-to-pdf-and-view ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean)
    (TeX-view)
    )

  ;; Toggle between master and current compilation
  (defvar my-latex-original-master nil
    "Variable to store the original value of TeX-master.")

  (defun my-latex-toggle-command ()
    "Toggle between executing commands on master and current file."
    (interactive)
    (if my-latex-original-master
	    (progn
	      (setq TeX-master my-latex-original-master)
	      (setq my-latex-original-master nil))
      (progn
	    (setq my-latex-original-master TeX-master)
	    (setq TeX-master nil)))
    (message "Switched command: %s" (if TeX-master "master" "current")))

  ;; (evil-define-key 'normal LaTeX-mode-map
  ;;   (kbd "<leader> ee") 'my-export-to-pdf
  ;;   (kbd "C-c T") 'my-latex-toggle-command
  ;;   (kbd "C-c E") 'my-export-to-pdf-view
  ;;   (kbd "C-c t") 'lsp-ui-imenu)

  (defun my-select-frac ()
    "Select the \\frac command and move to the start of the nearest \\frac."
    (interactive)
    (let ((current-point (point))
          (frac-start nil))
      (save-excursion
	    (when (re-search-backward "\\\\frac" nil t)
          (setq frac-start (match-beginning 0))
          (when (and (<= frac-start current-point) (<= current-point (match-end 0)))
            (setq current-point frac-start)
            (setq frac-start nil))))
      (if frac-start
          (goto-char frac-start)
	    (message "No \\frac found")))
    (when (looking-at "\\\\frac")
      (let ((start (point))
            (end (progn
                   (search-forward "{")
                   (backward-char)
                   (let ((level 1))
                     (while (> level 0)
                       (search-forward-regexp "{\\|}" nil t)
                       (if (string= (match-string 0) "{")
                           (setq level (1+ level))
			             (setq level (1- level)))))
                   (backward-char)
                   (point))))
	    (set-mark start)
	    (goto-char end)))))

;; (global-set-key (kbd "C-c f") 'my-select-frac)
;; (evil-define-key 'normal LaTeX-mode-map (kbd "<leader>r") 'my-select-frac))


(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)
  :hook
  (text-mode . yas-minor-mode)
  (prog-mode . yas-minor-mode)
  (LaTeX-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  (yas-minor-mode . yas-reload-all)
  (snippet-mode . disable-final-newline)
  :preface
  (defun make-silent (func &rest args)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) nil)))
      (apply func args)))
  (advice-add 'yas-reload-all :around #'make-silent)
  (add-hook 'org-mode '(lambda () (setq-local yas-indent-line 'fixed)))
  :config
  (defun disable-final-newline ()
    (interactive)
    (set (make-local-variable 'require-final-newline) nil))
  (yas-global-mode 1)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")))

(use-package warnings
  :ensure nil
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package auto-yasnippet
  :ensure t
  :bind
  (:map global-map
        ("<escape> y c" . my-aya-create)
        ("<escape> y e" . aya-expand)
        ("<escape> y E" . aya-expand-from-history)
        ("<escape> y d" . aya-delete-from-history)
        ("<escape> y h" . aya-clear-history)
        ("<escape> y n" . aya-next-in-history)
        ("<escape> y p" . aya-previous-in-history)
        ("<escape> y p" . aya-persist-snippet)
        ("<escape> y o" . aya-open-line))
  :config
  (defun my-aya-create (beg end)
    (interactive "r")
    (let ((count 0))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "\\b\\([0-9]+\\)\\b" nil t)
          (replace-match "~\\1")
          (setq count (1+ count)))
        (aya-create beg (+ count end))
        (delete-region beg end)
        (aya-expand count)))
    (recenter-top-bottom)
    ))

(use-package aas
  :ensure t
  :hook
  (org-mode . aas-activate-for-major-mode)
  (markdown-mode . aas-activate-for-major-mode)
  (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'LaTeX-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\($1\\\\) $0"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd  " (lambda () (interactive)
	         (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'org-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\( $1 \\\\) $0"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd" (lambda () (interactive)
           (setq-local yas-indent-line 'auto)
	       (yas-expand-snippet "\\[ \n $1 \n \\]\n $0")))
  (aas-set-snippets 'markdown-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "$ $1$ $0 $"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))

(use-package laas
  :ensure (:host github :repo "tecosaur/LaTeX-auto-activating-snippets")
  ;; :ensure t
  :hook
  (LaTeX-mode . laas-mode)
  (markdown-mode . laas-mode)
  (org-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
    ;; set condition!
    :cond #'texmathp ; expand only while in math

    ",t" (lambda () (interactive)
	       (yas-expand-snippet "\\int"))

    ".." (lambda () (interactive)
	       (yas-expand-snippet "_{$1}$0"))
    "ds" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta S $0"))
    "dh" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta H $0"))
    "dg" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta G $0"))

    ;; positive apices
    ",," (lambda () (interactive)
	       (yas-expand-snippet "^{$1}$0"))
    ",x" (lambda () (interactive)
	       (yas-expand-snippet "^{1}$0"))
    ",c" (lambda () (interactive)
	       (yas-expand-snippet "^{2}$0"))
    ",v" (lambda () (interactive)
	       (yas-expand-snippet "^{3}$0"))
    ",s" (lambda () (interactive)
	       (yas-expand-snippet "^{4}$0"))
    ",d" (lambda () (interactive)
	       (yas-expand-snippet "^{5}}$0"))
    ",f" (lambda () (interactive)
	       (yas-expand-snippet "^{6}$0"))
    ",w" (lambda () (interactive)
	       (yas-expand-snippet "^{7}$0"))
    ",e" (lambda () (interactive)
	       (yas-expand-snippet "^{8}$0"))
    ",r" (lambda () (interactive)
	       (yas-expand-snippet "^{9}$0"))

    ;; negative apices
    ".." (lambda () (interactive)
	       (yas-expand-snippet "^{-$1}$0"))
    ".x" (lambda () (interactive)
	       (yas-expand-snippet "^{-1}$0"))
    ".c" (lambda () (interactive)
	       (yas-expand-snippet "^{-2}$0"))
    ".v" (lambda () (interactive)
	       (yas-expand-snippet "^{-3}$0"))
    ".s" (lambda () (interactive)
	       (yas-expand-snippet "^{-4}$0"))
    ".d" (lambda () (interactive)
	       (yas-expand-snippet "^{-5}$0"))
    ".f" (lambda () (interactive)
	       (yas-expand-snippet "^{-6}$0"))
    ".w" (lambda () (interactive)
	       (yas-expand-snippet "^{-7}$0"))
    ".e" (lambda () (interactive)
	       (yas-expand-snippet "^{-8}$0"))
    ".r" (lambda () (interactive)
	       (yas-expand-snippet "^{-9}$0"))

    ".," (lambda () (interactive)
	       (yas-expand-snippet "^{$1}_{$0}"))

    "kk" (lambda () (interactive)
	       (yas-expand-snippet "_{$1}$0"))

    "++" (lambda () (interactive)
	       (yas-expand-snippet "^+ $0"))

    "--" (lambda () (interactive)
	       (yas-expand-snippet "^- $0"))

    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    ".q" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ".v" (lambda () (interactive) (laas-wrap-previous-object "vec"))
    ".t" (lambda () (interactive) (laas-wrap-previous-object "text"))
    ".b" (lambda () (interactive) (laas-wrap-previous-object "mathbf")))
  :init
  (defvar AUCTeX-version "13.2")
  )

(use-package cdlatex
  ;; :commands latex-mode
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-math-modify-prefix ?~)
  ;; (cdlatex-math-symbol-prefix nil)
  )

(use-package jinx
  :hook
  (org-mode . jinx-mode)
  :bind
  (:map org-mode-map
        ("M-$" . jinx-correct))
  (:map text-mode-map
        ("M-$" . jinx-correct))
  :ensure (:host github :repo "minad/jinx")
  :init
  ;; must load it before starting jinx-mode
  (setq jinx-languages "it en_US"))


(provide 'document-production)

;;; document-production.el ends here
