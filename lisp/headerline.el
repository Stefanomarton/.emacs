;;; headerline.el --- Minimal Headerline -*- lexical-binding: t; -*-

(defface org-outline-path-headerline-face
  '((t (:height 140 :foreground "white" :weight ultra-bold)))
  "face for headerline outline in org-mode")

(defun org-outline-path-headerline ()
  (concat " - " (org-display-outline-path nil t "  " t)))

(defvar-local my/org-header-old-header nil
  "Old value of `header-line-format'.")

(define-minor-mode my/org-header-outline-path-mode
  "Show outline path in header line."
  :ligher nil
  (if my/org-header-outline-path-mode
      (progn (setq my/org-header-old-header header-line-format)
             (setq header-line-format '((:eval (org-get-title 'current-buffer))(:eval (org-outline-path-headerline)))))
    (setq header-line-format my/org-header-old-header)))
