(with-eval-after-load 'typst-ts-mode
  ;; --- Select current node ---
  (defun sm/treesit-select-node (type)
    "Select (mark) the closest ancestor node of TYPE at point."
    (interactive "sNode type: ")
    (let ((node (treesit-node-at (point))))
      (while (and node (not (string= (treesit-node-type node) type)))
        (setq node (treesit-node-parent node)))
      (if node
          (progn
            (goto-char (treesit-node-start node))
            (push-mark (treesit-node-end node) nil t)
            (message "Selected %s node" type))
        (message "No node of type '%s' found" type))))

  (defun typst-select-math-after ()
    "Select the nearest 'math' node *after* point."
    (interactive)
    (let ((node (treesit-node-at (point))))
      (if (string= (treesit-node-type node) "math")
          (progn
            (goto-char (treesit-node-start node))
            (push-mark (treesit-node-end node) nil t)
            (message "Selected current math node"))
        (let* ((root (treesit-buffer-root-node))
               (candidates
                (treesit-query-capture
                 root
                 '((math) @math))))
          (let ((nearest
                 (seq-find
                  (lambda (match)
                    (> (treesit-node-start (cdr match)) (point)))
                  (sort candidates
                        (lambda (a b)
                          (< (treesit-node-start (cdr a))
                             (treesit-node-start (cdr b))))))))
            (if nearest
                (let ((math-node (cdr nearest)))
                  (goto-char (treesit-node-start math-node))
                  (push-mark (treesit-node-end math-node) nil t)
                  (message "Selected nearest math node after point"))
              (message "No math node found after point")))))))

  (defun typst-select-math-before ()
    "Select the nearest 'math' node *before* point."
    (interactive)
    (let ((node (treesit-node-at (point))))
      (if (string= (treesit-node-type node) "math")
          (progn
            (goto-char (treesit-node-start node))
            (push-mark (treesit-node-end node) nil t)
            (message "Selected current math node"))
        (let* ((root (treesit-buffer-root-node))
               (candidates
                (treesit-query-capture
                 root
                 '((math) @math))))
          (let ((nearest
                 (seq-find
                  (lambda (match)
                    (< (treesit-node-start (cdr match)) (point)))
                  (reverse
                   (sort candidates
                         (lambda (a b)
                           (< (treesit-node-start (cdr a))
                              (treesit-node-start (cdr b)))))))))
            (if nearest
                (let ((math-node (cdr nearest)))
                  (goto-char (treesit-node-start math-node))
                  (push-mark (treesit-node-end math-node) nil t)
                  (message "Selected nearest math node before point"))
              (message "No math node found before point")))))))

;;;; 1 ── custom marking functions (treesit-based) ───────────────────────────
  (require 'expand-region)          ;; make sure library is loaded

  (defun er/treesit-text ()
    "Expand-Region: select current Typst `text` node."
    (interactive)
    (sm/treesit-select-node "text"))

  (defun er/treesit-fraction ()
    "Expand-Region: select current Typst `text` node."
    (interactive)
    (sm/treesit-select-node "fraction"))

  (defun er/treesit-fraction ()
    "Expand-Region: select current Typst `text` node."
    (interactive)
    (sm/treesit-select-node "group"))

  (defun er/treesit-section ()
    "Expand-Region: select current Typst `section` node."
    (interactive)
    (sm/treesit-select-node "section"))

;;;; 2 ── mode-specific setup for Typst-TS buffers ───────────────────────────
  (defun sm/typst-ts-expand-region-setup ()
    "Set a buffer-local `er/try-expand-list` tailored for Typst-TS."
    (setq-local
     er/try-expand-list
     '(
       ;; words & subwords first
       er/mark-word
       er/treesit-text
       er/treesit-fraction
       er/treesit-group
       ;; common text objects
       er/mark-inside-quotes er/mark-outside-quotes
       er/mark-inside-pairs  er/mark-outside-pairs
       ;; comments / URLs / e-mails
       ;; er/mark-comment er/mark-url er/mark-email
       ;; Tree-sitter generic node + text units
       er/mark-ts-node
       er/mark-text-sentence
       er/mark-text-paragraph
       ;; custom Typst section
       er/treesit-section
       ;; whole page (last resort)
       mark-page)))

  (add-hook 'typst-ts-mode-hook #'sm/typst-ts-expand-region-setup))


(provide 'typst-select)
