;;; typst.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(with-eval-after-load 'typst-ts-mode

  (defvar-keymap my/cycle-repeat-map
    :repeat (:enter (outline-cycle-buffer outline-next-heading outline-previous-heading) :exit (consult-outline))
    "<tab>" #'outline-cycle-buffer
    "n" #'outline-next-heading
    "p" #'outline-previous-heading
    "C-<down>" #'outline-move-subtree-down
    "C-<up>" #'outline-move-subtree-up
    ">" #'outline-demote
    "<" #'outline-promote
    "o" #'consult-outline
    "q" #'repeat-exit
    )

  (which-key-add-keymap-based-replacements
    my/cycle-repeat-map
    "<tab>"  "cycle/vis toggle"
    "n"      "next heading"
    "p"      "prev heading"
    "C-<down>" "move subtree ↓"
    "C-<up>"   "move subtree ↑"
    ">"      "demote subtree"
    "<"      "promote subtree"
    "o"      "outline")

  (defun copy-formula-and-pop-mark ()
    "Select the current formula node, copy its text, and pop the mark."
    (interactive)
    (let ((start (point)))
      (deactivate-mark)
      (next-line)
      (sm/treesit-select-node "formula")
      (kill-ring-save (point) (mark))
      (pop-global-mark)
      (message "Formula copied to kill ring.")))

  (defvar-keymap my/typst-math-repeat-map
    :repeat (:enter (sm/treesit-select-nearest-math-after sm/treesit-select-nearest-math-before) :exit (copy-formula-and-pop-mark))
    "m" #'sm/treesit-select-nearest-math-after
    "M" #'sm/treesit-select-nearest-math-before
    "k" #'copy-formula-and-pop-mark
    )
  )


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

  (define-key typst-ts-mode-map (kbd "C-c rs")
              (lambda () (interactive) (sm/treesit-select-node "section")))

  (define-key typst-ts-mode-map (kbd "C-c rm")
              (lambda () (interactive) (sm/treesit-select-node "math")))

  (define-key typst-ts-mode-map (kbd "C-c c")
              (lambda () (interactive) (sm/treesit-select-node "code")))

  (defun sm/treesit-select-nearest-math-after ()
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

  (defun sm/treesit-select-nearest-math-before ()
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
  
  (define-key typst-ts-mode-map (kbd "C-c m") #'sm/treesit-select-nearest-math-after)
  (define-key typst-ts-mode-map (kbd "C-c M") #'sm/treesit-select-nearest-math-before)
  )




(provide 'typst-movement)
