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
    :repeat (:enter (typst-select-math-after typst-select-math-before) :exit (copy-formula-and-pop-mark))
    "m" #'typst-select-math-after
    "M" #'typst-select-math-before
    "k" #'copy-formula-and-pop-mark
    )
  )

(provide 'typst-movement)
