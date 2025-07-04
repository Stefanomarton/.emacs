(with-eval-after-load 'typst-ts-mode

  (require 'aas)

  (add-hook 'typst-ts-mode-hook #'aas-activate-for-major-mode)
  
  (defun typst-inside-math-p ()
    "Return t if point is inside a math-related Tree-sitter node."
    (let ((node (treesit-node-at (point))))
      (while (and node
                  (not (member (treesit-node-type node)
                               '("math" "math_block" "inline_math" "equation"))))
        (setq node (treesit-node-parent node)))
      (when node t)))
  
  
  (defun typst-snippets-table (columns &optional rows)
    "Insert a Typst #table snippet with COLUMNS and ROWS, using snippet fields."
    (interactive "nNumber of columns: ")
    (let* ((rows (or rows 3))
           (col-names (cl-loop for i from 1 to columns collect (format "[${%d}]" i)))
           (header (string-join col-names ", "))
           (body "")
           (counter (+ 1 columns)))
      ;; Generate body rows
      (dotimes (_ rows)
        (let ((row (cl-loop for j from 0 below columns
                            collect (format "[${%d}]" (+ counter j)))))
          (setq counter (+ counter columns))
          (setq body (concat body "  " (string-join row ", ") ",\n"))))
      ;; Insert full snippet
      (yas-expand-snippet
       (format "#table(\n  stroke: none,\n  columns: %d,\n  table.header(%s),\n%s)" 
               columns header body))))

  (defun typst-snippets-matrix (columns &optional rows)
    "Insert a Typst `mat( … )` snippet with COLUMNS × ROWS fields.

Interactively:
  • Prompts for number of COLUMNS and ROWS (default 3).
  • Expands a yasnippet so each matrix cell is a numbered field: ${1}, ${2}, …

Example call → 2×3 matrix:
mat(
  ${1}, ${2};
  ${3}, ${4};
  ${5}, ${6};
)"
    (interactive
     (let* ((cols (read-number "Number of columns: "))
            (rows (read-number "Number of rows: ")))
       (list cols rows)))
    (let ((counter 1)
          (body ""))
      ;; Build the matrix row by row
      (dotimes (_ rows)
        (setq body (concat body "  "))      ; indent
        (dotimes (c columns)
          (setq body (concat body (format "${%d}" counter)))
          (setq counter (1+ counter))
          (unless (= c (1- columns))
            (setq body (concat body ", ")))) ; field separator
        (setq body (concat body ";\n")))     ; row separator
      ;; Insert full snippet
      (yas-expand-snippet (concat "mat(\n" body ")"))))

  (defun typst-wrap-prev-node (left right)
    "Wrap the previous node or text with LEFT and RIGHT strings.
Preserves quotes in string nodes."
    (let* ((node (treesit-node-at (point)))
           (target nil)
           (text nil))
      ;; Tree-sitter: find node ending at point
      (while (and node (not (= (treesit-node-end node) (point))))
        (setq node (treesit-node-parent node)))
      (when (and node
                 (member (treesit-node-type node)
                         '("identifier" "reference" "math_variable" "number" "letter" "string")))
        (setq target node))
      (if target
          (progn
            (setq text (treesit-node-text target t))
            (delete-region (treesit-node-start target) (treesit-node-end target)))
        ;; fallback: take preceding word or string
        (let ((end (point))
              (start (save-excursion
                       (skip-chars-backward "\"[:alnum:]_")
                       (point))))
          (when (< start end)
            (setq text (buffer-substring-no-properties start end))
            (delete-region start end))))
      (yas-expand-snippet
       (if text
           (concat left text right "$0")
         (concat left "$1" right "$0")))))

  (aas-set-snippets 'typst-ts-mode

    "kd" (lambda () (interactive)
	       (yas-expand-snippet "$\n$1\n$\n$0"))

    "jf" (lambda () (interactive)
	       (yas-expand-snippet "$ $1 $ $0"))

    "jc" (lambda () (interactive)
	       (yas-expand-snippet "#ce(\"$1\")$0"))

    "jq" (lambda () (interactive)
	       (yas-expand-snippet "$\n#ce(\"$1\")\n$\n$0"))

    ;; math (guarded)
    :cond #'typst-inside-math-p
    ";a" "alpha "
    ";A" "Alpha "
    ";b" "beta "
    ";B" "Beta "
    ";d" "delta "
    ";D" "Delta "
    ";e" "epsilon "
    ";g" "gamma "
    ";G" "Gamma "

    "qq" "quad"
    
    ";i" "integral"

    ";;m" #'typst-snippets-matrix

    ";I" (lambda () (interactive)
	       (yas-expand-snippet "integral_($1)^($2) $0"))
    
    ".b"     (lambda () (interactive)
               (typst-wrap-prev-node "bold(" ")"))
    
    ".v"     (lambda () (interactive)
               (typst-wrap-prev-node "vec(" ")"))

    ".q"     (lambda () (interactive)
               (typst-wrap-prev-node "sqrt(" ")"))

    ".s"     (lambda () (interactive)
               (typst-wrap-prev-node "\"" "\""))

    ".c"     (lambda () (interactive)
               (typst-wrap-prev-node "cancel(" ")"))

    ".a"     (lambda () (interactive)
               (typst-wrap-prev-node "abs(" ")"))

    ".l"     (lambda () (interactive)
               (typst-wrap-prev-node "(" ")"))

    ".ub"     (lambda () (interactive)
                (typst-wrap-prev-node "underbrace(" ")"))
    
    ".ul"     (lambda () (interactive)
                (typst-wrap-prev-node "underline(" ")"))
    
    ".up"     (lambda () (interactive)
                (typst-wrap-prev-node "underparen(" ")"))

    ".us"     (lambda () (interactive)
                (typst-wrap-prev-node "undershell(" ")"))

    
    )
  )

(provide 'typst-snippets)
