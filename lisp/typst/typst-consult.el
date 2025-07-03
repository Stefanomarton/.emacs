(with-eval-after-load 'typst-ts-mode

  (require 'consult)
  
;;;  ──  helper: collect math blocks ──────────────────────────────────────────
  (defun sm/typst--equation-alist ()
    "Return an alist of all Typst `math` nodes in the current buffer.
Each element is (DISPLAY . (BEG END)), where DISPLAY is the string shown
by Consult and BEG/END delimit the node."
    (let ((root (treesit-buffer-root-node)))
      (mapcar
       (lambda (m)
         (let* ((node (cdr m))
                (beg  (treesit-node-start node))
                (end  (treesit-node-end   node))
                ;; keep properties with `buffer-substring` 🡒 manual confirms
                ;; they are preserved  :contentReference[oaicite:0]{index=0}
                (txt  (buffer-substring beg end))
                (lab  (format "%4d: %s" (line-number-at-pos beg) txt)))
           (cons lab (list beg end))))        ; value is a *list* → no cdr traps
       (treesit-query-capture root '((math) @node)))))

;;;  ── 1 · jump version ──────────────────────────────────────────────────────
  (defun typst-consult-equations-jump ()
    "Let Consult pick a math block, then jump there and pulse it."
    (interactive)
    (let* ((alist (sm/typst--equation-alist)))
      (unless alist (user-error "No math blocks found"))
      (cl-destructuring-bind (beg end)
          (cdr (assoc (consult--read              ; consult core API :contentReference[oaicite:1]{index=1}
                       alist
                       :prompt "Equation: "
                       :require-match t
                       :sort nil
                       :category 'consult-location)
                      alist))
        (goto-char beg)
        (pulse-momentary-highlight-region beg end))))  ; visual feedback

;;;; ── mini-mode for the temp buffer ────────────────────────────────────────
  (defvar sm/typst--edit-origin nil
    "Cons of (BUFFER . MARKER) where edited equation should be inserted.")

;;; ── 2 · confirm edit: reinject with line-breaks  ─────────────────────────

  (defun sm/typst-equation-edit-confirm ()
    "Finish editing: insert buffer contents as-is, then close."
    (interactive)
    (unless sm/typst--edit-origin
      (user-error "No origin recorded"))
    (let* ((snippet (string-trim (buffer-string)))      ; keep delimiters
           (dest-buf (car sm/typst--edit-origin))
           (dest-mkr (cdr sm/typst--edit-origin)))
      (with-current-buffer dest-buf
        (goto-char dest-mkr)
        (insert snippet))
      (kill-buffer)
      (message "Equation inserted")))

  (defun sm/typst-equation-edit-cancel ()
    "Abort editing and close buffer."
    (interactive)
    (kill-buffer) (message "Edit cancelled"))

  (define-minor-mode sm/typst-equation-edit-mode
    "Minor mode for temporary Typst equation editing."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c C-c") #'sm/typst-equation-edit-confirm)
              (define-key map (kbd "C-c C-k") #'sm/typst-equation-edit-cancel)
              map)
    :lighter " EqEdit")

;;;; ── 1 · jump command ────────────────────────────────────────────────────
  (defun typst-consult-equations-jump ()
    "Fuzzy-pick a Typst math block and jump to it."
    (interactive)
    (let* ((alist (sm/typst--equation-alist)))
      (unless alist (user-error "No math blocks found"))
      (cl-destructuring-bind (beg end)
          (cdr (assoc (consult--read alist
                                     :prompt "Jump equation: "
                                     :require-match t
                                     :sort nil
                                     :category 'consult-location)
                      alist))
        (goto-char beg)
        (pulse-momentary-highlight-region beg end))))

;;;; ── 2 · insert / edit command ───────────────────────────────────────────
;;; ── 1 · temp-buffer set-up  ──────────────────────────────────────────────
  (defun typst-consult-equations-insert (edit)
    "Insert immediately, or (with C-u) open an editable Typst equation buffer.
The editable buffer now keeps the original `$ … $` delimiters."
    (interactive "P")
    (let ((alist (sm/typst--equation-alist)))
      (unless alist (user-error "No math blocks found"))
      (cl-destructuring-bind (beg end)
          (cdr (assoc (consult--read alist
                                     :prompt (if edit "Edit equation: "
                                               "Insert equation: ")
                                     :require-match t
                                     :sort nil
                                     :category 'consult-location)
                      alist))
        (let ((txt (buffer-substring beg end)))       ; keep delimiters
          (if edit
              ;; ── open temp edit buffer, text unchanged ────────────────
              (let* ((buf (generate-new-buffer "*Typst Equation Edit*"))
                     (mkr (copy-marker (point) t)))
                (setq sm/typst--edit-origin (cons (current-buffer) mkr))
                (with-current-buffer buf
                  (insert txt)              ; keep $ … $
                  (goto-char (point-min))
                  (typst-ts-mode)
                  (font-lock-ensure)
                  (sm/typst-equation-edit-mode 1))
                (switch-to-buffer buf))
            ;; ── plain call: insert immediately ────────────────────────
            (insert txt))))))


;;;;;;; references
  
  (defun sm/typst--label-alist ()
    "Return an alist of all Typst `label` nodes in the current buffer.
Each element is (DISPLAY . (BEG END)), where DISPLAY is the string shown
by Consult and BEG/END delimit the node."
    (let ((root (treesit-buffer-root-node)))
      (mapcar
       (lambda (m)
         (let* ((node (cdr m))
                (beg  (treesit-node-start node))
                (end  (treesit-node-end   node))
                ;; keep properties with `buffer-substring` 🡒 manual confirms
                ;; they are preserved  :contentReference[oaicite:0]{index=0}
                (txt  (buffer-substring beg end))
                (lab  (format "%4d: %s" (line-number-at-pos beg) txt)))
           (cons lab (list beg end))))        ; value is a *list* → no cdr traps
       (treesit-query-capture root '((label) @node)))))

;;; ───────────────────────── helpers ────────────────────────────────────────
  (defun sm/typst--label-alist ()
    "Return (DISPLAY . (BEG END ID)) for every *stand-alone* Typst label.

A label is considered stand-alone when **none** of its ancestors in the
Tree-sitter syntax tree has type \"code\" (that’s where macro calls live,
e.g. `#sidecite(<id>)`)."
    (let ((root (treesit-buffer-root-node)))
      (seq-mapcat
       (lambda (m)
         (let* ((node (cdr m)))
           ;; ❶ Skip if any ancestor is a code node
           (if (treesit-parent-until
                node (lambda (n) (equal (treesit-node-type n) "code")))
               nil                                ; inside macro → drop
             ;; ❷ Otherwise keep it
             (let* ((beg (treesit-node-start node))
                    (end (treesit-node-end   node))
                    (id  (buffer-substring beg end))
                    (lab (format "%4d: %s" (line-number-at-pos beg) id)))
               (list (cons lab (list beg end id)))))))
       (treesit-query-capture root '((label) @node)))))


  (defvar-local sm/typst--label-preview-ov nil
    "Overlay used for live label preview in Consult.")

  (defun sm/typst--label-preview-state (alist)
    "Return a state fn for Consult that previews & highlights labels from ALIST."
    (lambda (action cand)
      ;; clear previous overlay
      (when sm/typst--label-preview-ov
        (delete-overlay sm/typst--label-preview-ov)
        (setq sm/typst--label-preview-ov nil))
      ;; during preview: jump & highlight
      (when (and cand (eq action 'preview))
        (cl-destructuring-bind (beg end _id) (cdr (assoc cand alist))
          (goto-char beg)
          (setq sm/typst--label-preview-ov (make-overlay beg end))
          (overlay-put sm/typst--label-preview-ov 'face 'highlight)
          (recenter)))))

;;; ───────────────────── command 1: jump to label ──────────────────────────
  (defun typst-consult-labels-jump ()
    "Consult list of labels → live preview → jump to chosen label."
    (interactive)
    (let* ((alist (sm/typst--label-alist)))
      (unless alist (user-error "No labels found"))
      (let* ((state (sm/typst--label-preview-state alist))
             (choice (consult--read alist
                                    :prompt "Jump label: "
                                    :require-match t
                                    :sort nil
                                    :category 'consult-location
                                    :state state)))
        (cl-destructuring-bind (beg end _id) (cdr (assoc choice alist))
          (goto-char beg)
          (pulse-momentary-highlight-region beg end)))))

;;; ─────────────────── command 2: insert #ref(label) ────────────────────────
  (defun typst-consult-insert-ref ()
    "Consult list of labels → live preview → insert `@label` at point
(without the enclosing angle brackets)."
    (interactive)
    (let* ((alist (sm/typst--label-alist)))
      (unless alist (user-error "No labels found"))
      (let* ((origin (point-marker))              ; save where to insert
             (state  (sm/typst--label-preview-state alist))
             (choice (consult--read alist
                                    :prompt "Insert ref: "
                                    :require-match t
                                    :sort nil
                                    :category 'consult-location
                                    :state state))
             (id-raw (nth 2 (cdr (assoc choice alist))))
             ;; remove exactly one leading '<' and one trailing '>'
             (id     (replace-regexp-in-string "^<\\|>$" "" id-raw)))
        (goto-char origin)
        (insert (format "@%s" id)))))
  )

(provide 'typst-consult)
