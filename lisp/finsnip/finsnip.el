;;; finsnip.el --- Context-aware auto-expansion engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 finsnip contributors

;; Author: finsnip contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))
;; Keywords: convenience, snippets
;; URL: https://github.com/finsnip/finsnip

;;; Commentary:

;; High-performance, context-aware auto-expansion with extreme customizability.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'filenotify)

(defgroup finsnip nil
  "Context-aware auto-expansion engine."
  :group 'convenience
  :prefix "finsnip-")

;;; Trie Implementation (Internal)

(defun finsnip--trie-create ()
  "Create a new trie."
  (make-hash-table :test 'equal))

(defun finsnip--trie-insert (trie string value)
  "Insert STRING with VALUE into TRIE."
  (let ((node trie))
    (cl-loop for char across string
             do (let ((next (gethash char node)))
                  (unless next
                    (setq next (make-hash-table :test 'equal))
                    (puthash char next node))
                  (setq node next)))
    (puthash :value value node)))

(defun finsnip--trie-lookup-prefix (trie string)
  "Look up the longest prefix of STRING in TRIE.
Returns a cons cell (LENGTH . VALUE) for the longest matching prefix,
or nil if no match is found."
  (let ((node trie)
        (last-match nil)
        (depth 0))
    (cl-loop for char across string
             while node
             do (setq node (gethash char node))
             (setq depth (1+ depth))
             if (and node (gethash :value node))
             do (setq last-match (cons depth (gethash :value node))))
    last-match))

;;; Registry API

(defvar finsnip-trie (finsnip--trie-create)
  "Global registry of snippets, stored in a trie for suffix matching.
Triggers are stored reversed.")

(cl-defun finsnip-register-snippet (trigger body &optional context (auto t))
  "Register a snippet with TRIGGER, BODY and optional CONTEXT and AUTO flag.
TRIGGER is the string that triggers the expansion.
BODY is the expansion content (string or function).
CONTEXT is an optional predicate function.
AUTO is a boolean (default t) determining if the snippet expands automatically."
  (finsnip--trie-insert finsnip-trie (reverse trigger) (list body context auto)))

(defun finsnip-context-major-mode-p (modes)
  "Return t if current major mode is derived from any of MODES.
MODES can be a symbol or a list of symbols."
  (let ((mode-list (if (listp modes) modes (list modes))))
    (cl-some (lambda (m) (derived-mode-p m)) mode-list)))

(defun finsnip-load-json-file (path)
  "Load VS Code compatible snippets from JSON file at PATH.
Supports 'prefix', 'body', 'scope', 'context', and 'auto' fields."
  (let ((data (with-temp-buffer
                (insert-file-contents path)
                (json-parse-buffer :object-type 'hash-table))))
    (maphash (lambda (_name snippet)
               (let* ((prefix (gethash "prefix" snippet))
                      (body (gethash "body" snippet))
                      (scope (gethash "scope" snippet))
                      (auto (gethash "auto" snippet))
                      (prefixes (cond ((listp prefix) prefix)
                                      ((vectorp prefix) (append prefix nil))
                                      (t (list prefix))))
                      (body-str (cond ((listp body) (mapconcat #'identity body "\n"))
                                      ((vectorp body) (mapconcat #'identity body "\n"))
                                      (t body)))
                      (json-context (gethash "context" snippet))
                      (context (cond
                                ((and scope (stringp scope) (not (string-empty-p scope)))
                                 (let ((modes (mapcar (lambda (s)
                                                        (intern (concat (string-trim s) "-mode")))
                                                      (split-string scope ","))))
                                   (if (and (stringp json-context) (not (string-empty-p json-context)))
                                       (let ((pred (intern json-context)))
                                         (lambda () (and (finsnip-context-major-mode-p modes)
                                                         (funcall pred))))
                                     (lambda () (finsnip-context-major-mode-p modes)))))
                                ((and (stringp json-context) (not (string-empty-p json-context)))
                                 (intern json-context)))))
                 (dolist (p prefixes)
                   (let ((is-auto (not (memq auto '(nil :json-false)))))
                     (finsnip-register-snippet p body-str context is-auto)))))
             data)))

(defun finsnip-load-snippets-dir (dir)
  "Load all .json snippet files in DIR."
  (dolist (file (directory-files dir t "\\.json$"))
    (finsnip-load-json-file file)))

(defvar finsnip--watched-paths nil
  "List of paths already being watched.")

(defvar finsnip--reload-timers (make-hash-table :test 'equal)
  "Timers for debouncing reloads per file.")

(defun finsnip--file-watch-callback (event)
  "Callback for filenotify. EVENT is (DESCRIPTOR ACTION FILE)."
  (let* ((action (nth 1 event))
         (file (nth 2 event)))
    (when (and (string-match-p "\\.json$" file)
               (memq action '(changed created renamed)))
      ;; Debounce reload to avoid rapid-fire events (T-04-03)
      (let ((timer (gethash file finsnip--reload-timers)))
        (when timer (cancel-timer timer))
        (puthash file
                 (run-with-timer 0.5 nil
                                 (lambda (f)
                                   (remhash f finsnip--reload-timers)
                                   (when (file-exists-p f)
                                     (finsnip-load-json-file f)))
                                 file)
                 finsnip--reload-timers)))))

(defun finsnip-watch-snippets (path)
  "Add a file/directory watcher for PATH.
When a .json file is changed/created, it will be automatically reloaded."
  (let ((abs-path (expand-file-name path)))
    (unless (member abs-path finsnip--watched-paths)
      (file-notify-add-watch abs-path '(change) #'finsnip--file-watch-callback)
      (push abs-path finsnip--watched-paths))))

(defun finsnip--match-suffix (string)
  "Find the longest snippet trigger matching the suffix of STRING.
Returns a cons cell (TRIGGER-LENGTH . (BODY CONTEXT AUTO)) or nil."
  (finsnip--trie-lookup-prefix finsnip-trie (reverse string)))

;;; Expansion Logic

(defvar-local finsnip--in-expansion-p nil
  "Internal flag to prevent recursive snippet expansion.")

(defvar-local finsnip--in-mirroring-p nil
  "Internal flag to prevent mirroring loops.")

(defun finsnip--expand (trigger-len body)
  "Expand TRIGGER-LEN chars into BODY.
Deletes TRIGGER-LEN characters preceding point and inserts BODY.
Wraps the expansion in a recursion guard."
  (let ((finsnip--in-expansion-p t)
        (real-body (if (functionp body) (funcall body) body)))
    (let ((beg (- (point) trigger-len))
          (end (point)))
      (delete-region beg end)
      (finsnip--insert-and-overlay (finsnip--parse-body real-body)))))

(defun finsnip--check-expansion ()
  "Check for snippet expansion at point."
  (unless (or finsnip--in-expansion-p finsnip--in-mirroring-p undo-in-progress)
    (let* ((limit (max (point-min) (- (point) 100))) ;; Arbitrary limit for suffix check
           (suffix (buffer-substring-no-properties limit (point)))
           (match (finsnip--match-suffix suffix)))
      (when match
        (let* ((trigger-len (car match))
               (data (cdr match))
               (body (nth 0 data))
               (context (nth 1 data))
               (auto (nth 2 data)))
          (when (and auto
                     (or (null context)
                         (funcall context)))
            (finsnip--expand trigger-len body)))))))

(defvar-local finsnip--pending-mirror-info nil
  "Information about a pending mirroring operation to be performed in post-command-hook.")

(defvar-local finsnip--pending-expansion-check nil
  "Flag to check for expansion in post-command-hook.")

(defun finsnip--after-change-handler (beg end _len)
  "After-change hook for snippet expansion and mirroring.
Only records info; modifications are deferred to `finsnip--post-command-handler`."
  (unless (or finsnip--in-expansion-p undo-in-progress)
    ;; Record info for mirroring
    (when (and finsnip-active-fields (not finsnip--in-mirroring-p))
      (let ((ov (cl-find-if (lambda (o)
                             (and (overlay-get o 'finsnip-id)
                                  (<= (overlay-start o) beg)
                                  (>= (overlay-end o) end)))
                           (overlays-in (max (point-min) (1- beg))
                                        (min (point-max) (1+ end))))))
        (when ov
          (setq finsnip--pending-mirror-info 
                (list ov (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))))))
    ;; Set flag for expansion check
    (setq finsnip--pending-expansion-check t)))

(defun finsnip--post-command-handler ()
  "Handle deferred buffer modifications after the command has finished."
  (save-match-data
    ;; 1. Handle Mirroring
    (when (and finsnip--pending-mirror-info (not undo-in-progress))
      (let* ((info finsnip--pending-mirror-info)
             (ov (car info))
             (content (cadr info))
             (id (overlay-get ov 'finsnip-id))
             (finsnip--in-mirroring-p t))
        (setq finsnip--pending-mirror-info nil)
        (dolist (other finsnip-active-fields)
          (when (and (not (eq other ov))
                     (overlay-buffer other) ;; Ensure overlay is still valid
                     (equal (overlay-get other 'finsnip-id) id))
            (save-excursion
              (goto-char (overlay-start other))
              (delete-region (overlay-start other) (overlay-end other))
              (insert content))))))
    
    ;; 2. Handle Expansion
    (when (and finsnip--pending-expansion-check (not undo-in-progress))
      (setq finsnip--pending-expansion-check nil)
      (finsnip--check-expansion))))

;;; Syntax Predicates

(defun finsnip-context-not-in-comment-p ()
  "Return t if point is not in a comment.
Uses `syntax-ppss` for efficient syntax checking."
  (not (nth 4 (syntax-ppss))))

(defun finsnip-context-not-in-string-p ()
  "Return t if point is not in a string.
Uses `syntax-ppss` for efficient syntax checking."
  (not (nth 3 (syntax-ppss))))

;;; Tree-sitter Context

(defun finsnip-context-in-node-p (types)
  "Return t if the node at point matches any of TYPES.
TYPES can be a single symbol or a list of symbols/strings.
If Tree-sitter is unavailable, returns nil."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (let ((node (treesit-node-at (point))))
      (when node
        (let ((node-type (treesit-node-type node))
              (type-list (if (listp types) types (list types))))
          (cl-member (if (symbolp node-type) (symbol-name node-type) node-type)
                     type-list
                     :test (lambda (a b)
                             (string-equal (if (symbolp a) (symbol-name a) a)
                                           (if (symbolp b) (symbol-name b) b)))))))))

(defun finsnip-context-inside-node-p (types)
  "Return t if any ancestor of the node at point matches any of TYPES.
TYPES can be a single symbol or a list of symbols/strings.
If Tree-sitter is unavailable, returns nil."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (let ((node (treesit-node-at (point)))
          (type-list (if (listp types) types (list types)))
          (found nil))
      (while (and node (not found))
        (setq node (treesit-node-parent node))
        (when node
          (let ((node-type (treesit-node-type node)))
            (when (cl-member (if (symbolp node-type) (symbol-name node-type) node-type)
                             type-list
                             :test (lambda (a b)
                                     (string-equal (if (symbolp a) (symbol-name a) a)
                                                   (if (symbolp b) (symbol-name b) b))))
              (setq found t)))))
      found)))

(defun finsnip-typst-in-math-p ()
  "Return t if point is inside a Typst math node.
Requires `typst-ts-mode` and available tree-sitter grammars."
  (finsnip-context-inside-node-p '("math" "math_group" "equation")))

;;; Snippet Body Parsing

(defun finsnip--parse-body (body)
  "Parse snippet BODY into a list of strings and field descriptors.
A field descriptor is (ID DEFAULT-ELEMENTS).
Supports recursive parsing for nested placeholders.
Escape \\$ as literal $."
  (let ((pos 0)
        (len (length body)))
    (cl-labels 
        ((parse-until (end-char)
           (let ((result nil)
                 (current-str ""))
             (while (and (< pos len)
                         (not (and end-char 
                                   (char-equal (aref body pos) end-char))))
               (let ((char (aref body pos)))
                 (cond
                  ;; Escape \$
                  ((and (char-equal char ?\\)
                        (< (1+ pos) len)
                        (char-equal (aref body (1+ pos)) ?$))
                   (setq current-str (concat current-str "$"))
                   (setq pos (+ pos 2)))
                  ;; Placeholder $N or ${N...}
                  ((char-equal char ?$)
                   (setq pos (1+ pos))
                   (unless (string-equal current-str "")
                     (push current-str result)
                     (setq current-str ""))
                   (if (and (< pos len) (char-equal (aref body pos) ?{))
                       ;; Complex ${N:default}
                       (progn
                         (setq pos (1+ pos))
                         (let ((id-start pos))
                           (while (and (< pos len) 
                                       (not (memq (aref body pos) '(?} ?:))))
                             (setq pos (1+ pos)))
                           (let ((id (string-to-number (substring body id-start pos))))
                             (if (and (< pos len) (char-equal (aref body pos) ?:))
                                 (progn
                                   (setq pos (1+ pos))
                                   (push (list id (parse-until ?})) result)
                                   (setq pos (1+ pos)))
                               (push (list id nil) result)
                               (when (and (< pos len) (char-equal (aref body pos) ?}))
                                 (setq pos (1+ pos)))))))
                     ;; Simple $N
                     (let ((id-start pos))
                       (while (and (< pos len)
                                   (>= (aref body pos) ?0)
                                   (<= (aref body pos) ?9))
                         (setq pos (1+ pos)))
                       (push (list (string-to-number (substring body id-start pos)) nil) result))))
                  (t
                   (setq current-str (concat current-str (char-to-string char)))
                   (setq pos (1+ pos))))))
             (unless (string-equal current-str "")
               (push current-str result))
             (nreverse result))))
      (parse-until nil))))

;;; Navigation and Field Management

(defface finsnip-field-face
  '((t :inherit secondary-selection))
  "Face used for snippet fields.")

(defvar-local finsnip-active-fields nil
  "List of active field overlays for the current snippet.")

(defvar-local finsnip--master-overlay nil
  "Overlay covering the entire active snippet.")

(defvar-local finsnip--field-initial-p nil
  "Internal flag: t if a field has just been entered and is in its initial state.")

(defun finsnip-exit-snippet ()
  "Clear all fields and exit the current snippet."
  (interactive)
  (remove-hook 'post-command-hook #'finsnip--check-exit-on-move t)
  (remove-hook 'pre-command-hook #'finsnip--handle-pre-command t)
  (dolist (ov finsnip-active-fields)
    (delete-overlay ov))
  (setq finsnip-active-fields nil)
  (setq finsnip--field-initial-p nil)
  (deactivate-mark)
  (when finsnip--master-overlay
    (delete-overlay finsnip--master-overlay)
    (setq finsnip--master-overlay nil)))

(defun finsnip--check-exit-on-move ()
  "Exit snippet if point moves outside of `finsnip--master-overlay`."
  (when (and (not (or finsnip--in-mirroring-p finsnip--in-expansion-p))
             finsnip--master-overlay)
    (unless (and (>= (point) (overlay-start finsnip--master-overlay))
                 (<= (point) (overlay-end finsnip--master-overlay)))
      (finsnip-exit-snippet))))

(defun finsnip--goto-field (id)
  "Move point to the first field overlay with ID and select its content.
Sets the mark at the end of the field and activates it, so that
`delete-selection-mode` (if enabled) or standard typing can overwrite it."
  (let ((ov (cl-find-if (lambda (o) (= (overlay-get o 'finsnip-id) id))
                       finsnip-active-fields)))
    (when ov
      (goto-char (overlay-start ov))
      ;; Set the mark at the end of the field and activate it
      (set-mark (overlay-end ov))
      (activate-mark)
      ;; Ensure the next self-insert command deletes the selection
      ;; This makes it work like a real snippet engine
      (setq-local finsnip--field-initial-p t))))

(defun finsnip--handle-pre-command ()
  "Handle pre-command logic for snippet fields.
If we are at the start of a field and it's in its initial state,
the next self-insert command should clear the field."
  (when finsnip--field-initial-p
    (let ((is-insert (memq this-command '(self-insert-command 
                                          org-self-insert-command 
                                          newline 
                                          newline-and-indent))))
      (when is-insert
        (let ((ov (cl-find-if (lambda (o)
                               (and (overlay-get o 'finsnip-id)
                                    (>= (point) (overlay-start o))
                                    (< (point) (overlay-end o))))
                             finsnip-active-fields)))
          (when (and ov (= (point) (overlay-start ov)))
            (let ((finsnip--in-mirroring-p t)
                  ;; Inhibit modification hooks to prevent external packages (like goggles)
                  ;; from interfering with the deletion of the placeholder.
                  (inhibit-modification-hooks t))
              (delete-region (overlay-start ov) (overlay-end ov))))))
      ;; Reset initial state unless it's a navigation command
      (unless (memq this-command '(finsnip-next-field 
                                   finsnip-prev-field 
                                   finsnip-exit-snippet))
        (setq-local finsnip--field-initial-p nil)))))

(defun finsnip-next-field ()
  "Move to the next field."
  (interactive)
  (let* ((ids (sort (cl-remove-duplicates 
                     (cl-remove nil (mapcar (lambda (ov) (overlay-get ov 'finsnip-id))
                                            finsnip-active-fields))
                     :test #'=)
                    #'<))
         (current-ov (cl-find-if (lambda (o)
                                   (and (>= (point) (overlay-start o))
                                        (<= (point) (overlay-end o))))
                                 finsnip-active-fields))
         (current-id (if current-ov (overlay-get current-ov 'finsnip-id) (car ids)))
         (pos (when (and current-id ids) (cl-position current-id ids :test #'=)))
         (next-idx (if pos (1+ pos) 1)))
    (if (or (null ids) (>= next-idx (length ids)))
        (finsnip-exit-snippet)
      (finsnip--goto-field (nth next-idx ids)))))

(defun finsnip-prev-field ()
  "Move to the previous field."
  (interactive)
  (let* ((ids (sort (cl-remove-duplicates 
                     (cl-remove nil (mapcar (lambda (ov) (overlay-get ov 'finsnip-id))
                                            finsnip-active-fields))
                     :test #'=)
                    #'<))
         (current-ov (cl-find-if (lambda (o)
                                   (and (>= (point) (overlay-start o))
                                        (<= (point) (overlay-end o))))
                                 finsnip-active-fields))
         (current-id (if current-ov (overlay-get current-ov 'finsnip-id) (car ids)))
         (pos (when (and current-id ids) (cl-position current-id ids :test #'=)))
         (prev-idx (if pos (1- pos) -1)))
    (when (and ids (>= prev-idx 0))
      (finsnip--goto-field (nth prev-idx ids)))))

(defvar finsnip-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'finsnip-next-field)
    (define-key map (kbd "<tab>") #'finsnip-next-field)
    (define-key map (kbd "<backtab>") #'finsnip-prev-field)
    (define-key map (kbd "S-TAB") #'finsnip-prev-field)
    (define-key map (kbd "C-g") #'finsnip-exit-snippet)
    map)
  "Keymap active when a snippet is being edited.")

(defun finsnip--insert-and-overlay (elements)
  "Insert ELEMENTS and create overlays for fields.
ELEMENTS is a list of strings and (ID DEFAULT-ELEMENTS) lists.
Populates `finsnip-active-fields`."
  (finsnip-exit-snippet)
  (let ((start-p (point))
        (positions nil))
    (cl-labels ((process (elems)
                  (dolist (el elems)
                    (cond
                     ((stringp el)
                      (insert el))
                     ((listp el)
                      (let* ((id (car el))
                             (default-elems (cadr el))
                             (start (point)))
                        (when default-elems (process default-elems))
                        (push (list id start (point) default-elems) positions)))))))
      (process elements)
      (setq finsnip--master-overlay (make-overlay start-p (point) nil nil t)))
    (dolist (pos (nreverse positions))
      (let* ((id (car pos))
             (start (nth 1 pos))
             (end (nth 2 pos))
             (default (nth 3 pos))
             (ov (make-overlay start end nil nil t)))
        (overlay-put ov 'face 'finsnip-field-face)
        (overlay-put ov 'finsnip-id id)
        (overlay-put ov 'finsnip-default default)
        (push ov finsnip-active-fields))))
  (setq finsnip-active-fields (nreverse finsnip-active-fields)) ;; Keep order
  (when finsnip-active-fields
    (add-hook 'post-command-hook #'finsnip--check-exit-on-move nil t)
    (add-hook 'pre-command-hook #'finsnip--handle-pre-command nil t)
    (set-transient-map finsnip-field-map (lambda () finsnip--master-overlay))
    (finsnip--goto-field (apply #'min (mapcar (lambda (ov) (overlay-get ov 'finsnip-id))
                                             finsnip-active-fields)))))

;;; CAPF Integration

(defun finsnip--get-all-triggers ()
  "Return a list of all registered triggers."
  (let ((triggers nil))
    (cl-labels ((collect (node prefix)
                  (maphash (lambda (key value)
                             (if (eq key :value)
                                 (push (reverse prefix) triggers)
                               (unless (eq key :value)
                                 (collect value (concat prefix (char-to-string key))))))
                           node)))
      (collect finsnip-trie ""))
    triggers))

(defun finsnip-capf ()
  "Completion-at-point function (Capf) for finsnip snippets."
  (let* ((end (point))
         (beg (save-excursion
                (skip-chars-backward "-a-zA-Z0-9._;")
                (point)))
         (prefix (buffer-substring-no-properties beg end)))
    (when (> (length prefix) 0)
      (let* ((all-triggers (finsnip--get-all-triggers))
             (candidates (cl-remove-if-not
                          (lambda (trigger)
                            (let ((context (nth 2 (finsnip--match-suffix trigger))))
                              (or (null context) (funcall context))))
                          (all-completions prefix all-triggers))))
        (when candidates
          (list beg end candidates
                :annotation-function
                (lambda (trigger)
                  (let ((match (finsnip--match-suffix trigger)))
                    (if match
                        (let* ((data (cdr match))
                               (body (nth 0 data))
                               (auto (nth 2 data)))
                          (concat (if auto " [auto]" " [man]")
                                  "  " 
                                  (if (functionp body) "[dynamic]" 
                                    (car (split-string body "\n")))))
                      "")))
                :exit-function
                (lambda (trigger status)
                  (when (memq status '(finished solo))
                    (let* ((len (length trigger))
                           (limit (max (point-min) (- (point) len)))
                           (actual-suffix (buffer-substring-no-properties limit (point))))
                      (when (string-equal actual-suffix trigger)
                        (let ((match (finsnip--match-suffix trigger)))
                          (when match
                            (finsnip--expand (car match) (car (cdr match)))))))))
                :company-kind (lambda (_) 'snippet)))))))

;;; Minor Mode

;;;###autoload
(define-minor-mode finsnip-mode
  "Minor mode for context-aware auto-expansion."
  :lighter " Fin"
  (if finsnip-mode
      (progn
        (add-hook 'after-change-functions #'finsnip--after-change-handler nil t)
        (add-hook 'post-command-hook #'finsnip--post-command-handler nil t)
        (add-hook 'completion-at-point-functions #'finsnip-capf nil t))
    (remove-hook 'after-change-functions #'finsnip--after-change-handler t)
    (remove-hook 'post-command-hook #'finsnip--post-command-handler t)
    (remove-hook 'completion-at-point-functions #'finsnip-capf t)))

(provide 'finsnip)
;;; finsnip.el ends here
