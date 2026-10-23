;;; finsnip-tests.el --- Tests for finsnip -*- lexical-binding: t; -*-

(require 'ert)
(require 'finsnip)

(defun finsnip-test--type (&rest strings)
  "Insert STRINGS like a command would, then run `post-command-hook'."
  (apply #'insert strings)
  (run-hooks 'post-command-hook))

(ert-deftest finsnip-test-registry-and-matching ()
  "Test that snippets can be registered and retrieved via suffix matching."
  (let ((finsnip-trie (finsnip--trie-create))) ;; Use local trie for test
    ;; Case 1: Simple registration and matching
    (finsnip-register-snippet "fn." "function ")
    (should (equal (finsnip--match-suffix "let's try fn.") '(3 "function " nil t)))
    (should (equal (finsnip--match-suffix "fn.") '(3 "function " nil t)))
    (should (null (finsnip--match-suffix "fn")))

    ;; Case 2: Longest match
    (finsnip-register-snippet "main.fn." "main function ")
    (should (equal (finsnip--match-suffix "call main.fn.") '(8 "main function " nil t)))
    (should (equal (finsnip--match-suffix "just fn.") '(3 "function " nil t)))

    ;; Case 4: Context support
    (finsnip-register-snippet "if." "if " (lambda () t))
    (let ((match (finsnip--match-suffix "if.")))
      (should (equal (car match) 3))
      (should (equal (car (cdr match)) "if "))
      (should (functionp (nth 2 match))))

    ;; Case 5: No match
    (should (null (finsnip--match-suffix "some unrelated text.")))))

(ert-deftest finsnip-test-expansion ()
  "Test that snippets are correctly expanded and recursion guard works."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function ")
      (finsnip-test--type "some text fn.")
      ;; Move point back to after "fn." (it's already there)
      (finsnip--expand 3 "function ")
      (should (equal (buffer-string) "some text function "))
      (should (= (point) (point-max))))))

(ert-deftest finsnip-test-minor-mode-auto-expansion ()
  "Test that enabling `finsnip-mode` triggers expansion on typing."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function ")
      (finsnip-mode 1)
      (should (memq 'finsnip--post-command-handler post-command-hook))

      ;; Simulate typing "fn."
      (finsnip-test--type "f")
      (finsnip-test--type "n")
      (finsnip-test--type ".")
      ;; Expansion should have happened automatically
      (should (equal (buffer-string) "function "))
      (should (= (point) (point-max)))

      (finsnip-mode -1)
      (should-not (memq 'finsnip--post-command-handler post-command-hook)))))

(ert-deftest finsnip-test-backspace-resilience ()
  "Test that triggers work correctly even with backspaces in history."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function ")
      (finsnip-mode 1)
      
      ;; Typing: "f", "n", "backspace", "n", "."
      (finsnip-test--type "f")
      (finsnip-test--type "n")
      (delete-backward-char 1) (run-hooks 'post-command-hook) ;; Remove "n"
      (finsnip-test--type "n") ;; Re-type "n"
      (finsnip-test--type ".") ;; Type "."
      
      ;; Trigger "fn." should still work because buffer suffix is "fn."
      (should (equal (buffer-string) "function "))
      (should (= (point) (point-max))))))

(ert-deftest finsnip-test-context-aware-expansion ()
  "Test that expansion only occurs if the context predicate is satisfied."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create))
          (should-expand nil))
      (finsnip-register-snippet "if." "if " (lambda () should-expand))
      (finsnip-mode 1)
      
      ;; 1. Predicate returns nil
      (finsnip-test--type "if.")
      (should (equal (buffer-string) "if."))
      
      ;; 2. Predicate returns t
      (setq should-expand t)
      (delete-backward-char 3) (run-hooks 'post-command-hook)
      (finsnip-test--type "if.")
      (should (equal (buffer-string) "if "))
    )
  )
)

(ert-deftest finsnip-test-syntax-predicates ()
  "Test that syntax-based predicates correctly identify comment and string contexts."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Test 1: Outside comment/string
    (finsnip-test--type "some code ")
    (should (finsnip-context-not-in-comment-p))
    (should (finsnip-context-not-in-string-p))

    ;; Test 2: Inside string
    (finsnip-test--type "\"")
    (should (finsnip-context-not-in-comment-p))
    (should-not (finsnip-context-not-in-string-p))
    (finsnip-test--type "inside string\"")
    (should (finsnip-context-not-in-comment-p))
    (should (finsnip-context-not-in-string-p)) ;; After string

    ;; Test 3: Inside comment
    (finsnip-test--type " ; ")
    (should-not (finsnip-context-not-in-comment-p))
    (should (finsnip-context-not-in-string-p))))

(ert-deftest finsnip-test-treesit-node-checking ()
  "Test that `finsnip-context-in-node-p` correctly identifies node types."
  ;; Since we might not have grammars installed in batch mode, 
  ;; we mock `treesit-node-at` and `treesit-node-type`.
  (cl-letf* (((symbol-function 'treesit-available-p) (lambda () t))
             ((symbol-function 'treesit-node-at) (lambda (_pos) "mock-node"))
             ((symbol-function 'treesit-node-type) (lambda (node) 
                                                     (if (stringp node) 'function_definition 'unknown))))
    ;; 1. Single type
    (should (finsnip-context-in-node-p 'function_definition))
    (should-not (finsnip-context-in-node-p 'class_definition))

    ;; 2. List of types
    (should (finsnip-context-in-node-p '(function_definition class_definition)))
    (should (finsnip-context-in-node-p '("function_definition" "class_definition")))
    (should-not (finsnip-context-in-node-p '(class_definition if_statement))))

  ;; 3. No Tree-sitter available
  (cl-letf (((symbol-function 'treesit-available-p) (lambda () nil)))
    (should-not (finsnip-context-in-node-p 'function_definition))))

(ert-deftest finsnip-test-treesit-parent-checking ()
  "Test that `finsnip-context-inside-node-p` correctly identifies ancestors."
  (let* ((child "child")
         (parent "parent")
         (grandparent "grandparent"))
    (cl-letf* (((symbol-function 'treesit-available-p) (lambda () t))
               ((symbol-function 'treesit-node-at) (lambda (_pos) child))
               ((symbol-function 'treesit-node-parent) (lambda (node) 
                                                         (cond ((equal node child) parent)
                                                               ((equal node parent) grandparent)
                                                               (t nil))))
               ((symbol-function 'treesit-node-type) (lambda (node) 
                                                       (cond ((equal node child) 'child_type)
                                                             ((equal node parent) 'parent_type)
                                                             ((equal node grandparent) 'grandparent_type)
                                                             (t 'unknown)))))
      ;; 1. Direct parent
      (should (finsnip-context-inside-node-p 'parent_type))
      
      ;; 2. Grandparent
      (should (finsnip-context-inside-node-p 'grandparent_type))
      
      ;; 3. Multiple types
      (should (finsnip-context-inside-node-p '(parent_type unknown)))
      
      ;; 4. Not in ancestors
      (should-not (finsnip-context-inside-node-p 'other_type)))))

(ert-deftest finsnip-test-parse-body ()
  "Test that snippet bodies are correctly parsed into strings and fields."
  ;; 1. Simple text
  (should (equal (finsnip--parse-body "plain text")
                 '("plain text")))
  
  ;; 2. Simple placeholders
  (should (equal (finsnip--parse-body "fn $1($2)")
                 '("fn " (1 nil) "(" (2 nil) ")")))
  
  ;; 3. Braced placeholders
  (should (equal (finsnip--parse-body "fn ${1}(${2})")
                 '("fn " (1 nil) "(" (2 nil) ")")))
  
  ;; 4. Placeholders with default values
  (should (equal (finsnip--parse-body "fn ${1:name}(${2:args})")
                 '("fn " (1 ("name")) "(" (2 ("args")) ")")))
  
  ;; 5. Escape sequences
  (should (equal (finsnip--parse-body "price: \\$100, field: $1")
                 '("price: $100, field: " (1 nil))))
  
  ;; 6. Complex example
  (should (equal (finsnip--parse-body "if (${1:condition}) {\n  $2\n}")
                 '("if (" (1 ("condition")) ") {\n  " (2 nil) "\n}"))))

(ert-deftest finsnip-test-insert-and-overlay ()
  "Test that fields are correctly inserted and overlays are created."
  (with-temp-buffer
    (let ((finsnip-active-fields nil)
          (elements '("fn " (1 ("name")) "(" (2 nil) ")")))
      (finsnip--insert-and-overlay elements)
      (should (equal (buffer-string) "fn name()"))
      
      ;; Verify overlays
      (should (= (length finsnip-active-fields) 2))
      
      ;; Check first overlay (name)
      (let ((ov (cl-find 1 finsnip-active-fields :key (lambda (o) (overlay-get o 'finsnip-id)))))
        (should ov)
        (should (= (overlay-start ov) 4))
        (should (= (overlay-end ov) 8))
        (should (equal (overlay-get ov 'finsnip-default) '("name"))))
      
      ;; Check second overlay (empty)
      (let ((ov (cl-find 2 finsnip-active-fields :key (lambda (o) (overlay-get o 'finsnip-id)))))
        (should ov)
        (should (= (overlay-start ov) 9))
        (should (= (overlay-end ov) 9))
        (should-not (overlay-get ov 'finsnip-default))))))

(ert-deftest finsnip-test-expansion-with-placeholders ()
  "Test that expansion with placeholders correctly creates overlays."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}(${2:args})")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      (should (equal (buffer-string) "fn name(args)"))
      (should (= (length finsnip-active-fields) 2))
      (finsnip-mode -1))))

(ert-deftest finsnip-test-navigation ()
  "Test that TAB and Shift-TAB move point between fields."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}(${2:args})")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      
      ;; 1. Point should be at the start of the first field
      (should (equal (buffer-substring (point) (+ (point) 4)) "name"))
      
      ;; 2. TAB to next field
      (call-interactively #'finsnip-next-field)
      (should (equal (buffer-substring (point) (+ (point) 4)) "args"))
      
      ;; 3. Shift-TAB back to first field
      (call-interactively #'finsnip-prev-field)
      (should (equal (buffer-substring (point) (+ (point) 4)) "name"))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-navigation-exit ()
  "Test that TAB on the last field exits snippet mode."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}(${2:args})")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      
      (should finsnip-active-fields)
      
      ;; TAB once (to second field)
      (call-interactively #'finsnip-next-field)
      ;; TAB again (should exit)
      (call-interactively #'finsnip-next-field)
      
      (should-not finsnip-active-fields)
      (should-not (overlays-at (point-min))) ;; All overlays should be gone
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-mirroring ()
  "Test that modifying one field updates all other fields with the same ID."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}() { ${1:name}(); }")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      
      (should (equal (buffer-string) "fn name() { name(); }"))
      
      ;; 1. Point is at the start of the first "name"
      ;; Type "foo"
      (finsnip-test--type "foo")
      ;; delete "name"
      (delete-char 4) (run-hooks 'post-command-hook)
      
      ;; 2. Both fields should now be "foo"
      (should (equal (buffer-string) "fn foo() { foo(); }"))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-mirroring-internal ()
  "Test that modifying inside a field mirrors correctly."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}() { ${1:name}(); }")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      
      (forward-char 1)
      (finsnip-test--type "X")
      
      (should (equal (buffer-string) "fn nXame() { nXame(); }"))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-mirroring-start ()
  "Test that typing at the start of a mirrored field also works."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}() { ${1:name}(); }")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      
      ;; Point is at start of first "name"
      (finsnip-test--type "new")
      (should (equal (buffer-string) "fn newname() { newname(); }"))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-function-bodies ()
  "Test that snippets can have function bodies."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      ;; 1. Function body returning a string
      (finsnip-register-snippet "func." (lambda () "expanded-from-func"))
      (finsnip-mode 1)
      (finsnip-test--type "func.")
      (should (equal (buffer-string) "expanded-from-func"))
      (delete-region (point-min) (point-max))
      
      ;; 2. Function body returning a string with placeholders
      (finsnip-register-snippet "pfunc." (lambda () "fn ${1:name}()"))
      (finsnip-test--type "pfunc.")
      (should (equal (buffer-string) "fn name()"))
      (should (= (length finsnip-active-fields) 1))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-json-loading ()
  "Test that snippets can be loaded from VS Code compatible JSON files."
  (let ((json-path (make-temp-file "finsnip-test-" nil ".json"))
        (finsnip-trie (finsnip--trie-create)))
    (unwind-protect
        (progn
          ;; 1. Prepare JSON content
          (with-temp-file json-path
            (finsnip-test--type "{
  \"test-snippet\": {
    \"prefix\": \"tp.\",
    \"body\": [\"line1\", \"line2\"],
    \"description\": \"test description\"
  },
  \"multi-prefix\": {
    \"prefix\": [\"mp1.\", \"mp2.\"],
    \"body\": \"multi body\"
  }
}"))
          ;; 2. Load JSON
          (finsnip-load-json-file json-path)
          
          ;; 3. Verify registration
          (should (equal (car (cdr (finsnip--match-suffix "tp."))) "line1\nline2"))
          (should (equal (car (cdr (finsnip--match-suffix "mp1."))) "multi body"))
          (should (equal (car (cdr (finsnip--match-suffix "mp2."))) "multi body")))
      (delete-file json-path))))

(ert-deftest finsnip-test-dir-loading ()
  "Test that snippets can be loaded from all JSON files in a directory."
  (let ((temp-dir (make-temp-file "finsnip-test-dir" t))
        (finsnip-trie (finsnip--trie-create)))
    (unwind-protect
        (progn
          ;; Create two JSON files
          (with-temp-file (expand-file-name "one.json" temp-dir)
            (finsnip-test--type "{\"a\": {\"prefix\": \"a.\", \"body\": \"A\"}}"))
          (with-temp-file (expand-file-name "two.json" temp-dir)
            (finsnip-test--type "{\"b\": {\"prefix\": \"b.\", \"body\": \"B\"}}"))
          
          ;; Load directory
          (finsnip-load-snippets-dir temp-dir)
          
          ;; Verify
          (should (equal (car (cdr (finsnip--match-suffix "a."))) "A"))
          (should (equal (car (cdr (finsnip--match-suffix "b."))) "B")))
      (delete-directory temp-dir t))))

(ert-deftest finsnip-test-cancellation-c-g ()
  "Test that C-g cancels the snippet."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}()")
      (finsnip-mode 1)
      (finsnip-test--type "fn.")
      (should finsnip-active-fields)
      
      ;; Call C-g binding (finsnip-exit-snippet)
      (call-interactively #'finsnip-exit-snippet)
      (should-not finsnip-active-fields)
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-exit-on-move ()
  "Test that moving point outside the snippet region cancels it."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "fn ${1:name}(${2:args})")
      (finsnip-mode 1)
      (finsnip-test--type "   ") ;; Some space before
      (finsnip-test--type "fn.")
      (should finsnip-active-fields)
      
      ;; Moving point within fields range (should stay)
      (cl-letf (((symbol-function 'finsnip--in-mirroring-p) (lambda () nil)))
        (finsnip--check-exit-on-move))
      (should finsnip-active-fields)
      
      ;; Moving point far away (to start of buffer)
      (goto-char (point-min))
      (finsnip--check-exit-on-move)
      (should-not finsnip-active-fields)
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-nested-placeholders ()
  "Test that nested placeholders are correctly parsed and expanded."
  ;; 1. Parse check
  (should (equal (finsnip--parse-body "${1:outer ${2:inner}}")
                 '((1 ("outer " (2 ("inner")))))))
  
  ;; 2. Expansion check
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "nest." "${1:outer ${2:inner}}")
      (finsnip-mode 1)
      (finsnip-test--type "nest.")
      (should (equal (buffer-string) "outer inner"))
      (should (= (length finsnip-active-fields) 2))
      
      ;; Verify overlay ranges
      (let ((ov1 (cl-find 1 finsnip-active-fields :key (lambda (o) (overlay-get o 'finsnip-id))))
            (ov2 (cl-find 2 finsnip-active-fields :key (lambda (o) (overlay-get o 'finsnip-id)))))
        (should ov1)
        (should ov2)
        ;; ov1 should cover "outer inner"
        (should (= (overlay-start ov1) 1))
        (should (= (overlay-end ov1) 12))
        ;; ov2 should cover "inner"
        (should (= (overlay-start ov2) 7))
        (should (= (overlay-end ov2) 12)))
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-nested-navigation ()
  "Test that TAB navigation works correctly in nested placeholders."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "nest." "${1:outer ${2:inner}}")
      (finsnip-mode 1)
      (finsnip-test--type "nest.")
      
      ;; 1. Initially at ID 1 (outer)
      (should (equal (buffer-substring (point) (+ (point) 5)) "outer"))
      
      ;; 2. TAB to ID 2 (inner)
      (call-interactively #'finsnip-next-field)
      (should (equal (buffer-substring (point) (+ (point) 5)) "inner"))
      
      ;; 3. TAB to exit (since there are no more IDs)
      (call-interactively #'finsnip-next-field)
      (should-not finsnip-active-fields)
      
      (finsnip-mode -1))))

(ert-deftest finsnip-test-get-all-triggers ()
  "Test that `finsnip--get-all-triggers` correctly extracts triggers from the trie."
  (let ((finsnip-trie (finsnip--trie-create)))
    (finsnip-register-snippet "fn." "function")
    (finsnip-register-snippet "foo" "bar")
    (finsnip-register-snippet "if" "then")
    
    (let ((triggers (finsnip--get-all-triggers)))
      (should (member "fn." triggers))
      (should (member "foo" triggers))
      (should (member "if" triggers))
      (should (= (length triggers) 3)))))

(ert-deftest finsnip-test-capf-candidates ()
  "Test that `finsnip-capf` returns correct candidates for a prefix."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function")
      (finsnip-register-snippet "foo" "bar")
      (finsnip-register-snippet "bar" "baz")
      
      (finsnip-test--type "f")
      (let ((capf (finsnip-capf)))
        (should capf)
        (should (= (car capf) 1)) ;; Start
        (should (= (nth 1 capf) 2)) ;; End
        (let ((candidates (nth 2 capf)))
          (should (member "fn." candidates))
          (should (member "foo" candidates))
          (should-not (member "bar" candidates)))))))

(ert-deftest finsnip-test-capf-expansion ()
  "Test that selecting a candidate from `finsnip-capf` triggers expansion."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function")
      
      (finsnip-test--type "fn.")
      (let* ((capf (finsnip-capf))
             (props (nthcdr 3 capf))
             (exit-fn (plist-get props :exit-function)))
        (should exit-fn)
        ;; Simulate selecting "fn."
        ;; CAPF exit-function takes (candidate status)
        (funcall exit-fn "fn." 'finished)
        
        (should (equal (buffer-string) "function"))))))

(ert-deftest finsnip-test-capf-annotation ()
  "Test that `finsnip-capf` provides correct annotations."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function body\nsecond line")
      
      (finsnip-test--type "f")
      (let* ((capf (finsnip-capf))
             (props (nthcdr 3 capf))
             (ann-fn (plist-get props :annotation-function)))
        (should ann-fn)
        (should (equal (funcall ann-fn "fn.") " [auto]  function body"))))))

(ert-deftest finsnip-test-capf-prefix-and-context ()
  "Capf completes triggers containing ; _ - and hides out-of-context ones."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet ";al" "alpha")
      (finsnip-register-snippet ";ax" "hidden" (lambda () nil))
      (insert "x ;a")
      (let ((capf (finsnip-capf)))
        (should (= (nth 0 capf) 3))
        (should (equal (nth 2 capf) '(";al")))))))

(ert-deftest finsnip-test-capf-expansion-no-duplication ()
  "Test that using CAPF with `finsnip-mode` active doesn't cause double expansion."
  (with-temp-buffer
    (let ((finsnip-trie (finsnip--trie-create)))
      (finsnip-register-snippet "fn." "function")
      (finsnip-mode 1)
      
      (finsnip-test--type "fn")
      (let* ((capf (finsnip-capf))
             (exit-fn (plist-get (nthcdr 3 capf) :exit-function)))
        (finsnip-test--type ".") ;; Triggers auto-expansion
        (funcall exit-fn "fn." 'finished)
        
        (should (equal (buffer-string) "function"))
        (finsnip-mode -1)))))

(ert-deftest finsnip-test-mode-registers-capf ()
  "finsnip-mode adds `finsnip-capf' to the buffer's own capf list and removes it."
  (with-temp-buffer
    (text-mode)
    (finsnip-mode 1)
    (should (memq #'finsnip-capf completion-at-point-functions))
    (should (local-variable-p 'completion-at-point-functions))
    (finsnip-mode -1)
    (should-not (memq #'finsnip-capf completion-at-point-functions))))

(ert-deftest finsnip-test-file-watching ()
  "Test that snippets are automatically reloaded when a JSON file changes."
  (let ((json-path (make-temp-file "finsnip-test-watch-" nil ".json"))
        ;; Use a local trie for testing by binding the global one
        (finsnip-trie (finsnip--trie-create))
        (finsnip--watched-files nil))
    (unwind-protect
        (progn
          ;; Initial load (simulated)
          (with-temp-file json-path
            (finsnip-test--type "{\"test\": {\"prefix\": \"v1.\", \"body\": \"V1\"}}"))
          
          ;; 1. Register watch
          ;; We might need to mock file-notify-add-watch to avoid actual OS calls
          (cl-letf (((symbol-function 'file-notify-add-watch) (lambda (_f _e _c) 1)))
            (finsnip-watch-snippets json-path))
          
          ;; 2. Manually trigger callback
          (cl-letf (((symbol-function 'run-with-timer) (lambda (_s _r f &rest args) 
                                                         (apply f args)
                                                         nil)))
            (finsnip--file-watch-callback `(1 changed ,json-path)))
          
          ;; 3. Verify loaded
          (should (equal (car (cdr (finsnip--match-suffix "v1."))) "V1"))
          
          ;; 4. Update content and trigger again
          (with-temp-file json-path
            (finsnip-test--type "{\"test\": {\"prefix\": \"v2.\", \"body\": \"V2\"}}"))
          (cl-letf (((symbol-function 'run-with-timer) (lambda (_s _r f &rest args) 
                                                         (apply f args)
                                                         nil)))
            (finsnip--file-watch-callback `(1 changed ,json-path)))
          
          ;; 5. Verify updated
          (should (equal (car (cdr (finsnip--match-suffix "v2."))) "V2")))
      (delete-file json-path))))

(provide 'finsnip-tests)
;;; finsnip-tests.el ends here
