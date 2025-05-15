;;; org-navigation.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package org-side-tree
  :commands org-side-tree)

(defvar my/org-mode-navigation-map (make-sparse-keymap)
  "Keymap for `my/org-mode-navigation`.")

;; Add your functions to the navigation map for keybindings
(define-key my/org-mode-navigation-map (kbd "<escape> n n") 'my/org-narrow-to-next-level-1)
(define-key my/org-mode-navigation-map (kbd "<escape> n p") 'my/org-narrow-to-previous-level-1)
(define-key my/org-mode-navigation-map (kbd "<escape> n m") 'my/org-narrow-to-level-1-parent-and-move)

(define-minor-mode my/org-mode-navigation
  "Minor mode for efficient Org-mode navigation with narrowing."
  :init-value nil
  :lighter " OrgNav"
  :keymap my/org-mode-navigation-map

  (if my/org-mode-navigation
      (progn
        ;; When the mode is activated
        (org-side-tree)  ;; Activate org-side-tree
        (other-window 1)
        (message "Org Navigation Mode activated!"))

    ;; When the mode is deactivated
    (org-side-tree-toggle)  ;; Deactivate org-side-tree
    (message "Org Navigation Mode deactivated!")))


(defun my/org-narrow-to-next-level-1 ()
  "Widen, then narrow to the next Org top-level heading. If no next heading, widen and show a message."
  (interactive)
  (widen)
  ;; If buffer was narrowed, move to end of narrowed region
  (when (buffer-narrowed-p)
    (goto-char (point-max)))
  ;; Move to next level-1 heading
  (outline-next-heading)
  (while (and (not (eobp))
              (not (= (org-current-level) 1)))
    (outline-next-heading))
  ;; If no next heading, widen and show message
  (if (or (eobp) (= (org-current-level) 0))
      (progn
        (widen)
        (message "No next level-1 heading found."))  ;; Show message if no next heading
    (progn
      ;; Narrow to level-1 heading if found
      (org-narrow-to-subtree)
      ;; Ensure visibility
      (org-fold-show-set-visibility (point-min) (point-max) t)))) ;; Update visibility

(defun my/org-narrow-to-previous-level-1 ()
  "Widen, then narrow to the previous Org top-level heading. If no previous heading, widen and show a message."
  (interactive)
  (widen)
  ;; If buffer was narrowed, move to beginning of narrowed region
  (when (buffer-narrowed-p)
    (goto-char (point-min)))
  ;; Move to previous level-1 heading
  (outline-previous-heading)
  (while (and (not (bobp))
              (not (= (org-current-level) 1)))
    (outline-previous-heading))
  ;; If no previous heading, widen and show message
  (if (or (bobp) (= (org-current-level) 0))
      (progn
        (widen)
        (message "No previous level-1 heading found."))  ;; Show message if no previous heading
    (progn
      ;; Narrow to level-1 heading if found
      (org-narrow-to-subtree)
      ;; Ensure visibility
      (org-fold-show-set-visibility (point-min) (point-max) t)))) ;; Update visibility


(defun my/org-narrow-to-level-1-parent-and-move ()
  "Use `consult-org-heading` to select a heading.
Narrow to the nearest level-1 heading (parent), then move to the selected heading."
  (interactive)
  (widen)
  (let ((marker (consult-org-heading)))
    (when marker
      (let ((target-pos (marker-position marker))
            level-1-pos)
        ;; Go to the selected heading first
        (goto-char target-pos)
        ;; Save position of the parent level-1 heading
        (save-excursion
          (org-back-to-heading)
          (while (> (org-current-level) 1)
            (outline-up-heading 1 t))
          (setq level-1-pos (point)))
        ;; Now narrow to level-1 heading
        (goto-char level-1-pos)
        (org-narrow-to-subtree)
        ;; Move to the originally selected heading
        (goto-char target-pos)
        (org-show-entry)
        (org-show-context)
        (recenter)
        (org-fold-show-set-visibility (point-min) (point-max) t)
        ))))

(provide 'org-navigation)

;;; org-navigation ends here
