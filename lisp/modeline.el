;;; modeline.el --- Minimal Modeline -*- lexical-binding: t; -*-

(defun my-mode-line--file-name ()
  "Return propertize name of ´my-mode-line-file-name´"
  (capitalize (shorten-directory default-directory 35)))

(defvar-local my-mode-line-file-name
    '(:eval (propertize my-mode-line--file-name 'face 'font-lock-comment-face))
  "Mode-line file name ")

(put 'my-mode-line--file-name 'risky-local-variable t)

(defun special-buffer-p (buffer-name)
  "Check if buffer-name is the name of a special buffer."
  (or (string-match-p "^\\*.+\\*$" buffer-name)
      ;; workaround for magit's 'trailing asterisk' problem
      ;; https://github.com/magit/magit/issues/2334
      (string-match-p "^\\*magit.*:.+$" buffer-name)))

;; helper function
;; stolen from: http://amitp.blogspot.se/2011/08/emacs-custom-mode-line.html
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

'(:eval (propertize
         " " 'display
         `((space :align-to (- (+ right right-fringe right-margin)
			                   ,(+ 3 (string-width mode-name)))))))

(setq-default
 mode-line-format
 '(
   ("%e") ;; print memory error
   ;; point position
   (:propertize "   " 'face 'font-lock-keyword-face)
   (8
    (:propertize " %l" face font-lock-string-face)
    )

   ;; right aligned stuff
   (:eval
    (let* ((status-offset 10))
      (concat
       ;; nyan-cat
       (concat
	    (propertize " " 'display `(space :align-to (- right ,status-offset)))
	    )
       (propertize (format-time-string " %H:%M") 'face 'font-lock-keyword-face)))))


 ;; read-only / changed
 ;; (propertize " " 'display `(space :align-to (- right ,status-offset)))
 ;; (cond (buffer-read-only
 ;;        (propertize "RO" 'face 'eshell-prompt))
 ;;       ((buffer-modified-p)
 ;;        (propertize "* " 'face 'eshell-prompt))
 ;;       (t "  ")))


 ;; (setq-default mode-line-format nil)
 ;; (setq mode-line-format nil)

 (provide 'modeline)

;;; modeline.el ends here
