;;; modeline.el --- Minimal Modeline -*- lexical-binding: t; -*-
(setq-default
 mode-line-format
 '(
   (" ")
   (:eval (symbol-name major-mode))
   (" ")
   (:eval (if (buffer-modified-p) "~  modified" "~ saved"))

   ;; right aligned stuff
   (:eval
    (let* ((status-offset 10))
      (concat
       (concat
	    (propertize " " 'display `(space :align-to (- right ,status-offset)))
        )
       (propertize (format-time-string " %H:%M") 'face 'font-lock-keyword-face)
       )))))


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
