;;; modeline.el --- Minimal Modeline -*- lexical-binding: t; -*-
(setq-default
 mode-line-format
 '(
   (" ")
   (:eval (propertize (concat "" (symbol-name major-mode) "") 'face 'bold))
   (" ")
   (:eval
    (if (buffer-modified-p)
        (concat (propertize "~" 'face 'font-lock-keyword-face) " modified")
      (concat (propertize "" 'face 'font-lock-keyword-face) ""))) 

   ;; right aligned stuff
   (:eval
    (let* ((status-offset 12))
      (concat
       (concat 
	    (propertize " " 'display `(space :align-to (- right ,status-offset)))
        )
       (if (buffer-modified-p) 
           (concat (propertize "" 'face 'font-lock-keyword-face) "[modified]")
         (concat (propertize "" 'face 'font-lock-keyword-face) ""))
       ;; (propertize (format-time-string " %H:%M") 'face 'font-lock-keyword-face)
       
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
