(defun embrace-with-typst-command ()
  "Return a (#command( . )) cons for Embrace, prompting for the command name."
  (let ((cmd (read-string "Typst command: ")))   ; no completion/annotation
    (cons (format "#%s(" (downcase cmd)) ")")))


(defun typst-surround-setup ()
  (setq embrace--pairs-list nil)
  (embrace-add-pair ?m "$" "$" "Inline Math" nil)
  (embrace-add-pair ?M "$" "$" "Block Math" t)
  (embrace-add-pair ?f "#figure(" ")" "Figure" t)
  (embrace-add-pair ?w "#wideblock(" ")" "Wideblock" t)
  (embrace-add-pair ?w "#table(" ")" "Table" t)
  (embrace-add-pair ?p "(" ")" "(" t)
  (embrace-add-pair ?s "[" "]" "[" t)
  (embrace-add-pair ?c "{" "}" "{" t)
  (embrace-add-pair-regexp
   ?c
   "#[A-Za-z0-9_]+("
   ")"
   'embrace-with-typst-command
   (embrace-build-help "#command(" ")")
   t)
  )

(add-hook 'typst-ts-mode-hook 'typst-surround-setup)

(provide 'typst-surround)

