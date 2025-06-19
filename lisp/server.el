;;; server.el -*- lexical-binding: t; -*-;;;

(when (daemonp)
  (add-hook
   'after-init-hook
   (defun my/load-packages-eagerly ()
     (run-at-time 1 nil
                  (lambda ()
                    (when (featurep 'straight) (straight-check-all))
                    (when (featurep 'pdf-tools) (pdf-tools-install t))
                    (load-library "pulse")
                    (when (string-suffix-p "server" server-name)
                      (let ((after-init-time (current-time)))
                        (dolist (lib '("org" "ob" "ox" "ol" "org-roam"
                                       "org-capture" "org-agenda" "org-fragtog"
                                       "org-gcal" "latex" "reftex" "cdlatex"
                                       "consult" "helpful" "elisp-mode"
                                       "notmuch" "elfeed" "simple"
                                       "expand-region" "embrace"
                                       "ace-window" "avy" "yasnippet"
                                       "magit" "modus-themes" "diff-hl"
                                       "dired" "ibuffer" "pdf-tools"
                                       "emacs-wm"))
                          (with-demoted-errors "Error: %S" (load-library lib)))
                        (let ((elapsed (float-time (time-subtract (current-time)
                                                                  after-init-time))))
                          (message "[Pre-loaded packages in %.3fs]" elapsed)))))))))


(provide 'server)
