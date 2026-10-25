;;; dettatura.el --- Popup per rivedere una dettatura prima di incollarla -*- lexical-binding: t; -*-

;; Aperto da ~/.config/hypr/scripts/dettatura-emacs.sh (Super+Alt+E): hyprwhspr-rs
;; incolla la trascrizione qui, la si corregge (o la si fa riformulare a Gemma
;; sull'homelab), poi C-c C-c la incolla nella finestra di partenza. Ctrl+Enter
;; negli appunti (Super+V) apre qui una voce della cronologia, da sistemare.

(defvar-local dettatura--window nil "Indirizzo Hyprland della finestra di partenza.")
(defvar-local dettatura--class nil "Classe della finestra di partenza (terminale o no).")

(defvar-keymap dettatura-mode-map
  "C-c C-c" #'dettatura-invia
  "C-c C-k" #'dettatura-annulla
  "C-c r"   #'dettatura-riformula)

(define-minor-mode dettatura-mode
  "Popup di dettatura: C-c C-c incolla, C-c C-k annulla, C-c r riformula."
  :lighter " Dettatura")

(defun dettatura-apri (window class &optional file)
  "Prepara il buffer del popup per la finestra WINDOW di classe CLASS.
Con FILE (una voce degli appunti) parte dal suo testo e lo cancella."
  (switch-to-buffer (get-buffer-create "*dettatura*"))
  (erase-buffer)
  (when file
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (delete-file file))
  (text-mode)
  (visual-line-mode 1)
  (dettatura-mode 1)
  (setq dettatura--window window
        dettatura--class class)
  (setq header-line-format
        (concat (unless file " Super+Alt+D ferma ·")
                " C-c C-c incolla · C-c r riformula · C-c C-k annulla")))

(defun dettatura--chiudi ()
  (kill-buffer (current-buffer))
  (delete-frame))

(defun dettatura-invia ()
  "Copia il testo, chiude il popup e lo incolla nella finestra di partenza."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (window dettatura--window)
        (class dettatura--class))
    (call-process "wl-copy" nil nil nil "--" text)
    (dettatura--chiudi)
    (start-process "dettatura" nil (expand-file-name "~/.config/hypr/scripts/dettatura-emacs.sh")
                   "incolla" window class)))

(defun dettatura-annulla ()
  "Chiude il popup senza incollare."
  (interactive)
  (let ((window dettatura--window))
    (dettatura--chiudi)
    (call-process "hyprctl" nil 0 nil "dispatch"
                  (format "hl.dsp.focus({ window = 'address:%s' })" window))))

(defun dettatura-riformula ()
  "Fa riformulare il testo a Gemma (homelab) e lo sostituisce; C-/ per tornare indietro."
  (interactive)
  (require 'gptel)
  (let ((buf (current-buffer)))
    (message "Riformulo…")
    (gptel-request (buffer-string)
      :system "Riformula il testo dettato in italiano corretto e scorrevole. Mantieni significato, tono, persona, nomi, numeri e sigle. Rispondi solo con il testo riformulato, senza commenti né virgolette."
      :stream nil
      :callback (lambda (response info)
                  (if (stringp response)
                      (with-current-buffer buf
                        (atomic-change-group
                          (erase-buffer)
                          (insert (string-trim response)))
                        (message "Riformulato (C-/ per annullare)"))
                    (message "Riformulazione fallita: %s" (plist-get info :status)))))))

(provide 'dettatura)
;;; dettatura.el ends here
