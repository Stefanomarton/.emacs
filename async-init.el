(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
(require 'cl-lib)

(add-to-list 'load-path "~/.config/emacs/lisp/org")
(defvar drive-folder "~/.marton-drive/")
(defvar notes-folder (concat drive-folder "notes"))

(load "org-export.el")
(load "org-attachments.el")
