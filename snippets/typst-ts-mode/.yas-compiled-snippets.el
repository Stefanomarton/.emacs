;;; "Compiled" snippets and support files for `typst-ts-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'typst-ts-mode
                     '(("quote"
                        "#quote(attribution: [$1])[\n$2\n]\n$0"
                        "quote" nil nil nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/quote"
                        nil nil)
                       ("lorem" "#lorem($1)$0" "lorem" nil nil nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/lorem"
                        nil nil)
                       ("//" "($1)/($2)" "frac" nil nil nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/frac"
                        nil nil)
                       ("wideblock"
                        "#wideblock(\n$0\ndouble: true,\n)"
                        "wideblock" nil nil nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/figure"
                        nil nil)
                       ("equation" "$\n$0\n$" "equation" nil nil nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/equation"
                        nil nil)
                       ("align" "#align($1)[\n$2\n]$0" "align" nil nil
                        nil
                        "/home/stefanom/.config/emacs/snippets/typst-ts-mode/align"
                        nil nil)))


;;; Do not edit! File generated at Tue Jun 17 17:26:33 2025
