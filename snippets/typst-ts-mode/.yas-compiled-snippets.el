;;; "Compiled" snippets and support files for `typst-ts-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'typst-ts-mode
                     '(("wideblock"
                        "#wideblock(\n$0\ndouble: true,\n)"
                        "wideblock" nil nil nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/wideblock"
                        nil nil)
                       ("quote"
                        "#quote(attribution: [$1])[\n$2\n]\n$0"
                        "quote" nil nil nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/quote"
                        nil nil)
                       ("lorem" "#lorem($1)$0" "lorem" nil nil nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/lorem"
                        nil nil)
                       ("grid" "#grid(\ncolumns:(50%,50%),\n$0\n)\n"
                        "grid" nil nil nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/grid"
                        nil nil)
                       ("//" "($1)/($2)" "frac" nil nil nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/frac"
                        nil nil)
                       ("align" "#align($1)[\n$2\n]$0" "align" nil nil
                        nil
                        "/home/sm/.config/emacs/snippets/typst-ts-mode/align"
                        nil nil)))


;;; Do not edit! File generated at Wed Dec 17 21:35:20 2025
