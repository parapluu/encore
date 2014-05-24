;;; Compiled snippets and support files for `encore-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'encore-mode
                     '(("cl" "class ${1:Main} {\n      $0\n}" "cl" nil nil nil nil nil nil)
                       ("def" "def ${1:main}($2) : ${3:void}\n    $0" "def" nil nil nil nil nil nil)
                       ("i" "$1:int$0" "i" nil nil nil nil nil nil)
                       ("if" "if ${1:x==0}\nthen\n  $2\nelse\n  $0" "if" nil nil nil nil nil nil)
                       ("let" "let ${1:x} = ${2} in\n      $0" "let" nil nil nil nil nil nil)
                       ("s" "$1:string$0" "s" nil nil nil nil nil nil)
                       ("wh" "while ${1:i > 0}\n      $0" "wh" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat May 24 11:08:55 2014
