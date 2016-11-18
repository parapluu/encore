;; By Elias Castegren (elias.castegren@it.uu.se)
;; Feel free to use and modify this file however you see fit.
;; Please e-mail me with comments and suggested changes if
;; you have any.
;;
;; This mode gives syntax highlighting and Haskell-mode-style
;; cyclic indentation for the mylittlepony version of encore.
;;
;; Put this file where Emacs can find it and require it in your
;; init-file. There is a hook to enable encore-mode for all files
;; with extension .enc.

;; Please keep these lists sorted
(setq encore-keywords
      '(
        "as"
        "and"
        "async"
        "await"
        "by"
        "chain"
        "class"
        "def"
        "else"
        "eos"
        "for"
        "foreach"
        "get"
        "getNext"
        "hiding"
        "if"
        "import"
        "in"
        "join"
        "let"
        "liftf"
        "liftv"
        "match"
        "module"
        "new"
        "not"
        "or"
        "passive"
        "print"
        "println"
        "qualified"
        "repeat"
        "require"
        "stream"
        "suspend"
        "then"
        "this"
        "trait"
        "typedef"
        "unless"
        "val"
        "when"
        "while"
        "with"
        "yield"
        ))

(setq encore-danger-words
      '(
        "body"
        "embed"
        "end"
        ))
(setq encore-constants
      '(
        "false"
        "null"
        "true"
        ))

(setq encore-primitives
      '(
        "bool"
        "char"
        "int"
        "uint"
        "void"
        ))

(setq encore-operators
      '(
        "||"
        ">>"
        ))

(setq encore-keywords-regexp (regexp-opt encore-keywords 'symbols))
(setq encore-danger-regexp (regexp-opt encore-danger-words 'symbols))
(setq encore-constants-regexp (regexp-opt encore-constants 'symbols))
(setq encore-primitives-regexp (regexp-opt encore-primitives 'symbols))
(setq encore-operators-regexp (regexp-opt encore-operators 'symbols))

(setq encore-types-regexp "\\<[A-Z][a-zA-Z]*\\>")
(setq encore-function-regexp "\\<\\(def\\|stream\\|require\\)\\> \\([^(]*\\)([^)]*)\\W*:\\W*.*")
(setq encore-variable-regexp "\\<\\([A-Za-z0-9_]*\\)\\>\\W*:")
(setq encore-comment-regexp "--.?*")
(setq encore-char-regexp "'\\(\\\\.\\|.\\)'")

(setq encore-font-lock-keywords
      `(
        (,encore-comment-regexp    . font-lock-comment-face)
        (,encore-char-regexp       . font-lock-string-face)
        (,encore-keywords-regexp   . font-lock-keyword-face)
        (,encore-danger-regexp     . font-lock-warning-face)
        (,encore-constants-regexp  . font-lock-constant-face)
        (,encore-primitives-regexp . font-lock-type-face)
        (,encore-operators-regexp  . font-lock-builtin-face)
        (,encore-types-regexp      . font-lock-type-face)
        (,encore-function-regexp   2 font-lock-function-name-face)
        (,encore-variable-regexp   1 font-lock-variable-name-face)
        )
      )

(setq encore-tab-width 2)
(make-local-variable 'encore-tab-width)

(defun current-line ()
  ""
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun first-word ()
  ""
  (let* ((current-line (current-line))
         (word-start (string-match "[^ \t]" current-line))
         (word-end (string-match "[^a-zA-Z]" current-line word-start)))
    (substring-no-properties current-line word-start word-end)))

(defun classify-indent (line first)
  "Give the proper indentation of a line below the current one"
  (if (equal first "class")
      0

    (if (equal first "def")
        (if (string-match "\\<def\\>" line)
            (match-beginning 0)
          (if (string-match "\\<passive\\>" line)
              (+ (match-beginning 0) encore-tab-width)
            (if (string-match "\\<class\\>" line)
                (+ (match-beginning 0) encore-tab-width))))

      (if (string-match "\\<passive\\> .*" line)
          (+ (match-beginning 0) encore-tab-width)

        (if (string-match "\\<class\\> .*" line)
            (+ (match-beginning 0) encore-tab-width)

          (if (string-match "\\<if\\>.*\\<then\\>.*\\<else\\>" line)
              (match-beginning 0)
            (if (string-match "\\<if\\>.*\\<then\\>" line)
                (if (equal first "else")
                    (match-beginning 0)
                  (+ (match-beginning 0) encore-tab-width))
              (if (string-match "\\<then\\>" line)
                  (if (equal first "else")
                      (match-beginning 0)
                    (+ (match-beginning 0) encore-tab-width))
                (if (string-match "\\<if\\>.*" line)
                    (if (or (equal first "then") (equal first "else"))
                        (match-beginning 0)
                      (let ((indent (string-match "[^ \t]" line (+ (match-beginning 0) 2)))) (if indent indent (+ (match-beginning 0) encore-tab-width))))
                  (if (string-match "\\<else\\>.*" line)
                      (+ (match-beginning 0) encore-tab-width)

                    (if (string-match "\\<while\\>" line)
                        (+ (match-beginning 0) encore-tab-width)

                      (if (string-match "\\<match\\>" line)
                          (+ (match-beginning 0) encore-tab-width)

                        (if (string-match "\\<let\\>.*\\<in\\>$" line)
                            (+ (match-beginning 0) encore-tab-width)

                          (if (string-match "\\<embed\\>.*\\<end\\>" line)
                              (match-beginning 0)
                            (if (string-match "\\<embed\\>" line)
                                (if (equal first "end")
                                    (match-beginning 0)
                                  (+ (match-beginning 0) encore-tab-width))

                              (if (string-match "\\<let\\>\\W*\\(\\w+\\)" line)
                                  (if (equal "in" first)
                                      (match-beginning 0)
                                    (match-beginning 1))
                                (if (string-match "\\<let\\>" line)
                                    (if (equal "in" first)
                                        (match-beginning 0)
                                      (+ (match-beginning 0) encore-tab-width))
                                  (if (string-match "\\<in\\>[ \t]*$" line)
                                      (+ (match-beginning 0) encore-tab-width)

                                    (if (string-match "\\<def\\>" line)
                                        (+ (match-beginning 0) encore-tab-width)

                                      (if (string-match "\\W*\\([^;]*\\);" line)
                                          (match-beginning 1)
                                        (if (bobp)
                                            0)))))))))))))))))))))

(setq encore-checked-line nil)
(make-local-variable 'encore-checked-line)

(setq encore-last-indent 100)
(make-local-variable 'encore-last-indent)

(setq encore-indent-trigger-commands
      '(indent-for-tab-command yas-expand yas/expand))

(defun encore-indent-line ()
  "Indent current line as encore code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((indent) (first (first-word)))
        (save-excursion
          (if (memq last-command encore-indent-trigger-commands)
              (progn
                (goto-char (point-min))
                (forward-line (1- encore-checked-line)))
            (progn
              (setq encore-checked-line (line-number-at-pos))
              (setq encore-last-indent 100)))
          (while (and (< 1 (line-number-at-pos)) (or (not indent) (and (> indent encore-tab-width) (>= indent encore-last-indent))))
            (forward-line -1)
            (setq indent (classify-indent (current-line) first))
            (setq encore-checked-line (line-number-at-pos))))
        (if (not indent) (setq indent 0))
        (indent-line-to indent)
        (if (<= indent encore-tab-width)
            (progn
              (setq encore-checked-line (line-number-at-pos))
              (setq encore-last-indent 100))
          (setq encore-last-indent indent)))))
  (if (looking-back "^[ \t]*") (back-to-indentation))
  )

(define-derived-mode encore-mode prog-mode
  (setq font-lock-defaults '(encore-font-lock-keywords))
  (setq mode-name "Encore")
  (set (make-local-variable 'indent-line-function) 'encore-indent-line)
  )

;; Open "*.enc" in encore-mode
(add-to-list 'auto-mode-alist '("\\.enc\\'" . encore-mode))

(defvar encore-imenu-generic-expression
  '(("passive class" "^\s*passive\s+class\s*\\(.*\\)" 1)
    ("active class" "^\s*class\s*\\(.*\\)" 1)
    ("method definition" "^\s*def\s*\\(.*\\) \{?" 1))
  "Contains regexes to parse Encore with imenu")

(defun encore-imenu-configure ()
  (interactive)
  (setq imenu-generic-expression encore-imenu-generic-expression))
;(setq-local imenu-create-index-function 'imenu-default-create-index-function))


;(add-to-list 'load-path (concat (file-name-directory (buffer-file-name)) "dtrt-indent-20140325.1330/"))
;(require 'dtrt-indent)
;(dtrt-indent-mode 1)

(add-hook 'encore-mode-hook
          (lambda ()
            (setq imenu-generic-expression (encore-imenu-configure))))

;; compilation-mode
(add-hook 'encore-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "encorec " (buffer-name)))))

(setq encorec-error-regexp '(encorec "\"\\(.*\\.enc\\)\" (line \\([0-9]*\\), column \\([0-9]*\\)" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist encorec-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'encorec)))

;; If you use flycheck-mode, add the following lines to your init file:
;(flycheck-define-checker encorec
;  "The Encore compiler"
;  :command ("encorec" "-I" "." "-tc" source)
;  :error-patterns
;    ((warning "Warning at" "\"" (file-name) "\"" " (line " line ", column " column "):\n"
;              (message))
;     (error "\"" (file-name) "\"" " (line " line ", column " column "):\n"
;            (message))
;     (error " *** Error during typechecking *** \n"
;            "\"" (file-name) "\"" " (line " line ", column " column ")\n"
;            (message))
;     (info line-start "Importing module" (message) line-end)
;     )
;  :modes encore-mode)

;(add-to-list 'flycheck-checkers 'encorec)

(provide 'encore-mode)
;;; encore-mode.el ends here
