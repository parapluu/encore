;;; encore-mode.el

;; Author: Elias Castegren 2017 (elias.castegren@it.uu.se)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Please e-mail me with comments and suggested changes if
;; you have any.
;;
;; Commentary:
;;
;; This mode gives syntax highlighting and Haskell-mode-style
;; cyclic indentation for Encore. There is also support for
;; ruby-mode-style matching of blocks, imenu navigation, automatic
;; compilation and flychecking. If you use expand-region, there is
;; support for that as well.
;;
;; Usage:
;;
;; Add this folder to your load-path and require encore-mode:
;;
;; (add-to-list 'load-path "~/path/to/encore-mode/")
;; (require 'encore-mode)
;;
;; There is a hook to enable encore-mode for all files with
;; extension '.enc'. All the bells and whistles mentioned above
;; should be enabled automatically. See encore-block.el for
;; configuration.
;;
;; If you use yasnippets (and you should!) add the snippets folder
;; as well:
;;
;; (add-to-list 'yas-snippet-dirs "~/path/to/emacs-mode/snippets")
;;
;; Code

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep these lists sorted
(setq encore-keywords
      '(
        "active"
        "as"
        "and"
        "async"
        "await"
        "borrow"
        "borrowed"
        "break"
        "by"
        "case"
        "chain"
        "class"
        "consume"
        "def"
        "do"
        "else"
        "end"
        "eos"
        "for"
        "fun"
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
        "linear"
        "local"
        "match"
        "module"
        "new"
        "not"
        "or"
        "print"
        "println"
        "qualified"
        "read"
        "repeat"
        "require"
        "shared"
        "sharable"
        "stream"
        "subord"
        "suspend"
        "then"
        "this"
        "trait"
        "typedef"
        "unless"
        "val"
        "var"
        "when"
        "where"
        "while"
        "with"
        "yield"
        ))

(setq encore-danger-words
      '(
        "BODY"
        "EMBED"
        "END"
        "unsafe"
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
        "unit"
        ))

(setq encore-operators
      '(
        "|||"
        ">>"
        ))

(setq encore-keywords-regexp (regexp-opt encore-keywords 'symbols))
(setq encore-danger-regexp (regexp-opt encore-danger-words 'symbols))
(setq encore-constants-regexp (regexp-opt encore-constants 'symbols))
(setq encore-primitives-regexp (regexp-opt encore-primitives 'symbols))
(setq encore-operators-regexp (regexp-opt encore-operators 'symbols))

(setq encore-types-regexp "\\<[A-Z][a-zA-Z]*\\>")
(setq encore-function-regexp "\\<\\(def\\|stream\\|fun\\|require\\)\\> \\([^(]*\\)([^)]*)\\W*:\\W*.*")
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

(defvar encore-mode-syntax-table nil "Syntax table for `encore-mode'.")

(setq encore-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; Block comment "{- ... -}"
        ;; Inline comment "-- ..."
        (modify-syntax-entry ?\{ "(}1nb" synTable)
        (modify-syntax-entry ?\} "){4nb" synTable)
        (modify-syntax-entry ?- "_ 123" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

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
  (if (equal first "trait")
      0
  (if (equal first "typedef")
      0

  (if (equal first "def")
      (if (string-match "\\<def\\>" line)
          (match-beginning 0)
      (if (string-match "\\(\\<.+\\>\\)? *\\<class\\>" line)
          (+ (match-beginning 0) encore-tab-width)
      (if (string-match "\\(\\<.+\\>\\)? *\\<trait\\>" line)
          (+ (match-beginning 0) encore-tab-width))))

  (if (equal first "fun")
      (if (string-match "\\<where\\>" line)
          (+ (match-beginning 0) encore-tab-width)
      (if (string-match "\\<end\\>" line)
          (match-beginning 0)
      (if (string-match "val *\\w* *= *$" line)
          (+ (match-beginning 0) encore-tab-width)
      (if (string-match "var *\\w* *= *$" line)
          (+ (match-beginning 0) encore-tab-width)
      (if (string-match "\\w* *= *$" line)
          (+ (match-beginning 0) encore-tab-width)
        0)))))

  (if (equal first "where")
      (if (string-match "\\<def\\>" line)
          (match-beginning 0)
      (if (string-match "^\\W*\\(\\<fun\\>\\) *[^(]" line)
          (match-beginning 1)
      (if (string-match "\\<class\\>" line)
          (match-beginning 0)
      (if (string-match "\\<.+\\>? *\\<trait\\>" line)
          (match-beginning 0)))))

  (if (equal first "else")
      (if (string-match "\\<else\\> *\\<if\\>.*\\<then\\> *$" line)
          (match-beginning 0)
      (if (string-match "\\<if\\>.*\\<then\\> *$" line)
          (match-beginning 0)
      (if (string-match "\\<then\\> *$" line)
          (match-beginning 0)
      (if (string-match "\\<if\\> *$" line)
          (match-beginning 0)))))

  (if (equal first "then")
      (if (string-match "\\<else\\> *\\<if\\> *$" line)
          (match-beginning 0)
      (if (string-match "\\<if\\> *$" line)
          (match-beginning 0)))

  (if (and (not (string-match "\\<end\\>" line)) (string-match "\\<else\\>" line))
      (+ (match-beginning 0) encore-tab-width)

  (if (string-match "^[ \t]*\\(\\<then\\>\\)" line)
      (+ (match-beginning 1) encore-tab-width)

  (if (equal first "case")
      (if (string-match "\\<case\\>" line)
          (match-beginning 0)
      (if (string-match "\\<match\\>" line)
          (+ (match-beginning 0) encore-tab-width)))

  (if (string-match "^[ \t]*\\(\\<case\\>.*=>\\)" line)
      (+ (match-beginning 1) encore-tab-width)

  (if (equal first "in")
      (if (string-match "\\<let\\>" line)
          (match-beginning 0))

  (if (string-match "^[ \t]*\\(\\<in\\>\\) *$" line)
      (+ (match-beginning 1) encore-tab-width)

  (if (and (not (string-match "\\<in\\>" line))
           (string-match "\\<let\\> *\\(.+\\) *= *" line))
      (match-beginning 1)

  ;; TODO: If the current line is "x =", we should not use this rule
  (if (string-match "val *\\w* *= *$" line)
      (+ (match-beginning 0) encore-tab-width)
  (if (string-match "var *\\w* *= *$" line)
      (+ (match-beginning 0) encore-tab-width)
  (if (string-match "\\w* *= *$" line)
      (+ (match-beginning 0) encore-tab-width)

  (if (string-match "\\<EMBED\\> *(.*) *$" line)
      (+ (match-beginning 0) encore-tab-width)

  (if (not (string-match encore-block-open-regex line))
      (if (string-match "[^ \t]" line)
          (match-beginning 0))
  (if (and (not (string-match "\\<end\\>" line)) (string-match encore-block-open-regex line))
      (+ (match-beginning 0) encore-tab-width)
  (if (bobp)
      0)))))))))))))))))))))))

(setq encore-checked-line nil)
(make-local-variable 'encore-checked-line)

(setq encore-last-indent 100)
(make-local-variable 'encore-last-indent)

(setq encore-indent-trigger-commands
      '(indent-for-tab-command yas-expand yas/expand))

(setq encore-block-open-regex
      (concat "\\<def\\>"    "\\|"
              "\\<fun\\>"    "\\|"
              "\\(\\<.+\\>\\)? *\\<class\\>" "\\|"
              "\\(\\<.+\\>\\)? *\\<trait\\>" "\\|"
              "\\<while\\>"  "\\|"
              "\\<for\\>"    "\\|"
              "\\<repeat\\>" "\\|"
              "\\<do\\>"     "\\|"
              "\\<fun\\>" "\\|"
              "\\<let\\>" "\\|"
              "\\<if\\>" "\\|"
              "\\<unless\\>" "\\|"
              "\\<borrow\\>" "\\|"
              "\\<match\\>.*\\<with\\> *\\($\\|--\\)\\|\\<case\\>.*=>[ \t]*\\($\\|--\\)"))

(defun encore-skip-block ()
  "Skip the current block"
  (interactive)
  (let ((skipped nil))
    (while (and (not (eobp))
                (not (string-match "\\<end\\>\\|\\<where\\>" (current-line))))
      (if (not skipped) (forward-line 1))
      (setq skipped nil)
      (if (and (not (string-match "\\<else\\> +\\<if\\>" (current-line)))
               (string-match encore-block-open-regex (current-line)))
          (progn (encore-skip-block)
                 (setq skipped t)
                 (forward-line 1))))))

(defun encore-skip-block-backward ()
  "Skip the current block backwards"
  (interactive)
  (let ((skipped nil))
    (while (and (not (bobp))
                (or (string-match "\\<else\\> +\\<if\\>" (current-line))
                    (not (string-match encore-block-open-regex (current-line)))))
      (if (not skipped) (forward-line -1))
      (setq skipped nil)
      (if (string-match "\\<end\\>" (current-line))
          (progn (encore-skip-block-backward)
                 (forward-line -1)
                 (setq skipped t))))))

(defun indent-to-nearest-open-block ()
  "Find the indent of the nearest block without a matching end"
  (interactive)
  (let ((indent 0) (done nil))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp)) (not done))
        (if (and (not (string-match "^[ \t]*--" (current-line)))
                 (string-match "\\<end\\>" (current-line)))
            (encore-skip-block-backward)
          (if (and (not (string-match "^[ \t]*--" (current-line)))
                   (not (string-match "\\<require\\>" (current-line)))
                   (not (string-match "\\<else\\> +\\<if\\>" (current-line)))
                   (string-match encore-block-open-regex (current-line)))
              (progn (setq indent (match-beginning 0))
                     (setq done 't))))
        (forward-line -1)))
    (indent-line-to indent)))

(defun encore-indent-line ()
  "Indent current line as encore code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((indent) (first (first-word)))
        (if (equal first "end")
            (indent-to-nearest-open-block)
        (save-excursion
          (if (memq last-command encore-indent-trigger-commands)
              (progn
                (goto-char (point-min))
                (forward-line (1- encore-checked-line)))
            (progn
              (setq encore-checked-line (line-number-at-pos))
              (setq encore-last-indent 100)))
          (while (and (< 1 (line-number-at-pos))
                      (or (not indent) (and (> indent encore-tab-width)
                                            (>= indent encore-last-indent))))
            (forward-line -1)
            ;; TODO: Skip backwards when finding an 'end'
            (if (not (string-match "^[ \t]*--" (current-line)))
                (progn
                  (setq indent (classify-indent (current-line) first))
                  (setq encore-checked-line (line-number-at-pos))))))
        (if (not indent) (setq indent 0))
        (indent-line-to indent)
        (if (<= indent encore-tab-width)
            (progn
              (setq encore-checked-line (line-number-at-pos))
              (setq encore-last-indent 100))
          (setq encore-last-indent indent))))))
    (if (looking-back "^[ \t]*") (back-to-indentation)))

;;;;;;;;;;;;;;;
;; selection ;;
;;;;;;;;;;;;;;;

(require 'encore-mode-expansions)

(when (require 'expand-region nil :noerror)
  (add-hook 'encore-mode-hook
            (lambda ()
              (make-local-variable 'er/try-expand-list)
              (add-encore-mode-expansions))))

;;;;;;;;;;;
;; imenu ;;
;;;;;;;;;;;

(defvar encore-imenu-generic-expression
  '(("passive class" "^\s*passive\s+class\s*\\(\\<\\w+\\>\\) *\\(\\[.*\\]\\)? *:?.*" 1)
    ("active class" "^\s*class\s*\\(\\<\\w+\\>\\)" 1)
    ("trait" "^\\<.+\\>?\s*trait\s*\\(\\<\\w+\\>\\)" 1)
    ("method definition" "^\s*def\s*\\(.*\s+\\)*\\(\\<\\w+\\>\\) *\\(\\[.*\\]\\)?(" 2)
    ("function definition" "^\s*fun\s*\\(\\w+\\) *\\(\\[.*\\]\\)?(" 1))
  "Contains regexes to parse Encore with imenu")

(defun encore-imenu-configure ()
  (interactive)
  (setq imenu-generic-expression encore-imenu-generic-expression))

(add-hook 'encore-mode-hook
          (lambda ()
            (make-local-variable 'comment-start)
            (setq comment-start "--")
            (setq imenu-generic-expression (encore-imenu-configure))))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(add-hook 'encore-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "encorec " (buffer-name)))))

(setq encorec-error-regexp '(encorec "\"\\(.*\\.enc\\)\" (line \\([0-9]*\\), column \\([0-9]*\\)" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist encorec-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'encorec)))

;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

(when (require 'flycheck nil :noerror)
  (flycheck-define-checker encorec
    "The Encore compiler"
    :command ("encorec" "-I" "." "-tc" source)
    :error-patterns
    ((warning "Warning at" "\"" (file-name) "\"" " (line " line ", column " column "):\n"
              (message))
     (error "\"" (file-name) "\"" " (line " line ", column " column "):\n"
            (message))
     (error " *** Error during typechecking *** \n"
            "\"" (file-name) "\"" " (line " line ", column " column ")\n"
            (message))
     (error " *** Error during capturechecking *** \n"
            "\"" (file-name) "\"" " (line " line ", column " column ")\n"
            (message))
     (info line-start "Importing module" (message) line-end)
     )
    :modes encore-mode)
  (add-to-list 'flycheck-checkers 'encorec)
  (add-hook 'encore-mode-hook
            (lambda ()
              (flycheck-mode))))


;;;;;;;;;;;;;;;;;;
;; encore-block ;;
;;;;;;;;;;;;;;;;;;

(require 'encore-block)

(add-hook 'encore-mode-hook
          (lambda ()
            (encore-block-mode t)))

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode encore-mode prog-mode
  (setq font-lock-defaults '(encore-font-lock-keywords))
  (setq mode-name "Encore")
  (set (make-local-variable 'indent-line-function) 'encore-indent-line)
  )

;; Open "*.enc" in encore-mode
(add-to-list 'auto-mode-alist '("\\.enc\\'" . encore-mode))

(provide 'encore-mode)
;;; encore-mode.el ends here
