;;; encore-mode-expansions.el --- Encore-specific expansions for expand-region

;; Author: Elias Castegren 2017

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

;; Known issues:
;;
;; * There is a lot of code in this file. Maybe there is some more
;;   general way to do this?
;;
;; * encore-mark-inside-block does the wrong thing sometimes.

;;; Code:

(defun encore-mark-var-decl ()
  "Select 'var/val = e'"
  (interactive)
  (let (p1 p2 col (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<val\\|var\\>"))

      (setq col (current-column))
      (forward-line 1)
      (back-to-indentation)
      (while (> (current-column) col)
        (forward-line 1)
        (back-to-indentation))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))


(defun encore-mark-if-end ()
  "Select single line 'if b then e1 else e2'"
  (interactive)
  (let (p1 p2 lbound rbound (case-fold-search nil))
    (progn
      (move-beginning-of-line nil)
      (setq lbound (point))
      (move-end-of-line nil)
      (setq rbound (point))
      (setq p1 (re-search-backward "[^e] \\<if\\>" lbound))
      (setq p2 (re-search-forward "\\<end\\>" rbound))
      (goto-char p1)
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-outside-block ()
  "Select block including delimiters"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (setq p1 (point))
      (if (not (string-match "\\<end\\>" (current-line)))
          (while (and (not (eobp))
                      (not (string-match "^[ \t]*\\<end\\>" (current-line))))
            (forward-line 1)
            (if (and (not (string-match "\\<else\\> +\\<if\\>" (current-line)))
                     (string-match encore-block-open-regex (current-line)))
                (progn
                  (encore-skip-block)
                  (forward-line 1)))))

      (move-beginning-of-line nil)
      (setq p2 (re-search-forward "\\<end\\>"))

      (goto-char p1)
      (while (and (not (bobp))
                  (or (string-match "\\<else\\> +\\<if\\>" (current-line))
                      (string-match "\\<match\\>.*\\<with\\>" (current-line))
                      (string-match "\\<class\\|trait\\>" (current-line))
                      (not (string-match encore-block-open-regex (current-line)))))
        (forward-line -1)
        (if (and (not (string-match "^[ \t]*--" (current-line)))
                 (string-match "\\<end\\>" (current-line)))
            (progn
              (encore-skip-block-backward)
              (forward-line -1))))

      (move-beginning-of-line nil)
      (re-search-forward encore-block-open-regex)
      (re-search-backward encore-block-open-regex)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-inside-block ()
  "Select block excluding delimiters"
  (interactive)
  (let (p1 p2 bound (case-fold-search nil))
    (progn
      (if (region-active-p) (forward-line -1))
      (setq p1 (point))
      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\(\\<end\\>\\|\\<where\\>\\)" (current-line)))
                  (not (string-match "\\<else\\>" (current-line))))
        (forward-line 1)
        (if (and (not (string-match "\\<else\\> +\\<if\\>" (current-line)))
                 (string-match encore-block-open-regex (current-line)))
            (progn
              (encore-skip-block)
              (forward-line 1))))

      (move-beginning-of-line nil)
      (setq p2 (re-search-forward "\\<end\\>\\|\\<where\\>\\|\\<else\\>"))
      (backward-sexp)
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (while (and (not (bobp))
                  (or (string-match "\\<else\\> +\\<if\\>" (current-line))
                      (string-match "\\<end\\>" (current-line))
                      (and (not (string-match encore-block-open-regex (current-line)))
                           (not (string-match "\\<else\\>" (current-line)))
                           (not (string-match "\\<then\\>" (current-line))))))
        (forward-line -1)
        (if (and (not (string-match "^[ \t]*--" (current-line)))
                 (string-match "\\<end\\>" (current-line)))
            (progn
              (encore-skip-block-backward)
              (forward-line -1))))

      (if (string-match "\\<do\\|then\\|else\\|in\\>\\|=>" (current-line))
          (progn
            (move-beginning-of-line nil)
            (re-search-forward "\\<do\\|then\\|else\\|in\\>\\|=>"))
        (move-end-of-line nil))
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-let-in ()
  "Mark code between 'let' and 'in'"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<let\\>"))
      (setq p2 (re-search-forward "\\<in\\>"))
      (goto-char (- p2 2))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char (+ p1 3))
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-let-in ()
  "Mark code between 'let' and 'in'"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<let\\>"))
      (setq p2 (re-search-forward "\\<in\\>"))
      (goto-char (- p2 2))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char (+ p1 3))
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-in-end ()
  "Select 'let x = e1 in e2 end'"
  (interactive)
  (let (p1 p2 bound)
    (progn
      (move-end-of-line nil)
      (setq bound (point))
      (setq p1 (re-search-backward "\\<in\\>"))
      (setq p2 (re-search-forward "\\<end\\>" bound))
      (goto-char (- p2 3))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char (+ p1 2))
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-inside-block-closure ()
  "Select the body of a block closure"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<fun\\>\\W*(.*)[^=>]*$"))
      (setq p2 (re-search-forward "\\<end\\>"))
      (goto-char (- p2 3))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char p1)
      (move-end-of-line nil)
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-outside-block-closure ()
  "Select a block closure including header and end"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<fun\\>\\W*(.*)[^=>]*$"))
      (setq p2 (re-search-forward "\\<end\\>"))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-inside-line-closure ()
  "Select the body of an inline closure"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "=>"))
      (move-end-of-line nil)
      (skip-chars-backward " \t")
      (setq p2 (point))
      (goto-char (+ p1 2))
      (skip-chars-forward " \t")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-outside-line-closure ()
  "Select a whole inline closure"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "fun.*=>.*$"))
      (move-end-of-line nil)
      (skip-chars-backward " \t")
      (setq p2 (point))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-line-case ()
  "Select a single line case"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "case.*=>.*\\W.*$"))
      (move-end-of-line nil)
      (skip-chars-backward " \t")
      (setq p2 (point))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-match-with-end ()
  "Select 'match e with ... end'"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<match\\>"))
      (forward-line 1)
      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (string-match encore-block-open-regex (current-line))
            (progn
              (encore-skip-block)))
        (forward-line 1))
      (setq p2 (re-search-forward "\\<end\\>"))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-match-with ()
  "Select code between 'match' and 'end'"
  (interactive)
  (let (p1 p2 bound (case-fold-search nil))
    (progn
      (move-beginning-of-line nil)
      (setq bound (point))
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<match\\>" bound))
      (setq p2 (re-search-forward "\\<with\\>"))
      (goto-char (- p2 4))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char (+ p1 5))
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-inside-embed ()
  "Select code between 'EMBED (...)' and 'END'"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<EMBED\\>"))
      (setq p2 (re-search-forward "\\<END\\>"))
      (goto-char (- p2 3))
      (skip-chars-backward " \t\n")
      (setq p2 (point))
      (goto-char (+ p1 5))
      (forward-sexp)
      (skip-chars-forward " \t\n")
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-outside-embed ()
  "Select 'EMBED (...) ... END'"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<EMBED\\>"))
      (setq p2 (re-search-forward "\\<END\\>"))
      (goto-char p1)
      (push-mark p2)
      (setq mark-active t))))

(defun encore-mark-inside-function ()
  "Select the body of a function or method"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<fun\\|def\\> *\\w+"))
      (move-end-of-line nil)
      (skip-chars-forward " \t\n")
      (setq p1 (point))

      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\|where\\>" (current-line))))
        (if (or (string-match encore-block-open-regex (current-line))
                (string-match "\\<EMBED\\>" (current-line)))
            (encore-skip-block))
        (forward-line 1))
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-outside-function ()
  "Select a function or method including header and end"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<fun\\|def\\> *\\w+"))

      (forward-line 1)
      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (or (string-match encore-block-open-regex (current-line))
                (string-match "\\<EMBED\\>" (current-line)))
            (encore-skip-block))
        (forward-line 1))

      (setq p2 (re-search-forward "\\<end\\>"))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-inside-where-clause ()
  "Select the local functions of a where clause"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (re-search-backward "\\<where\\>")
      (move-end-of-line nil)
      (skip-chars-forward " \t\n")
      (setq p1 (point))

      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (string-match encore-block-open-regex (current-line))
            (encore-skip-block))
        (forward-line 1))
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-outside-where-clause ()
  "Select a where clause"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (re-search-backward "\\<where\\>")
      (back-to-indentation)
      (setq p1 (point))
      (forward-line 1)

      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (string-match encore-block-open-regex (current-line))
            (progn (encore-skip-block)))
        (forward-line 1))
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-field ()
  "Select a field"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<var\\|val\\>"))
      (move-end-of-line nil)
      (setq p2 (point))
      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-require ()
  "Select a requirement"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (setq p1 (re-search-backward "\\<require\\>"))
      (move-end-of-line nil)
      (setq p2 (point))
      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-inside-class-or-trait ()
  "Select the contents of a class or trait"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (re-search-backward "\\<class\\>\\|\\<\.+\>? *\\<trait\\>")
      (move-end-of-line nil)
      (skip-chars-forward " \t\n")
      (setq p1 (point))

      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (and (not (string-match "\\<require\\>" (current-line)))
                 (string-match encore-block-open-regex (current-line)))
            (encore-skip-block))
        (forward-line 1))
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-outside-class-or-trait ()
  "Select a whole class or trait"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (re-search-backward "\\<class\\|trait\\>")
      (move-beginning-of-line nil)
      (setq p1 (point))

      (forward-line 1)
      (while (and (not (eobp))
                  (not (string-match "^[ \t]*\\<end\\>" (current-line))))
        (if (and (not (string-match "\\<require\\>" (current-line)))
                 (string-match encore-block-open-regex (current-line)))
            (encore-skip-block))
        (forward-line 1))

      (setq p2 (re-search-forward "\\<end\\>"))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

(defun encore-mark-typedef ()
  "Select a typedef"
  (interactive)
  (let (p1 p2 (case-fold-search nil))
    (progn
      (move-end-of-line nil)
      (re-search-backward "\\<typedef\\>")
      (move-beginning-of-line nil)
      (setq p1 (point))

      (forward-line 1)
      (re-search-forward "\\<typedef\\|fun\\|trait\\|class\\>")
      (move-beginning-of-line nil)
      (skip-chars-backward " \t\n")
      (setq p2 (point))

      (goto-char p1)
      (push-mark p2)

      (setq mark-active t))))

;; To run in encore-mode-hook
(defun add-encore-mode-expansions ()
  (setq er/try-expand-list
        (append
         er/try-expand-list
         '(encore-mark-var-decl
           encore-mark-if-end
           encore-mark-outside-block
           encore-mark-inside-block
           encore-mark-let-in
           encore-mark-in-end
           encore-mark-let-in-end
           encore-mark-inside-block-closure
           encore-mark-outside-block-closure
           encore-mark-inside-line-closure
           encore-mark-outside-line-closure
           encore-mark-line-case
           encore-mark-match-with-end
           encore-mark-match-with
           encore-mark-inside-embed
           encore-mark-outside-embed
           encore-mark-inside-function
           encore-mark-outside-function
           encore-mark-inside-where-clause
           encore-mark-outside-where-clause
           encore-mark-field
           encore-mark-require
           encore-mark-inside-class-or-trait
           encore-mark-outside-class-or-trait
           encore-mark-typedef))))

(provide 'encore-mode-expansions)

;; latex-mode-expansions.el ends here
