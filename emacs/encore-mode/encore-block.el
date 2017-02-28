;;; encore-block.el --- highlight matching block

;; This file is based on ruby-block.el:
;; Copyright (C) 2007-2013 khiker, shishi, juszczakn

;; Author: Elias Castegren <elias.castegren@it.uu.se>
;; Keywords: languages, faces, encore

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.         See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.        If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Usage:

;; This mode is automatically loaded by encore-mode. If you want
;; to use it separately, add these lines to your .emacs:
;;
;; (require 'encore-block)
;; (encore-block-mode t)
;;
;; In addition, you can also add one of these lines too:
;;
;; ;; do overlay
;; (setq encore-block-highlight-toggle 'overlay)
;; ;; display to minibuffer
;; (setq encore-block-highlight-toggle 'minibuffer)
;; ;; display to minibuffer and do overlay
;; (setq encore-block-highlight-toggle t)
;;
;; Default is minibuffer.
;;
;; Tested on Emacs 24.3

;;; Note:

;; A encore-mode.el or enh-encore-mode is necessary to use this package.

;;; Code:

;; Variables:

(defconst encore-block-version "0.0.11"
  "Encore block package version. Based on ruby-block 0.0.11")

(defconst encore-block-keyword-list
  ;; TODO: Add all keywords? Reuse the one from encore-mode?
  (list "case"
        "class"
        "def"
        "do"
        "end"
        "for"
        "fun"
        "if"
        "let"
        "match"
        "repeat"
        "trait"
        "until"
        "while"
        "unless")
  "Keywords for highlighting.")

(defconst encore-block-keyword-regex
  (concat
   "[ \t]*\\("
   "case.*=> *$"          "\\|"
   "class"                "\\|"
   "def"                  "\\|"
   "do"                   "\\|"
   "end"                  "\\|"
   "fun *(.*) *$"         "\\|" ;; Closure
   "fun *.*(.*) *:[^=]*$" "\\|" ;; Function
   "if"                   "\\|"
   "let"                  "\\|"
   "match.*with"          "\\|"
   "trait"                "\\|"
   "until"                "\\|"
   "unless"
   "\\)")
  "Regular expression to look for correspondence.")

(defgroup encore-block nil
  "Encore block"
  :tag "Encore block"
  :group 'encore-block)

(defcustom encore-block-delay 0.50
  "*Time in seconds to delay before showing a matching paren."
  :type         'number
  :group 'encore-block)

(defcustom encore-block-highlight-face 'highlight
  "*Face for block highlighting."
  :type         'face
  :group 'encore-block)

(defcustom encore-block-highlight-toggle 'minibuffer
  "*How to display corresponding line.
    Default is minibuffer. display to minibuffer.
    The choices are as follows.
    nil        => nothing
    minibuffer => minibuffer
    overlay    => overlay
    t          => minibuffer and overlay"
  :type '(choice (const :tag "nothing" nil)
                 (const :tag "minibuffer" minibuffer)
                 (const :tag "overlay" overlay)
                 (const :tag "minibuffer and overlay" t))
  :group 'encore-block)

(defvar encore-block-timer nil)

(defvar encore-block-highlight-overlay nil)


;; Functions:

(define-minor-mode encore-block-mode
  "In encore-mode, Displays the line where there is a keyword corresponding
   to END keyword.
   This is a minor-mode for encore-mode only."
  :init-value t
  :global nil
  :keymap nil
  :lighter " EBlock"
  (if encore-block-mode
      (encore-block-start-timer)
    (encore-block-stop-timer)))

(defun encore-block-start-timer ()
  "start timer."
  (when encore-block-timer
    (cancel-timer encore-block-timer))
  (setq encore-block-timer
        (run-with-idle-timer encore-block-delay t 'encore-block-hook)))

(defun encore-block-stop-timer ()
  "Stop timer."
  (when encore-block-timer
    (cancel-timer encore-block-timer)
    (setq encore-block-timer nil)))

(defun encore-block-hook ()
  "When Major-mode is encore-mode, this package is running."
  (if (eq major-mode 'encore-mode)
      (condition-case err
          (encore-block-function)
        (error
         (setq encore-block-mode nil)
         (message "Error: %S; encore-block-mode now disabled." err)))
    (setq encore-block-mode nil)))

(defun encore-block-line-beginning-position (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
            (pos (point))
            (count 0))
        (while (and (not (funcall xor (bobp) (eolp)))
                    (> pos (point-min)))
          (setq pos (1- pos))
          (goto-char (1- (point))))
        ;; delete linefeed of start point.
        (when (and (eolp) (>= (point-max) (1+ pos)))
          (setq pos (1+ pos)))
        pos))))

(defun encore-block-line-end-position (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
            (pos (point)))
        (while (and (not (funcall xor (eobp) (eolp)))
                    (>= (point-max) pos))
          (setq pos (1+ pos))
          (goto-char (1+ (point))))
        pos))))

(defun encore-block-function ()
  "Point position's word decides behavior."
  (let* ((cur (current-word))
         ;; if point after END, dec point and get face
         (p (point))
         (face (get-text-property p 'face))
         (p (if (and (eq nil face) (> p 3))
                (1- p)
              p)))
    (when (and (member cur '("else" "end"))
               (eq face 'font-lock-keyword-face))
      (let* ((pos (encore-block-corresponding-position p))
             (sp (encore-block-line-beginning-position pos))
             (ep (encore-block-line-end-position pos)))
        (when pos
          ;; display line contents to minibuffer
          (when (memq encore-block-highlight-toggle '(t minibuffer))
            (message "%d: %s"
                     (1+ (count-lines (point-min) sp)) (buffer-substring sp ep)))
          ;; do overlay
          (when (memq encore-block-highlight-toggle '(t overlay))
            (encore-block-do-highlight sp ep)))))))

(defun encore-block-stmt-if (pos)
  (save-excursion
    (goto-char pos)
    (let ((status 'skip))
      (while (and (not (bolp))
                  (eq status 'skip))
        (forward-char -1)
        (let ((ch (char-after)))
          (cond
           ((memq ch '(?\n ?\r ?\())
            (setq status t))
           ((memq ch '(32 \t))
            (setq status 'skip))
           (t
            (setq status nil)))))
      (when (eq status 'skip)
        (setq status t))
      status)))

(defun encore-block-corresponding-position (pos)
  "Get point of corresponding line."
  (save-excursion
    (goto-char pos)
    (let ((key 1) pos face cur)
      (while (and (> key 0)
                  (re-search-backward encore-block-keyword-regex nil t))
        (setq pos (match-beginning 1)
              face (get-text-property pos 'face)
              cur (current-word))
        (when (and (eq face 'font-lock-keyword-face)
                   (member cur encore-block-keyword-list)
                   ;; case: STMT if (or let) EXPR
                   (cond
                    ((member cur '("if" "let"))
                     (encore-block-stmt-if pos))
                    (t t)))
          (cond
           ((and (string= cur "end"))
            (setq key (1+ key)))
           (t
            (setq key (1- key))))))
      (when (= key 0)
        pos))))

(defun encore-block-do-highlight (beg end)
  "Do overlay corresponding line."
  (if encore-block-highlight-overlay
      (move-overlay encore-block-highlight-overlay beg end)
    (setq encore-block-highlight-overlay (make-overlay beg end)))
  (overlay-put encore-block-highlight-overlay
               'face encore-block-highlight-face)
  (add-hook 'pre-command-hook 'encore-block-highlight-done))

(defun encore-block-highlight-done ()
  "After do overlay, restore the line to original color."
  (remove-hook 'pre-command-hook 'encore-block-highlight-done)
  (if encore-block-highlight-overlay
      (delete-overlay encore-block-highlight-overlay)))

(defun encore-block-highlight-toggle ()
  "Switch on/off for encore-block-mode."
  (interactive)
  (if encore-block-highlight-toggle
      (setq encore-block-highlight-toggle nil)
    (setq encore-block-highlight-toggle t)))

(provide 'encore-block)

;; Local Variables:
;; Coding: utf-8
;; End:

;;; encore-block.el ends here