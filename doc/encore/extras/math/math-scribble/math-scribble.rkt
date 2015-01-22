#lang racket
;;;
;;; Support for MathJax and Asymptote.
;;;

(require scribble/manual
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scriblib/render-cond
         racket/path
         racket/runtime-path
         file/md5)


(provide $ $$ asymptote enable-math theorem proof corollary boxed exercise definition example remark 
         element html-only exact chapter subchapter subsubchapter pi)

(define-runtime-path math-inline.css "math-inline.css")
(define-runtime-path math-inline.tex "math-inline.tex")
(define-runtime-path math-display.css "math-display.css")
(define-runtime-path math-display.tex "math-display.tex")
(define-runtime-path boxed.css "boxed.css")
(define-runtime-path boxed.tex "boxed.tex")

(define-runtime-path htmlonly.tex "htmlonly.tex")
(define-runtime-path htmlonly.css "htmlonly.css")
        
(define mathjax-source
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  ; "http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
  ;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-HTML"
  )

(define (enable-math) 
  (cond-element 
   [latex ""]
   [html (make-element (make-style #f
                                   (list (make-script-property "text/javascript" mathjax-source)))
                       '())]
   [text ""]))


(define html-only-style
  (make-style "HTMLOnly"
              (list (make-css-addition htmlonly.css)
                    (make-tex-addition htmlonly.tex)
                    'exact-chars)))

(define (html-only s . strs)
  (make-element 
   html-only-style
   (cons s strs)))

(define (exact s . strs)
  (newline)
  (map display (cons s strs))
  (newline)
  (make-element (make-style "identity" '(exact-chars)) 
                (cons s strs)))


(define math-inline-style
  (make-style "MathInline"
              (list (make-css-addition math-inline.css)
                    (make-tex-addition math-inline.tex)
                    'exact-chars)))

(define math-display-style
  (make-style "MathDisplay"
              (list (make-css-addition math-display.css)
                    (make-tex-addition math-display.tex)
                    'exact-chars)))

#;(define boxed-style
    (make-style "Boxed"
                (list (make-css-addition boxed.css)
                      (make-tex-addition boxed.tex))))

(define single-dollar-l
  (cond-element 
   [latex "$"]
   [html "\\("]
   [text ""]))

(define single-dollar-r
  (cond-element 
   [latex "$"]
   [html "\\)"]
   [text ""]))

(define ($ s . strs)
  (make-element 
   math-inline-style
   (cons single-dollar-l (cons s (append strs (list single-dollar-r))))))


(define ($$ s . strs)
  (make-element 
   math-display-style
   (cons "\\[" (cons s (append strs '("\\]"))))))

(define (asymptote  s . strs)
  (define asymptote-dir "asymptote-images")
  (let* ([strs (apply string-append (cons s strs))]
         [md (bytes->string/utf-8 (md5 strs))]
         [asy-name (string-append md ".asy")]
         [asy-path (build-path asymptote-dir asy-name)]
         [png-name (string-append md ".png")]
         [png-path (build-path asymptote-dir png-name)]
         [eps-name (string-append md ".eps")]
         [eps-path (build-path asymptote-dir eps-name)]
         [pdf-name (string-append md ".pdf")]
         [pdf-path (build-path asymptote-dir pdf-name)]
         [svg-name (string-append md ".svg")]
         [svg-path (build-path asymptote-dir svg-name)])
    (display (current-directory)) (display md) (newline)

    ;; create dir if neccessary
    (unless (directory-exists? asymptote-dir)
      (make-directory asymptote-dir))
    ;; save asymptote code to <md5-of-input>.asy
    (with-output-to-file asy-path
      (lambda () (display strs) (newline))
      #:exists 'replace)
    (parameterize ([current-directory (build-path (current-directory) asymptote-dir)])
      ;; run asymptote to generate eps
      (unless (file-exists? svg-name)
          (system (format "asy  -f svg ~a" asy-name)))
      ;; run asymptote to generate pdf
      (unless (file-exists? pdf-name)
        (system (format "asy -v -f pdf ~a" asy-name)))
      ;; run asymptote to generate png
      (unless (file-exists? png-name)
        (system (format "asy -v -f png ~a" asy-name)))
      ;(image png-path #:suffixes (list ".png" #;".pdf" ))     ; HTML pdf PDF pdf
      ;(image svg-path #:suffixes (list ".svg" #;".pdf" ))     ; HTML pdf PDF pdf
      (image (build-path asymptote-dir md) #:suffixes (list ".pdf" ".svg" ".png"))  ; HTML png PDF pdf
    
    ; (image (build-path svg-path) #:suffixes (list ".svg" ".pdf" ".png"))

)))


(define (boxed s . strs)
  (let ([s (cons s strs)])
    (make-element boxed-style (decode-content s))))

(define (definition s . strs)
  (let ([ss strs])
    (make-paragraph plain
                    (cons (make-element 'bold "Definition ")
                          (cons (make-element 'italic s)
                                (decode-content ss))))))

(define (theorem s . strs)
  (let ([ss strs])
    (make-paragraph plain
                    (cons (make-element 'bold "Sætning ")
                          (cons (make-element 'italic s)
                                (decode-content ss))))))

(define (example . strs)
  (let ([s (if (not (null? strs)) (car strs) "")]
        [ss (if (not (null? strs)) (cdr strs) null)])
    (make-paragraph plain
                    (cons (make-element 'bold "Eksempel ")
                          (cons (make-element 'italic s)
                                (decode-content ss))))))

(define (corollary s . strs)
  (let ([s (cons s strs)])
    (make-paragraph plain
                    (cons (make-element 'bold "Følgesætning ")
                          (decode-content s)))))

(define (proof . s)
  (make-paragraph plain
                  (cons (make-element 'bold "Bevis ")
                        (decode-content s))))

(define (remark . s)
  (make-paragraph plain
                  (cons (make-element 'bold "Bemærkning ")
                        (decode-content s))))

;;; One large page
;(define chapter section)
;(define subchapter subsection)
;(define subsubchapter subsubsection)

;;; Several small pages
(define chapter section)
(define subchapter section)
(define subsubchapter subsection)


(define (exercise . s)
  (make-paragraph plain
                  (cons (make-element 'bold "Opgave ")
                        (decode-content s))))

