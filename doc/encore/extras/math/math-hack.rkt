#lang racket/base

(require 
         scribble/core
         scriblib/render-cond
         scribble/html-properties
         racket/runtime-path
         racket/path
         )

(provide enable-math)
        
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
