;; -*- mode: scheme -*-

(library (Main foreign)
  (export argv exit)
  (import (only (rnrs base) define lambda begin)
          (prefix (rnrs programs) programs:)
          (only (purs runtime pstring) string->pstring)
          (prefix (chezscheme) scm:)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define argv
    (lambda ()
      (srfi:214:list->flexvector
        (scm:map (lambda (s) (string->pstring s)) (programs:command-line)))))

  (define exit
    (lambda (code)
      (lambda ()
        (programs:exit code))))
)
