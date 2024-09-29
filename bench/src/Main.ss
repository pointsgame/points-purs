;; -*- mode: scheme -*-

(library (Main foreign)
  (export commandLine)
  (import (rnrs)
          (only (purs runtime pstring) string->pstring)
          (prefix (chezscheme) scm:))

  (define commandLine
      (lambda ()
        (begin
          (scm:map (lambda (s) (string->pstring s)) (command-line)))))
)
