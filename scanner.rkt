#lang racket
; Define basic token patterns
(define keywords '("DEF" "END" "IF" "THEN" "ENDIF" "WHILE" "DO" "ENDWHILE" "PRINT" "REM" "RETURN"))
; Main scanner function
(define (scan input-string)
  (define lines (string-split input-string "\n"))
  (apply append (map scan-line lines)))
; Scan a single line
(define (scan-line line)
  (define tokens '())
  (for ([word (string-split line #px"\\s+")])  ; Changed to use regexp pattern for whitespace
    (set! tokens 
          (append tokens
                  (cond
                    [(string=? word "") '()]
                    [(member (string-upcase word) keywords)
                     (list (list 'KEYWORD (string-upcase word)))]
                    [(regexp-match? #px"^[0-9]+$" word)
                     (list (list 'integer word))]
                    [(regexp-match? #px"^\".*\"$" word)
                     (list (list 'STRING word))]
                    [(equal? word ":=")
                     (list (list 'assign-op ":="))]
                    [(equal? word "+")
                     (list (list 'add-op "+"))]
                    [(equal? word "-")
                     (list (list 'sub-op "-"))]
                    [(equal? word "*")
                     (list (list 'mult-op "*"))]
                    [(equal? word ":")
                     (list (list 'colon ":"))]
                    [(equal? word ";")
                     (list (list 'semicolon ";"))]
                    [(equal? word "(")
                     (list (list 'lparen "("))]
                    [(equal? word ")")
                     (list (list 'rparen ")"))]
                    [(equal? word "<=")
                     (list (list 'le-op "<="))]
                    [(equal? word ">=")
                     (list (list 'ge-op ">="))]
                    [(equal? word "<")
                     (list (list 'lt-op "<"))]
                    [(equal? word ">")
                     (list (list 'gt-op ">"))]
                    [(equal? word "=")
                     (list (list 'eq-op "="))]
                    [else
                     (list (list 'ID word))]))))
  tokens)

(provide scan)