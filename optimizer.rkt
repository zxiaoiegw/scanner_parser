#lang racket

(provide optimize-parse-tree)

(define (optimize-parse-tree tree)
  (cond
    [(not (list? tree)) tree]
    [(empty? tree) '()]
    [else
      (match tree
        ; Assignment statements
        [(list 'STMT id assign expr)
         (list 'STMT 
               (optimize-parse-tree id)
               (optimize-parse-tree assign)
               (optimize-expr expr))]
        ; Expression structures
        [(list 'expr rest)
         (optimize-expr-content rest)]
        ; Value simplifications
        [(list 'value val)
         (optimize-parse-tree val)]
        [(list 'value (list 'id name))
         (list 'ID name)]
        [(list 'value (list 'constant (list 'integer num)))
         (list 'integer num)]
        [(list 'constant (list 'integer num))
         (list 'integer num)]
        [(list 'id name)
         (list 'ID name)]
        ; Keep other structures unchanged
        [else (map optimize-parse-tree tree)])]))

(define (optimize-expr expr)
  (match expr
    [(list 'expr (list 'and-expr (list 'not-expr (list 'compare-expr (list 'add-expr mult op val)))))
     (list 'expr (list (optimize-value mult) op (optimize-value val)))]
    [(list 'value val)
     (optimize-parse-tree val)]
    [else expr]))

(define (optimize-expr-content expr)
  (match expr
    [(list 'and-expr (list 'not-expr (list 'compare-expr (list 'add-expr mult op val))))
     (list (optimize-value mult) op (optimize-value val))]
    [else expr]))

(define (optimize-value node)
  (match node
    [(list 'mult-expr (list 'negate-expr val))
     (optimize-parse-tree val)]
    [(list 'value val)
     (optimize-parse-tree val)]
    [(list 'value (list 'constant (list 'integer num)))
     (list 'INTEGER num)]
    [(list 'constant (list 'integer num))
     (list 'INTEGER num)]
    [else node]))