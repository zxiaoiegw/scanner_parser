#lang racket

(require "scanner.rkt")

; Parse entire program
(define (parse-program tokens)
  (let loop ([remaining tokens]
             [statements '()])
    (if (empty? remaining)
        (reverse statements)
        (let-values ([(stmt rest) (parse-statement remaining)])
          (loop rest (cons stmt statements))))))

; Parse single statement
(define (parse-statement tokens)
  (let ([first (car tokens)])
    (cond
      ; Assignment statement
      [(and (equal? (car first) 'ID)
            (equal? (car (cadr tokens)) 'assign-op))
       (let-values ([(expr rest) (parse-expression (cddr tokens))])
         (values
          (list 'STMT 
                first
                (cadr tokens)
                expr)
          rest))]
      
      ; Print statement
      [(and (equal? (car first) 'KEYWORD)
            (equal? (cadr first) "PRINT"))
       (let-values ([(items rest) (parse-print-items (cdr tokens))])
         (values
          (list 'PRINT-STMT items)
          rest))]
      
      ; End statement
      [(and (equal? (car first) 'KEYWORD)
            (equal? (cadr first) "END"))
       (values
        (list 'END-STMT)
        (cdr tokens))]
      
      ; Default case
      [else (values first (cdr tokens))])))

; Parse expression with proper nesting
(define (parse-expression tokens)
  (let ([first (car tokens)])
    (if (and (> (length tokens) 2)
             (member (car (cadr tokens)) '(add-op sub-op mult-op)))
        (let-values ([(right rest) (parse-expression (cddr tokens))])
          (values
           (list 'expr 
                 (list 'and-expr
                       (list 'not-expr
                             (list 'compare-expr
                                   (list 'add-expr
                                         (list 'mult-expr
                                               (list 'negate-expr
                                                     (list 'value 
                                                           (if (equal? (car first) 'ID)
                                                               (list 'id (cadr first))
                                                               (list 'constant 
                                                                     (list 'integer (cadr first)))))))
                                         (cadr tokens)
                                         right)))))
           rest))
        (values 
         (list 'value 
               (if (equal? (car first) 'ID)
                   (list 'id (cadr first))
                   (list 'constant 
                         (list 'integer (cadr first)))))
         (cdr tokens)))))

; Parse print items
(define (parse-print-items tokens)
  (let ([first (car tokens)])
    (if (and (> (length tokens) 2)
             (equal? (car (cadr tokens)) 'semicolon))
        (let-values ([(more rest) (parse-print-items (cddr tokens))])
          (values
           (cons first more)
           rest))
        (values (list first) (cdr tokens)))))

(provide parse-program)