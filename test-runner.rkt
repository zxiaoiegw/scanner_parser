#lang racket

(require "scanner.rkt")
(require "parser.rkt")
(require "optimizer.rkt") ; Include the optimizer

;; Define run-test before using it
(define (run-test filename)
  (with-handlers 
      ([exn:fail? (lambda (e) 
                    (displayln (format "Error in ~a: ~a" 
                                       filename 
                                       (exn-message e))))])
    (test-file filename)))

(define (test-file filename)
  (displayln (format "\nTesting file: ~a" filename))
  (displayln "===================")
  
  (let ([input (file->string filename)])
    (displayln "Input:")
    (displayln input)
    
    (let ([tokens (scan input)])
      (displayln "\nTokens:")
      (displayln tokens)
      
      (displayln "\nParse Tree:")
      (let ([parse-tree (parse-program tokens)])
        (displayln parse-tree)
        
        (displayln "\nOptimized Parse Tree:")
        (let ([optimized-tree (optimize-parse-tree parse-tree)])
          (displayln optimized-tree))))))

;; Run all files
(for ([file '("file1.txt" "file2.txt" "file3.txt" "file4.txt")])
  (run-test file))