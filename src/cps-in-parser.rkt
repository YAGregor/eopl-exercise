#lang racket

(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         "sytanx-struct.rkt")

(define-tokens basic [IDENTIFIER NUMBER])
(define-empty-tokens
  puct
  [LPAREN RPAREN COMMA EQ IN LET THEN ELSE IF PROC LET-REC
          DIFF ZERO])

(define lexer
  (lexer-src-pos
   [(:or whitespace blank) (void )]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\, (token-COMMA)]
   [#\= (token-EQ)]
   ["in" (token-IN)]
   ["let" (token-LET)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["if" (token-IF)]
   ["proc" (token-PROC)]
   ["retrec" (token-LET-REC)]
   ["-" (token-DIFF)]
   ["zero?" (token-ZERO)]
   [(:: (:? (:or #\+ #\-)) (:+ numeric))(token-NUMBER (string->number lexeme))]
   [(:: (:* numeric) (:+ (:or alphabetic #\-)) (:* numeric) (:* symbolic))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(eof) eof]))

(define number/p
  (do [n <- (token/p 'NUMBER)] (pure n)))

(define identifier/p
  (do [id <- (token/p 'IDENTIFIER)]
    (pure id)))

(define let/p
  (do (token/p 'LET)
    [id <- identifier/p]
    (token/p 'EQ)
    [bind-exp <- expression/p]
    (token/p 'IN)
    [body <- expression/p]
    (pure (let-exp id bind-exp body))))

(define if/p
  (do (token/p 'IF)
    [cond <- expression/p]
    (token/p 'THEN)
    [true-exp <- expression/p]
    (token/p 'ELSE)
    [false-exp <- expression/p]
    (pure (if-exp cond true-exp false-exp))))

(define id-list-tail/p
  (do (token/p 'COMMA)
    [id <- identifier/p]
    (pure id)))

(define id-list/p
  (do [id <- identifier/p]
    [tail-id-list <- (many/p id-list-tail/p)]
    (pure (list* id tail-id-list))))

(define proc/p
  (do (token/p 'PROC)
    (token/p 'LPAREN)
    [id-list <- id-list/p]
    (token/p 'RPAREN)
    [body <- expression/p]
    (pure (proc-exp id-list body))))

(define proc-call/p
  (do (token/p 'LPAREN)
    [rator <- expression/p]
    [rands <- (many/p expression/p)]
    (token/p 'RPAREN)
    (pure (call-exp rator rands))))

(define letrec/p
  (do (token/p 'LET-REC)
    [id <-  identifier/p]
    (token/p 'LPAREN)
    [id-list <- id-list/p]
    (token/p 'RPAREN)
    (token/p 'EQ)
    [body <- expression/p]
    (token/p 'IN)
    [in <- expression/p]
    (let-rec-exp id id-list body in)))

(define diff/p
  (do (token/p 'DIFF)
    (token/p 'LPAREN)
    [exp1 <- expression/p]
    (token/p 'COMMA)
    [exp2 <- expression/p]
    (token/p 'RPAREN)
    (pure (diff-exp exp1 exp2))))

(define zero-exp/p
  (do (token/p 'ZERO)
    (token/p 'LPAREN)
    [z-exp <- expression/p]
    (token/p 'RPAREN)
    (pure (zero?-exp z-exp))))

(define expression/p (or/p number/p identifier/p let/p if/p proc/p proc-call/p letrec/p diff/p zero-exp/p))

(define program/p
  (do [p <- expression/p]
    [_ <- eof/p]
    (pure p)))

(define (lex string)
  (define in (open-input-string string))
  (let loop ([v (lexer in)])
    (cond [(void? (position-token-token v)) (loop (lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (lexer in)))])))

(define (parse source-code)
  (parse-result! (parse-tokens program/p (lex source-code))))

(provide parse)
