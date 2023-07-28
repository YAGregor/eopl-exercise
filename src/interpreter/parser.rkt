#lang racket
(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         "built-in.rkt")

(define-tokens basic [IDENTIFIER NUMBER TRUE FALSE OPERATION])
(define-empty-tokens puct
  [LPAREN RPAREN COMMA EQ IN LET THEN ELSE IF EMPTYLIST PROC])

(define let-lexer
  (lexer-src-pos
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\, (token-COMMA)]
   ["if" (token-IF)]
   ["let" (token-LET)]
   [(:or "zero?" "minus" "equal?" "greater?" "less?" #\+  #\- #\* #\/
         "cons" "list" "car" "cdr")
    (token-OPERATION (string->symbol lexeme))]
   ["in" (token-IN)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["emptylist" (token-EMPTYLIST)]
   ["proc" (token-PROC)]
   [#\= (token-EQ)]
   [(:+ (:/ #\a #\z)) (token-IDENTIFIER (string->symbol lexeme))]
   [(:+ (:/ #\0 #\9)) (token-NUMBER (string->number lexeme))]
   [(:or whitespace blank) (void)]
   [(eof) eof]))

(define (lex-let str)
  (define in (open-input-string str))
  (let loop ([v (let-lexer in)])
    (cond [(void? (position-token-token v)) (loop (let-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (let-lexer in)))])))

(define number/p 
    (do [n <- (token/p 'NUMBER)]
        (pure (ast-number n))))

(define identifier/p 
    (do [id <- (token/p 'IDENTIFIER)]
        (pure (ast-identifer id))))

(define emptylist/p
   (do [_ <- (token/p 'EMPTYLIST)]
     (pure default-empty-list)))

(define if/p
   (do (token/p 'IF)
     [condition <- expression/p]
     (token/p 'THEN)
     [true-expr <-  expression/p]
     (token/p 'ELSE)
     [false-expr <- expression/p]
     (pure (ast-if condition true-expr false-expr))))

(define let/p
   (do (token/p 'LET)
     [identifier <- identifier/p]
     (token/p 'EQ)
     [expr-bind <- expression/p]
     (token/p 'IN)
     [expr-return <- expression/p]
     (pure (ast-in identifier expr-bind expr-return))))

(define param-tail/p
   (do (token/p 'COMMA)
     [expression <- expression/p]
     (pure expression)))

(define parameters/p
   (do (token/p 'LPAREN)
     [head <- expression/p]
     [tail <- (many/p param-tail/p)]
     (token/p 'RPAREN)
     (pure (list* head tail))))

(define operation/p
   (do [op <- (token/p 'OPERATION)]
     [parameters <- parameters/p]
     (pure (ast-operation op parameters))))

(define proc/p
   (do (token/p 'PROC)
     (token/p 'LPAREN)
     [identifier <- identifier/p]
     (token/p 'RPAREN)
     (expression <- expression)
     (pure (ast-proc identifier expression))))

(define proc-call/p
   (do (token/p 'LPAREN)
     [proc <- expression/p]
     [proc-param <- expression/p]
     (token/p 'RPAREN)
     (pure (ast-proc-call proc proc-param))))

(define expression/p (or/p number/p identifier/p let/p operation/p emptylist/p
                           proc/p proc-call/p))

(define (parse-let-syntax-tree src-text) (parse-result! (parse-tokens expression/p (lex-let src-text))))

(struct expression () #:transparent)

(struct ast-number expression (n) #:transparent)
(struct ast-boolean expression (b) #:transparent)
(struct ast-identifer expression (symbol) #:transparent)

(struct ast-if expression (cond true false) #:transparent)
(struct ast-in expression (identifier expression-bind expression) #:transparent)
(struct ast-operation expression (name parameters) #:transparent)
(struct ast-emptylist expression () #:transparent)
(struct ast-proc expression (identifier expression) #:transparent)
(struct ast-proc-call expression (proc param) #:transparent)


(define (parse source-code)
  (parse-let-syntax-tree source-code))

(provide
 (struct-out ast-number) (struct-out ast-boolean) (struct-out ast-identifer) (struct-out ast-if) (struct-out ast-if) (struct-out ast-in) (struct-out ast-in) (struct-out ast-operation)
 (struct-out ast-emptylist) (struct-out ast-proc)
  parse-let-syntax-tree parse)
