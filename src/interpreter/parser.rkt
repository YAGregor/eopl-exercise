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
  [LPAREN RPAREN COMMA EQ IN LET THEN ELSE IF EMPTYLIST PROC LET-REC BEGIN END SEMICOLON SET LET-MUTABLE])

(define let-lexer
  (lexer-src-pos
   [(:or whitespace blank) (void)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\, (token-COMMA)]
   ["if" (token-IF)]
   ["let" (token-LET)]
   ["letrec" (token-LET-REC)]
   ["begin" (token-BEGIN)]
   ["end" (token-END)]
   [#\; (token-SEMICOLON)]
   ["in" (token-IN)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["emptylist" (token-EMPTYLIST)]
   ["proc" (token-PROC)]
   ["set" (token-SET)]
   ["letmutable" (token-LET-MUTABLE)]
   [#\= (token-EQ)]
   [(:or "zero?" "minus" "equal?" "greater?" "less?" #\+  #\- #\* #\/
         "cons" "list" "car" "cdr" "deref" "newref")
    (token-OPERATION (string->symbol lexeme))]
   [(:: (:* numeric) (:+ alphabetic) (:* numeric) (:* symbolic))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(:: (:? (:or #\+ #\-)) (:+ numeric))(token-NUMBER (string->number lexeme))]
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
    (pure (ast-emptylist))))

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

(define let-rec-name-param-exp/p
  (do [name <- identifier/p]
    (token/p 'LPAREN)
    [identifier <- identifier/p]
    (token/p 'RPAREN)
    (token/p 'EQ)
    [expression <- expression/p]
    (pure (ast-name-param-exp name identifier expression))))

(define let-rec/p
  (do (token/p 'LET-REC)
    [name-param-exp-list <- (many/p let-rec-name-param-exp/p)]
    [token/p 'IN]
    [body <- expression/p]
    (pure (ast-let-rec name-param-exp-list body))))

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
    (expression <- expression/p)
    (pure (ast-proc identifier expression))))

(define proc-call/p
  (do (token/p 'LPAREN)
    [proc <- expression/p]
    [proc-param <- expression/p]
    (token/p 'RPAREN)
    (pure (ast-proc-call proc proc-param))))

(define begin-sentence/p
  (do (token/p 'SEMICOLON)
    [exp <- expression/p]
    (pure exp)))

(define begin-exp/p
  (do (token/p 'BEGIN)
    [first-exp <- expression/p]
    [tail-exp <- (many/p begin-sentence/p)]
    (token/p 'END)
    (pure (ast-begin (cons first-exp tail-exp)))))

(define assign/p
  (do (token/p 'SET)
    [id <- identifier/p]
    (token/p 'EQ)
    [expression <- expression/p]
    (pure (ast-assign id expression))))

(define let-mutable/p
  (do (token/p 'LET-MUTABLE)
    [id <- identifier/p]
    (token/p 'EQ)
    [bind-expression <- expression/p]
    (token/p 'IN)
    [in-expression <- expression/p]
    (pure (ast-let-mutable id bind-expression in-expression))))

(define expression/p (or/p number/p identifier/p let/p let-rec/p operation/p emptylist/p
                           proc/p proc-call/p if/p begin-exp/p assign/p let-mutable/p))

(define program/p
  (do [p <- expression/p]
    [_ <- eof/p]
    (pure p)))

(define (parse-let-syntax-tree src-text) (parse-result! (parse-tokens program/p (lex-let src-text))))

(struct expression () #:transparent)

(struct ast-number expression (n) #:transparent)
(struct ast-boolean expression (b) #:transparent)
(struct ast-identifer expression (symbol) #:transparent)

(struct ast-if expression (cond true false) #:transparent)
(struct ast-in expression (identifier expression-bind expression) #:transparent)
(struct ast-operation expression (name parameters) #:transparent)
(struct ast-emptylist expression () #:transparent)
(struct ast-proc expression (identifier-list expression) #:transparent)
(struct ast-proc-call expression (proc param) #:transparent)
(struct ast-name-param-exp (name param exp) #:transparent)
(struct ast-let-rec expression (name-param-exp-list body) #:transparent)
(struct ast-begin expression (exp-list) #:transparent)
(struct ast-assign expression (id expression))
(struct ast-let-mutable expression (identifier expression-bind expression))

(define (parse source-code)
  (parse-let-syntax-tree source-code))

(provide
 (struct-out ast-number) (struct-out ast-boolean) (struct-out ast-identifer) (struct-out ast-if) (struct-out ast-if) (struct-out ast-in) (struct-out ast-in) (struct-out ast-operation)
 (struct-out ast-emptylist) (struct-out ast-proc) (struct-out ast-proc-call) (struct-out ast-let-rec) (struct-out ast-begin) (struct-out ast-name-param-exp)
 (struct-out ast-assign) (struct-out ast-let-mutable)
 parse-let-syntax-tree parse)
