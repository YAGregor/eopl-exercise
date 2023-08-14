#lang racket
(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         "built-in.rkt"
         "ast-element.rkt")

(define-tokens basic [IDENTIFIER NUMBER TRUE FALSE OPERATION])
(define-empty-tokens puct
  [LPAREN RPAREN COMMA EQ IN LET THEN ELSE IF EMPTYLIST PROC LET-REC SEMICOLON BEGIN END SET])

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
   ["set" (token-SET)]
   [#\; (token-SEMICOLON)]
   ["in" (token-IN)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["emptylist" (token-EMPTYLIST)]
   ["proc" (token-PROC)]
   [#\= (token-EQ)]
   [(:or "zero?" "minus" "equal?" "greater?" "less?" #\+  #\- #\* #\/
         "cons" "list" "car" "cdr" "not" "pair" "left" "right"
         "setref"
          "setleft" "setright"
         "newarray" "arrayref" "arrayset")
    (token-OPERATION (string->symbol lexeme))]
   [(:: (:? (:or #\+ #\-)) (:+ numeric))(token-NUMBER (string->number lexeme))]
   [(:: (:* numeric) (:+ (:or alphabetic #\-)) (:* numeric) (:* symbolic))
    (token-IDENTIFIER (string->symbol lexeme))]
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
    (pure (ast-identifier id))))

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

(define let-bind/p
  (do [identifier <- identifier/p]
    (token/p 'EQ)
    [expr-bind <- expression/p]
    (pure (cons identifier expr-bind))))

(define let/p
  (do (token/p 'LET)
    [id-exp-list <- (many+/p let-bind/p)]
    (token/p 'IN)
    [expr-return <- expression/p]
    (pure (ast-let id-exp-list expr-return))))

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
    [identifier <- (many/p identifier/p)]
    (token/p 'RPAREN)
    (expression <- expression/p)
    (pure (ast-proc identifier expression))))

(define proc-call/p
  (do (token/p 'LPAREN)
    [proc <- expression/p]
    [proc-param-list <- (many/p expression/p)]
    (token/p 'RPAREN)
    (pure (ast-proc-call proc proc-param-list))))

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


(define expression/p (or/p number/p identifier/p let/p let-rec/p operation/p emptylist/p
                           proc/p proc-call/p if/p begin-exp/p assign/p))


(define program/p
  (do [p <- expression/p]
    [_ <- eof/p]
    (pure p)))

(define (parse-let-syntax-tree src-text) (parse-result! (parse-tokens program/p (lex-let src-text))))

(define (parse source-code)
  (parse-let-syntax-tree source-code))

(provide parse)
