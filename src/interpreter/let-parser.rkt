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
  [LPAREN RPAREN COMMA EQ IN LET THEN ELSE IF EMPTYLIST])

(struct operation (name) #:transparent)

(define let-lexer
  (lexer-src-pos
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\, (token-COMMA)]
   ["if" (token-IF)]
   ["let" (token-LET)]
   [(:or "zero?" "minus" "equal?" "greater?" "less?" "cons" #\+  #\- #\* #\/ "cons" "list")
    (token-OPERATION (operation lexeme))]
   ["in" (token-IN)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["emptylist" (token-EMPTYLIST)]
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

(define number/p (syntax/p (token/p 'NUMBER)))
(define identifier/p (syntax/p (token/p 'IDENTIFIER)))
(define emptylist/p
  (syntax/p
   (do [_ <- (token/p 'EMPTYLIST)]
     (pure default-empty-list))))

(define if/p
  (syntax/p
   (do (token/p 'IF)
     [condition <- expression/p]
     (token/p 'THEN)
     [true-expr <-  expression/p]
     (token/p 'ELSE)
     [false-expr <- expression/p]
     (pure (list 'IF condition true-expr false-expr)))))

(define let/p
  (syntax/p
   (do (token/p 'LET)
     [identifier <- identifier/p]
     (token/p 'EQ)
     [expr-bind <- expression/p]
     (token/p 'IN)
     [expr-return <- expression/p]
     (pure (list 'LET identifier expr-bind expr-return)))))

(define param-tail/p
  (syntax/p
   (do (token/p 'COMMA)
     [expression <- expression/p]
     (pure expression))))

(define parameters/p
  (syntax/p
   (do (token/p 'LPAREN)
     [head <- expression/p]
     [tail <- (many/p param-tail/p)]
     (token/p 'RPAREN)
     (pure (list* head tail)))))

(define operation/p
  (syntax/p
   (do [operation <- (token/p 'OPERATION)]
     [parameters <- parameters/p]
     (pure (list operation parameters)))))

(define expression/p (or/p number/p identifier/p let/p operation/p emptylist/p))

(define (parse-let-syntax-tree src-text) (parse-result! (parse-tokens expression/p (lex-let src-text))))

(struct expression () #:transparent)

(struct ast-number expression (n) #:transparent)
(struct ast-boolean expression (b) #:transparent)
(struct ast-identifer expression (symbol) #:transparent)

(struct ast-if expression (cond true false) #:transparent)
(struct ast-in expression (identifier expression-bind expression) #:transparent)
(struct ast-operation expression (name parameters) #:transparent)
(struct ast-emptylist expression () #:transparent)

(define (to-ast tokens)
  (cond
    [(number? tokens) (ast-number tokens)]
    [(boolean? tokens) (ast-boolean tokens)]
    [(symbol? tokens) (ast-identifer tokens)]
    [(syntax? tokens) (to-ast (syntax-e tokens))]
    [(eopl-empty-list? tokens) (ast-emptylist )]
    [(list? tokens)
     (let ([first (car tokens)])
       (cond
         [(syntax? first) (to-ast (map syntax-e tokens))]
         [else (match tokens
                 [(list 'LET id value in) (ast-in (to-ast id) (to-ast value) (to-ast in))]
                 [(list 'IF exp-cond exp-then exp-else) (ast-if (to-ast exp-cond) (to-ast exp-then) (exp-else))]
                 [(list (struct operation (name)) params) (ast-operation name (map to-ast params))]
                 [_ 'error])]))]
    [else 'error]))

(define (parse source-code)
  (to-ast (parse-let-syntax-tree source-code)))

(provide
 (struct-out ast-number) (struct-out ast-boolean) (struct-out ast-identifer) (struct-out ast-if) (struct-out ast-if) (struct-out ast-in) (struct-out ast-in) (struct-out ast-operation) 
 (struct-out ast-emptylist)
 to-ast parse-let-syntax-tree parse)
