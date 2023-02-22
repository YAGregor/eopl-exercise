#lang racket
(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative)

(define-tokens basic [IDENTIFIER NUMBER TRUE FALSE])
(define-empty-tokens puct [LPAREN RPAREN COMMA EQ IN SUB IF ZERO LET THEN ELSE])

(define let-lexer
  (lexer-src-pos
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\, (token-COMMA)]
   ["if" (token-IF)]
   ["let" (token-LET)]
   ["zero?" (token-ZERO)]
   ["in" (token-IN)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   [#\= (token-EQ)]
   [#\- (token-SUB)]
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

(define diff/p
  (syntax/p
   (do (token/p 'SUB)
     (token/p 'LPAREN)
     [left-expr <- expression/p]
     (token/p 'COMMA)
     [right-expr <- expression/p]
     (token/p 'RPAREN)
     (pure (list 'SUB left-expr right-expr )))))

(define if/p
  (syntax/p
   (do (token/p 'IF)
     [condition <- expression/p]
     (token/p 'THEN)
     [true-expr <-  expression/p]
     (token/p 'ELSE)
     [false-expr <- expression/p]
     (pure (list 'IF condition true-expr false-expr)))))

(define zero/p
  (syntax/p
   (do (token/p 'ZERO)
     (token/p 'LPAREN)
     [zero-expr <- expression/p]
     (token/p 'RPAREN)
     (pure (list 'ZERO zero-expr)))))

(define let/p
  (syntax/p
   (do (token/p 'LET)
     [identifier <- identifier/p]
     (token/p 'EQ)
     [expr-bind <- expression/p]
     (token/p 'IN)
     [expr-return <- expression/p]
     (pure (list 'LET identifier expr-bind expr-return)))))

(define expression/p (or/p number/p identifier/p let/p diff/p if/p zero/p))

(define (parse-let-syntax-tree src-text) (parse-result! (parse-tokens expression/p (lex-let src-text))))

(struct expression ())

(struct let-number expression (n) #:transparent)
(struct let-boolean expression (b) #:transparent)
(struct identifer expression (symbol) #:transparent)

(struct let-if expression (cond true false))
(struct let-in expression (identifier expression-bind expression))
(struct diff expression (expression-1 expression-2))

(define (to-ast tokens)
  (cond
    [(number? tokens) (let-number tokens)]
    [(boolean? tokens) (let-boolean tokens)]
    [(symbol? tokens) (identifer tokens)]
    [(syntax? tokens) (to-ast (syntax-e tokens))]
    [(list? tokens)
     (let ([first (car tokens)])
       (cond
         [(syntax? first) (to-ast (map syntax-e tokens))]
         [else (match tokens
                 [(list 'LET id value in) (let-in (to-ast id) (to-ast value) (to-ast in))]
                 [(list 'SUB a b ) (diff (to-ast a) (to-ast b))]
                 [(list 'IF exp-cond exp-then exp-else) (let-if (to-ast exp-cond) (to-ast exp-then) (exp-else))]
                 [_ 'error])]))]
    [else 'error]
    )
  )

(provide parse-let-syntax-tree to-ast (struct-out let-number) (struct-out let-boolean)
 (struct-out identifer) (struct-out let-if) (struct-out let-in) (struct-out diff))
