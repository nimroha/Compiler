;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "pc.scm")
(load "pattern-matcher.scm")

;(define fold-left
;  (lambda (proc init lst)
;    (if (null? lst)
;        init
;        (fold-left proc
;                  (proc init (car lst))
;                  (cdr lst)))))
;
;(define fold-right
;  (lambda (proc init lst)
;    (fold-left proc init (reverse lst))))
    

(define foldl fold-left)
(define foldr fold-right)

(define empty '())

(define (flatten lst)
  (cond 
    [(null? lst) empty]
    [(= (length lst) 0) empty]
    [(not (list? lst)) (if (pair? lst)
                           `(,(car lst) ,(cdr lst))
                           lst)]
    [(list? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst)))]
    [else
     (cons (car lst) (flatten (cdr lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Number;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <digit-0-9> 
  (range #\0 #\9))

(define <digit-1-9> 
  (range #\1 #\9))

(define <Natural>
  (new  (*parser (char #\0))
        *star
        (*parser <digit-1-9> )
        (*parser <digit-0-9>) *star
        (*caten 3) 
        (*pack-with (lambda (zeros x y) (string->number
                                         (list->string `(,x ,@y)))))
        
        (*parser  (char #\0)) 
        (*pack (lambda (_) 0))
        
        (*disj 2)
        done))

(define <Integer>
  (new(*parser (char #\+))
      (*parser <Natural>)
      (*caten 2)
      (*pack-with (lambda (x y) y)) 
      
      (*parser (char #\-))
      (*parser <Natural>)
      (*caten 2)
      (*pack-with (lambda (x y) (- y))) 
      
      (*parser <Natural>) 
      
      (*disj 3)
      done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
        (lambda (num div den)
          (/ num den)))
       done))

(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       
       (*disj 2)
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Boolean>
  (new (*parser (char #\#))
       
       (*parser (char-ci #\t))
       (*pack (lambda (t) #t))
       (*parser (char-ci #\f))
       (*pack (lambda (f) #f))
       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (hash val) val))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CHAR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       done))

(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*guard (lambda (n)
                 (< (char->integer #\space) (char->integer n))))
       (*pack (lambda (char) char))
       done))

(define ^<named-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
         (*pack (lambda (_) ch))
         done)))

(define <NamedChar>
  (new (*parser (^<named-char> "lambda" (integer->char 955)))
       (*parser (^<named-char> "newline" #\newline))
       (*parser (^<named-char> "nul" #\nul))
       (*parser (^<named-char> "page" #\page))
       (*parser (^<named-char> "return" #\return))
       (*parser (^<named-char> "space" #\space))
       (*parser (^<named-char> "tab" #\tab))
       (*disj 7)
       
       done))

(define <hex-dig>
  (let ((zero (char->integer #\0))
        (lc-a (char->integer #\a))
        (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
         (*pack
          (lambda (ch)
            (- (char->integer ch) zero)))
         
         (*parser (range #\a #\f))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) lc-a))))
         
         (*parser (range #\A #\F))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) uc-a))))
         
         (*disj 3)
         done)))

(define <HexChar>
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\f))
       (*parser (range #\A #\F))
       (*disj 3)
       done))


(define <HexUnicodeChar>  
  (new (*parser (word-ci "x"))
       
       (*parser <hex-dig>)
       *plus
       
       (*parser (char #\;))
       *maybe
       
       (*caten 3)
       (*pack-with (lambda (x chars semi)
                     (letrec ((build-hex (lambda (total lst)
                                           (if (null? lst)
                                               total
                                               (build-hex (+ (* total 16) (car lst)) (cdr lst))))))
                       (let ((n (build-hex 0 chars)))
                         (if (> n 1114111)
                             1114112
                             (integer->char n))
                         ))))
       done))

(define <Char>
  (new (*parser <CharPrefix>)
       
       (*parser <HexUnicodeChar>)
       (*guard (lambda (char)
                 (not (eq? char 1114112))))
       
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)      
       (*disj 3)
       
       (*delayed (lambda () <SymbolChar>)) ; TODO is this enough?
       *not-followed-by
       
       (*caten 2)
       (*pack-with (lambda (pre char) char))
       done))


(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
         (*pack (lambda (_) ch))
         done)))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       
       (*disj 6)
       done))	 

(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser <HexUnicodeChar>)
       (*guard (lambda (char)
                 (not (eq? char 1114112))))
       
       (*caten 2)
       (*pack-with (lambda (slash char)
                     char))
       done))

(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*guard (lambda (x)
                 (not (eq? x #\\))))
       done))

(define <StringChar>
  (new (*parser <StringMetaChar>)
       (*parser <StringHexChar>)
       (*parser <StringLiteralChar>)
       (*disj 3)
       
       (*parser (char #\"))
       
       *diff
       done))


(define <String>
  (new (*parser (char #\"))
       
       (*parser <StringChar>)
       *star
       
       (*parser (char #\"))
       
       (*caten 3)
       (*pack-with (lambda (del1 chars del2)
                     (list->string (flatten chars))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SYMBOL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <special-char>
  (new (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\/))
       (*parser (char #\!))
       (*parser (char #\^))
       (*parser (char #\_))
       (*parser (char #\*))
       (*parser (char #\+))
       (*parser (char #\-))
       (*parser (range #\< #\?))
       (*disj 10)
       done))

(define <SymbolChar>
  (new (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*parser <special-char>)
       (*disj 3)
       (*pack (lambda (char)
                (let ((val  (char->integer char))
                      (uc-a (char->integer #\A))
                      (uc-z (char->integer #\Z))
                      (diff-case (- (char->integer #\A) (char->integer #\a))))
                  (if (and (>= val uc-a) (<= val uc-z))
                      (integer->char (- val diff-case))
                      char))))
       done))

(define <not-symbol>
  (new (*parser <Number>)
       (*parser <Fraction>)
       (*disj 2)
       
       (*parser <SymbolChar>)
       *not-followed-by
       done))


(define <Symbol> 
  (new (*parser <SymbolChar>)
       (*parser <digit-0-9>) 
       (*disj 2)
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       *plus
       (*caten 2)
       
       (*parser <not-symbol>)
       *diff
       (*pack-with (lambda (char lst)
                     (string->symbol (list->string (append (list char) lst)))))
       
       (*parser <SymbolChar>)
       (*delayed (lambda () <Symbol>))
       *not-followed-by
       (*pack (lambda (char)
                (string->symbol (list->string (list char)))))
       (*disj 2)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ProperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ProperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_x exp _y)
                     `(,@exp)))
       
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <sexpr>))
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda (_p1 exp1 _dot exp2 _p2)
                     `( ,@exp1 . ,exp2)))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *star
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (_x _y exp _z)
                     (list->vector `( ,@exp))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Quoted>
  (new (*parser (char #\'))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quote exp)))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quasiquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Unquoted>
  (new (*parser (char #\,))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'unquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*delayed (lambda () <sexpr>))
       (*caten 3)
       (*pack-with (lambda (_x _y exp )
                     (list 'unquote-splicing exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mayer  COMMENT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    
    (new (*parser (char #\;))
         
         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star
         
         (*parser <end-of-line-comment>)
         (*caten 3)
         done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda ()  <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (new (*parser <line-comment>)
       (*parser <sexpr-comment>)
       (*disj 2)
       done))

(define <skip>
  (new (*parser <comment>)
       (*parser <whitespace>)
       (*disj 2)
       done))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INFIX COMMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <infix-comment>
  (new (*parser <line-comment>)
       
       (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       
       (*disj 2)
       done))


(define <infix-skip>
  (new (*parser <infix-comment>)
       (*parser <whitespace>)
       (*disj 2)
       done))



(define ^<infix-skipped*> (^^<wrapped> (star <infix-skip>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INFIX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <operator-symbol>
  (new (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\+))
       (*parser (char #\^))
       (*parser (char #\/))
       (*parser (word "**"))
       (*disj 6)
       done))

(define <InfixSymbol>
  (new (*parser <digit-0-9>)
       *star
       
       (*parser <SymbolChar>)
       (*parser <operator-symbol>)
       *diff
       
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       (*parser <operator-symbol>)
       *diff
       *star
       
       (*caten 3)
       (*pack-with (lambda args
                     (string->symbol (list->string (flatten args)))))
       done))

(define <PowerSymbol>
  (^<infix-skipped*>
   (new (*parser (char #\^))
        (*parser (word "**"))
        (*disj 2)
        done)))     



(define <InfixPrefixExtensionPrefix>
  (new (*parser (word "##"))
       (*parser (word "#%"))
       (*disj 2)
       done))


;;;;;;;;;;;;;;;;;;;;;;;;;Infix Op;;;;;;;;;;;;;;;;;

(define <InfixPow>
  (^<infix-skipped*>
   (new 
    (*delayed (lambda () <InfixFuncall>))
    
    (*parser <PowerSymbol>) 
    (*delayed (lambda () <InfixFuncall>))  
    (*caten 2)
    (*pack-with (lambda (sign exp) exp))
    *star
    
    (*caten 2)
    (*pack-with (lambda (firstExp restExp) 
                  (letrec ((lst  `(,firstExp ,@restExp))
                           (res (lambda (resLst)
                                  (if (= (length resLst)1) (car resLst)
                                      `(expt ,(car resLst) ,(res (cdr resLst)))))))
                    
                    (res lst))))
    
    
    done)))



(define <InfixDiv-Mul>
  (new (*delayed (lambda () <InfixNeg>))
       
       (*parser (char #\/))
       (*parser (char #\*))
       (*disj 2)
       (*pack (lambda (x)
                (string->symbol 
                 (list->string `(,x))))) 
       
       (*delayed (lambda () <InfixNeg>))
       (*caten 2) 
       (*pack-with (lambda (sign exp)(cons sign exp)))
       *star
       
       (*caten 2)
       (*pack-with (lambda (firstExp restExp) 
                     (letrec ((lst (reverse `(,firstExp ,@restExp)))
                              (res (lambda (resLst)
                                     (if (= (length  resLst) 1) (car resLst)
                                         `(,(car (car resLst)) ,(res (cdr resLst)) ,(cdr (car resLst)))))))
                       (res lst))))
       
       done))


(define <InfixSub-Add>
  (new (*parser <InfixDiv-Mul>)
       
       (*parser (char #\-))
       (*parser (char #\+))
       (*disj 2)
       (*pack (lambda (x)
                (string->symbol 
                 (list->string `(,x))))) 
       
       (*parser <InfixDiv-Mul>) 
       (*caten 2) 
       (*pack-with (lambda (sign exp)(cons sign exp)))
       *star
       
       (*caten 2)
       (*pack-with (lambda (firstExp restExp) 
                     (letrec ((lst (reverse `(,firstExp ,@restExp)))
                              (res (lambda (resLst)
                                     (if (= (length  resLst) 1) (car resLst)
                                         `(,(car (car resLst)) ,(res (cdr resLst)) ,(cdr (car resLst)))))))
                       (res lst))))
       
       
       done))


(define <InfixNeg>
  (new (*parser <InfixPow>)
       
       (*parser (char #\-))
       (*delayed (lambda () <InfixPow>))
       (*caten 2)
       (*pack-with (lambda (op exp)
                     `(- ,exp)))
       (*disj 2)
       done))



(define <InfixArrayGet>
  (new (*delayed (lambda () <InfixParen>))
       (*parser <whitespace>) *star
       (*parser (char #\[))    
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\]))
       (*parser <whitespace>) *star
       (*caten 5)
       (*pack-with (lambda (_x _z exp _y _w) exp))
       *star
       
       (*caten 2)
       (*pack-with (lambda (exp exps)
                     (letrec ((lst (reverse `(,exp ,@exps)))
                              (ret (lambda (result)
                                     (if (= (length result) 1) (car result)
                                         `(vector-ref ,(ret (cdr result)) ,(car result))))))
                       (ret lst))))
       done))



(define <InfixArgList>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\,))
       (*caten 2)
       (*pack-with (lambda (arg comma) arg))
       *star
       
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with (lambda (args arg)
                     (append args (list arg))))
       
       (*delayed (lambda () <InfixExpression>))
       
       (*parser <epsilon>)
       (*disj 3)
       done))

(define <InfixFuncall>
  (new (*delayed (lambda ()  (^<infix-skipped*> <InfixArrayGet>)))
       (*parser (char #\())
       (*parser (^<infix-skipped*> <InfixArgList>))
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (f p1 args p2)
                     `(,f ,@args)))
       
       (*delayed (lambda () (^<infix-skipped*> <InfixArrayGet>)))
       (*disj 2)
       done))

(define <InfixParen>
  (new (*parser <InfixSymbol>)
       (*parser <Number>)
       (*delayed (lambda () <InfixSexprEscape>))
       
       
       (*parser (char #\())
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (p1 exp p2) exp))
       
       (*disj 4)
       done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (pre exp)
                     exp))      
       done))


(define <InfixExpression>
  (^<infix-skipped*>
   <InfixSub-Add>))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (pre exp) exp)) ;TODO is this how we want to pass this?
       
       done)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEXPR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <sexpr>
  (^<skipped*>
   (new  
    (*parser <Boolean>)
    
    (*parser <Char>)
    (*parser <Number>)
    *not-followed-by
    
    (*parser <Number>)
    (*parser <Symbol>)
    *not-followed-by
    
    (*parser <Symbol>)
    (*parser <String>)
    (*parser <ProperList>)
    (*parser <ImproperList>)
    (*parser <Vector>)
    (*parser <Quoted>)
    (*parser <QuasiQuoted>)
    (*parser <Unquoted>)
    (*parser <UnquoteAndSpliced>)
    (*parser <InfixExtension>)
    (*disj 13)
    done)))

(define <Sexpr> <sexpr>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))



(define var?
  (lambda (x)
    (and (symbol? x) (not (member x *reserved-words*)))))

(define *void-object* (void));

(define void?
  (lambda (x) (eq? *void-object* x)))

(define empty? null?)

(define valid?
  (lambda (args)
    (letrec ((not-contain-duplicates
              (lambda (lst) 
                (if (null? lst) #t
                    (and (not (member (car lst) (cdr lst))) (not-contain-duplicates (cdr lst))))))
             (not-contain-duplicates-not-list (lambda (lst acc) 
                                                (if (not (pair? lst)) (and (var? lst)(and (not (member  lst acc))))
                                                    (and (not (member (car lst) acc)) (not-contain-duplicates-not-list (cdr lst) (cons (car lst) acc))))
                                                ))
             )
      (cond ((or (null? args) (var? args)) #t)
            ((list? args) (and (not-contain-duplicates args) (andmap var? args)))
            (else (not-contain-duplicates-not-list args '()))
            )
      )))


(define first car)
(define second cadr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; const pattern rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Null
(define pattern-rule-nil
  (pattern-rule `() (lambda ()`(const ()))))

;;Number
(define pattern-rule-number
  (pattern-rule (? 'x number?) (lambda (e) `(const ,e))))

;;Boolean
(define pattern-rule-boolean
  (pattern-rule (? 'x boolean?) (lambda (e) `(const ,e))))

;;Char
(define pattern-rule-char
  (pattern-rule (? 'x char?) (lambda (e) `(const ,e))))

;;Stirng
(define pattern-rule-string
  (pattern-rule (? 'x string?) (lambda (e) `(const ,e))))

;;vector
(define pattern-rule-vector
  (pattern-rule (? 'x vector?) (lambda (e) `(const ,e))))

;;void
(define pattern-rule-void
  (pattern-rule (? 'p void?) (lambda (e) `(const ,e))))

;;Fail - ToDo: delete
(define fail!
  (lambda () "ERROR"))

;;;;;;;;;;;;;;;;;;;;;;;; quote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-quotes
  (pattern-rule `(quote ,(? 'x))
                (lambda (e) `(const ,e))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;; quasiquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-quasiquote
  (pattern-rule (list 'quasiquote (? 'x)) (lambda (e) (tag-parser (expand-qq e)))))

(define pattern-rule-quasiquote-not-needed
  (compose-patterns
   (pattern-rule (list 'unquote (? 'x))
                 (lambda (e)  e))
   
   (pattern-rule `(,(list 'unquote-splicing (? 'x)) . ,(? 'y))
                 (lambda (e1 e2) `(append ,e1 ,(expand-qq e2))))
   
   (pattern-rule `(,(? 'x) . ,(list  'unquote-splicing (? 'y)))
                 (lambda (e1 e2) `(cons ,(expand-qq e1) ,e2)))
   
   (pattern-rule `(,(? 'x) . ,(? 'y))
                 (lambda (e1 e2) `(cons ,(expand-qq e1) ,(expand-qq e2))))
   
   (pattern-rule (? 'x vector?)
                 (lambda (e) vector-map expand-qq e))
   
   (pattern-rule (? 'x (lambda(a)(or (null? a) (symbol? a))))
                 (lambda (e) `',e))
   
   (pattern-rule (? 'x)
                 (lambda (e) e))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;mayer quasiquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
                 simple-sexprs-predicates)
          (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
            (pair? e)
            (symbol? e)
            (vector? e))
        `',e
        e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
        (cadr e)
        e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
         (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
            (lambda (e)
              (cond ((unquote? e) (cadr e))
                    ((unquote-splicing? e)
                     (error 'expand-qq
                            "unquote-splicing here makes no sense!"))
                    ((pair? e)
                     (let ((a (car e))
                           (b (cdr e)))
                       (cond ((unquote-splicing? a)
                              `(append ,(cadr a) ,(expand-qq b)))
                             ((unquote-splicing? b)
                              `(cons ,(expand-qq a) ,(cadr b)))
                             (else `(cons ,(expand-qq a) ,(expand-qq b))))))
                    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
                    ((or (null? e) (symbol? e)) `',e)
                    (else e))))
           (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
           (optimizer
            (compose-patterns
             (pattern-rule
              `(append ,(? 'e) '())
              (lambda (e) (optimize-qq-expansion e)))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
              (lambda (c1 c2)
                (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
                  c)))
             (pattern-rule
              `(append ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  `(append ,e1 ,e2))))
             (pattern-rule
              `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify (list (unquotify c1) (unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(cons ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  (if (and (const? e1) (const? e2))
                      (quotify (cons (unquotify e1) (unquotify e2)))
                      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Father of all const;;;;;;;;;;;;;;;;;;;;;;
(define compose-patterns-const
  (compose-patterns 
   pattern-rule-nil
   pattern-rule-number
   pattern-rule-boolean
   pattern-rule-quotes
   pattern-rule-char
   pattern-rule-string
   pattern-rule-vector
   pattern-rule-void
   
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-var 
  (pattern-rule (? 'v var?)
                (lambda (v) `(var ,v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; if ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define pattern-rule-if3
  (pattern-rule `(if ,(? 'test) ,(? 'true) ,(? 'false))
                (lambda (test true false)
                  `(if3 ,(tag-parser test) ,(tag-parser true) ,(tag-parser false)))))

(define pattern-rule-if2
  (pattern-rule `(if ,(? 'test) ,(? 'true))
                (lambda (test true)
                  `(if3 ,(tag-parser test) ,(tag-parser true) ,(tag-parser (void))))))

(define compose-patterns-if
  (compose-patterns
   pattern-rule-if3
   pattern-rule-if2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-cond0
  (pattern-rule `(cond  ,(? 'first empty?)) (lambda(e) (tag-parser *void-object*))))

(define pattern-rule-cond3
  (pattern-rule `(cond  ,(? 'condlst) . ,(? 'res)) (lambda (e1 e2) (tag-parser (cond->if e1 e2)))))

(define pattern-rule-cond1
  (pattern-rule `(cond  ,(? 'first)) (lambda(e) (tag-parser (cond->if  e '())))))

(define cond->if
  (lambda ( e1 e2)
    (cond ((null? e2) (if  (eq? (car e1) 'else)
                           `(begin ,@(cdr e1)) 
                           `(if ,(first e1) (begin ,@(cdr e1))))
                      )
          (`(if ,(first e1) (begin ,@(cdr e1)) ,(cond->if (car e2) (cdr e2))))          
          )))

(define compose-patterns-cond
  (compose-patterns
   ;pattern-rule-cond0
   pattern-rule-cond3
   ;pattern-rule-cond1
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-and
  (pattern-rule `(and  ,(? 'args) . ,(? 'res)) (lambda(e1 e2) (tag-parser (and->if (append (list e1) e2))))))

(define pattern-rule-and0
  (pattern-rule `(and) (lambda () (tag-parser #t))))

(define make-if
  (lambda (predicate consequent alternative)
    `(if  ,predicate ,consequent ,alternative)))

(define and->if
  (lambda (args)
    (letrec ((and-if-helper (lambda (lst)                             
                              (if (empty? lst) '#t
                                  (if (empty? (cdr lst)) (car lst)
                                      (make-if (car lst) (and-if-helper (cdr lst)) '#f))))))
      (and-if-helper args))))

(define compose-patterns-and
  (compose-patterns
   pattern-rule-and
   pattern-rule-and0
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-or0
  (pattern-rule `(or ,@(? 'exp-list null?))
                (lambda (exp-list)(tag-parser #f))))

(define pattern-rule-or
  (pattern-rule `(or ,@(? 'exp-list))
                (lambda (exp-list)
                  (cond ((= (length exp-list) 1) (tag-parser (car exp-list)))
                        (else `(or ,(map tag-parser exp-list)))
                        ))))

(define compose-patterns-or
  (compose-patterns
   pattern-rule-or0
   pattern-rule-or
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lambda forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define identify-lambdas
  (lambda (arg)
    (letrec ((identify
              (lambda (argl ret-simple ret-opt ret-var)
                (cond 
                  ((null? argl) (ret-simple '()))
                  ((var? argl) (ret-var argl))    
                  (else (identify (cdr argl)
                                  (lambda (s) (ret-simple `(,(car argl) ,@s)))
                                  (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
                                  (lambda (var) (ret-opt `(,(car argl)) var))))))))
      
      (identify arg (lambda (argl) `(lambda-simple ,argl)) (lambda (argl res) `(lambda-opt ,argl ,res)) (lambda (argl) `(lambda-var ,argl))))))

(define pattern-rule-lambda
  (pattern-rule `(lambda ,(? 'argl) . ,(? 'exp)) (lambda (argl exp)
                                                   (cond ((null? argl) `(,@(identify-lambdas argl) ,(tag-parser (cons 'begin exp))))
                                                         ((not (valid? argl)) (error 'ERROR "ERROR"))
                                                         (else `(,@(identify-lambdas argl) ,(tag-parser (cons 'begin exp))))
                                                         ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-def-MIT
  (pattern-rule `(define ,(? 'def (lambda (x)
                                    (or (list? x) (pair? x))))
                   ,(? 'body))
                (lambda (def body)
                  `(def ,(tag-parser (car def)) ,(tag-parser `(lambda ,(cdr def) ,body))))))

(define pattern-rule-def-MIT-list
  (pattern-rule `(define ,(? 'def (lambda (x)(or (list? x) (pair? x)))) .  ,(? 'body))
                (lambda (def body)
                  `(def ,(tag-parser (car def)) ,(tag-parser `(lambda ,(cdr def) ,(append '(begin) body))))))) 				  

(define pattern-rule-def-REG
  (pattern-rule `(define ,(? 'v var?) ,(? 'e))
                (lambda (v e)
                  `(def ,(tag-parser v) ,(tag-parser e)))))


(define pattern-rule-def-REG-list
  (pattern-rule `(define ,(? 'v var?) . ,(? 'e list?)) (lambda (v e) `(def ,(tag-parser v) ,(tag-parser (append '(begin) e))))))

(define compose-patterns-define
  (compose-patterns
   pattern-rule-def-REG
   pattern-rule-def-REG-list
   pattern-rule-def-MIT
   pattern-rule-def-MIT-list
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-applic
  (pattern-rule `(,@(? 'app (lambda (app)
                              (and (list? app)
                                   (not (empty? app))
                                   (not (member (car app) *reserved-words*))))))
                (lambda e
                  (let ((exps (car e)))
                    (if (empty? (cdr exps))
                        `(applic ,(tag-parser (car exps)) ())
                        `(applic ,(tag-parser (car exps)) ,(map tag-parser (cdr exps)))
                        )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define begin-killer
  (lambda (exps)
    (let* ((begin? (lambda (e) (eq? 'begin e)))
           (beginless-exps (fold-right (lambda (exp acc)
                                         (if (and (list? exp) (begin? (car exp))) (append (cdr exp) acc) (cons exp acc)) )'() exps))
           )
      (if (equal? exps beginless-exps) beginless-exps (begin-killer beginless-exps))
      )))



(define pattern-rule-exp-seq0 
  (pattern-rule `(begin ,@(? 'seq null?))
                (lambda (s)(tag-parser *void-object*))))

(define pattern-rule-exp-seq 
  (pattern-rule `(begin ,@(? 'seq list?))
                (lambda (s)
                  (cond ((= (length s) 1) (tag-parser (car s)))
                        (else `(seq ,(map tag-parser  (begin-killer s))))
                        ))))

(define pattern-rule-imp-seq 'not-implemented)

(define compose-patterns-seq
  (compose-patterns
   pattern-rule-exp-seq0
   pattern-rule-exp-seq
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-set 
  (pattern-rule `(set! ,(? 'var var?) ,(? 'val)) (lambda (var val) `(set ,(tag-parser var) ,(tag-parser val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define val-list (lambda (lst)
                   (if (null? lst) lst
                       (cons (second (first lst)) (val-list (cdr lst))))))
(define var-list (lambda (lst)
                   (if (null? lst) lst
                       (cons (car (car lst)) (var-list (cdr lst))))))

(define pattern-rule-let0
  (pattern-rule `(let ,(? 'lst list? empty?) ,(? 'body) )
                (lambda (lst body) 
                  (tag-parser `((lambda () , body))))
                ))

(define pattern-rule-let1
  (pattern-rule `(let ((,(? 'id var?) ,(? 'val) )) ,(? 'body) )
                (lambda (id val body) 
                  (tag-parser `((lambda (,id) ,body) ,val ))
                  )))

(define pattern-rule-let+
  (pattern-rule `(let ,(? 'lst list?) . ,(? 'body) )
                (lambda (lst body) 
                  (letrec (
                           (vars (var-list lst))
                           (vals (val-list lst)))
                    (tag-parser `((lambda (,@vars) ,@body) ,@vals)))
                  )))

(define pattern-rule-let
  (compose-patterns
   pattern-rule-let0
   pattern-rule-let1
   pattern-rule-let+
   ))      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-let*0
  (pattern-rule `(let* ,(? 'lst list? empty?) ,(? 'body) )
                (lambda (lst body) 
                  (tag-parser `(let () ,body)))))


(define pattern-rule-let*+
  (pattern-rule `(let* ,(? 'lst list?) . ,(? 'body) )
                (lambda (lst body) 
                  (letrec ((let*->let (lambda(lst body)
                                        (cond ((null? lst) `(let () (begin ,@body)))
                                              ((null? (cdr lst)) `(let (,(car lst)) (begin ,@body)))
                                              (`(let (,(car lst)) ,(let*->let (cdr lst) body)))))))
                    (tag-parser (let*->let lst body))))))

(define pattern-rule-let*
  (compose-patterns
   pattern-rule-let*0
   pattern-rule-let*+
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; letrec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define box-list (lambda (vars vals)
                   (if (null? vars) '()
                       (cons `(set! ,(car vars) ,(car vals))(box-list(cdr vars)(cdr vals))))))

(define fals-list (lambda (lst)
                    (if (null? lst) lst
                        (cons #f (fals-list (cdr lst))))))							  

(define pattern-rule-letrec0
  (pattern-rule `(letrec ,(? 'lst list? empty?) . ,(? 'body) )
                (lambda (lst body)
                  (let* (
                         (vars (var-list lst))
                         (vals (val-list lst))
                         (boxs (box-list vars vals))
                         (falses (fals-list vals))
                         )
                    (tag-parser `((lambda (,@vars) (begin  ((lambda () ,@body)))),@falses))))))

(define pattern-rule-letrec+
  (pattern-rule `(letrec ,(? 'lst list?) . ,(? 'body) )
                (lambda (lst body)
                  (let* (
                         (vars (var-list lst))
                         (vals (val-list lst))
                         (boxs (box-list vars vals))
                         (falses (fals-list vals))
                         )
                    (tag-parser `((lambda (,@vars) (begin ,@boxs ((lambda () ,@body)))),@falses))))))


(define pattern-rule-letrec
  (compose-patterns
   pattern-rule-letrec0
   pattern-rule-letrec+
   ))
;;;;;;;;;;;;;;;;;; tag-parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define error?
  (lambda (exp)
    (eq? exp "ERROR")))

(define tag-parser
  (let ((run 
         (compose-patterns 
          pattern-rule-set
          compose-patterns-if
          compose-patterns-cond
          compose-patterns-and
          compose-patterns-or
          compose-patterns-define
          compose-patterns-seq
          pattern-rule-applic      
          pattern-rule-lambda
          pattern-rule-let
          pattern-rule-let*
          pattern-rule-letrec
          pattern-rule-quasiquote
          pattern-rule-var
          compose-patterns-const
          )))
    (lambda (sexp)(run sexp (lambda () (fail!))))))

(define parse tag-parser)
;;;;;;;;;;;;;;;;;;;;;;;; end compiler.scm HW2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elliminate nested defines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lambda-words '(lambda-simple lambda-opt lambda-var))

(define ellim-internal
  (lambda (seq)
    (cond [(eq? (car seq) 'seq) (if (not (eq? (caaadr seq) 'def))
                                    seq
                                    (let* ((defs (map (lambda (x) `(set ,@(cdr x))) (filter (lambda (e) (eq? (car e) 'def)) (cadr seq))))
                                           (vars (map (lambda (e) (cadadr e)) defs))
                                           (rest (filter (lambda (e) (not (eq? (car e) 'def))) (cadr seq)))
                                           (init (make-list (length defs) '(const #f))))
                                      `(applic (lambda-simple ,vars (seq ,(append defs rest))) ,init)))]
          [(eq? (car seq) 'def) `(applic (lambda-simple ,(cdadr seq) (set ,(cdr seq))) ((const #f)))]
          [else seq])))

(define eliminate-nested-defines
  (lambda (exp) 
    (cond [(not (list? exp)) exp]
          [(null? exp) exp]
          [(member (car exp) lambda-words) (if (eq? (car exp) 'lambda-opt)
                                               `(,(car exp) ,(cadr exp) ,(caddr exp)
                                                            ,(eliminate-nested-defines (ellim-internal (cadddr exp))))
                                               
                                               `(,(car exp) ,(cadr exp)
                                                            ,(eliminate-nested-defines (ellim-internal (caddr exp)))))]
          
          [else (map (lambda (sub-exp) (eliminate-nested-defines sub-exp)) exp)]
          )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remove-applic-lambda-nil ;;;;;;;;;;;;;;;;;;;;;;;
(define empty-lambda?
  (lambda (e)
    (eq? #t ((pattern-rule `(applic (lambda-simple ()  . ,(? 'exp)) ()) (lambda (exp) #t)) e fail!))))

(define empty-lambda-pattern
  (pattern-rule `(applic (lambda-simple ()  . ,(? 'exp)) ()) (lambda (exp) (car exp))))

(define remove-applic-lambda-nil
  (lambda (exps)
    (letrec ((empty-lambda->exp (lambda (e) (empty-lambda-pattern e fail!)))
             (remove-lambda-empty (lambda (e)
                                    (cond((empty-lambda? e) (remove-lambda-empty (empty-lambda->exp e)))
                                         ((null? e) '())
                                         ((list? e) (cons (remove-lambda-empty (car e)) (remove-lambda-empty (cdr e))))
                                         (else e)
                                         ))))
      
      (remove-lambda-empty exps)
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lambda-simple-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exp)) (lambda (args exp) #t)) e fail!))
    ))

(define lambda-simple->args
  (lambda (e)
    ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exps)) (lambda (args exps) args)) e fail!)
    ))

(define lambda-simple->exps
  (lambda (e)
    ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exps)) (lambda (args exps) exps)) e fail!)
    ))

(define lambda-var-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exp)) (lambda (args exp) #t)) e fail!))
    ))

(define lambda-var->args
  (lambda (e)
    ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exps)) (lambda (args exps) args)) e fail!)
    ))

(define lambda-var->exps
  (lambda (e)
    ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exps)) (lambda (args exps) exps)) e fail!)
    ))

(define lambda-opt-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exp) #t)) e fail!))
    ))

(define lambda-opt->args
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  args )) e fail!)
    ))

(define lambda-opt->rest
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  rest )) e fail!)
    ))

(define lambda-opt->args-and-rest
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  (append args (list rest)))) e fail!)
    ))

(define lambda-opt->exps
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps) exps)) e fail!)
    ))
(define var-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(var ,(? 'var)) (lambda (var) #t)) e fail!))
    ))

(define var->exp
  (lambda (e)
    ((pattern-rule `(var ,(? 'var)) (lambda (var) var)) e fail!)
    ))

(define index-of-arg
  (lambda (arg args)
    (letrec ((get-index (lambda (lst acc)
                          (if (eq? arg (car lst)) acc
                              (get-index (cdr lst) (+ 1 acc)))
                          )))
      (get-index args 0))
    ))

(define contain-in-previus-args?
  (lambda (e lst)
    (if (null? lst) #f
        (or (eq? e (caar lst))  (contain-in-previus-args? e (cdr lst))))
    ))

(define get-var-place
  (lambda (e lst)
    (if (eq? e (caar lst)) (cadar lst)
        (get-var-place e (cdr lst)))
    ))

(define replace-old-var
  (lambda (e lst)
    (cond ((pair? (get-var-place e lst)) `(bvar ,e ,@(get-var-place e lst)))
          (else `(pvar ,e ,(get-var-place e lst))))
    ))

(define update-previus-args
  (lambda (args previus-args)
    (let* ((lst1 (map (lambda (arg) (list arg (index-of-arg arg args))) (filter (lambda (arg)(not (contain-in-previus-args? arg previus-args)))args)))
           (lst2 (fold-right (lambda (argv acc)
                               (let* ((arg (car argv))
                                      (arg-place (get-var-place arg previus-args)))                    
                                 (cond ((member arg args) (cons (list arg (index-of-arg arg args)) acc))         
                                       ((pair? arg-place) (cons (list arg (cons (+ 1 (car arg-place)) (cdr arg-place))) acc))
                                       (else (cons (list arg (list 0  arg-place)) acc)))
                                 
                                 ))'() previus-args)))
      (append lst1 lst2))
    ))



(define pe->lex-pe
  (lambda (exps) 
    (letrec ((replace-var (lambda (e previus-args)
                            (cond ((var-pattern? e) (if (contain-in-previus-args? (var->exp e) previus-args) 
                                                        (replace-old-var (var->exp e) previus-args)
                                                               `(fvar ,(var->exp e))))


                                  ((lambda-simple-pattern? e)  `(lambda-simple ,(lambda-simple->args e) 
                                                                               ,@(replace-var (lambda-simple->exps e) (update-previus-args (lambda-simple->args e) previus-args))))
                                  ((lambda-var-pattern? e)  `(lambda-var ,(lambda-var->args e) 
                                                                         ,@(replace-var (lambda-var->exps e) (update-previus-args (list(lambda-var->args e)) previus-args))))
                                  ((lambda-opt-pattern? e)  `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) 
                                                                         ,@(replace-var (lambda-opt->exps e) (update-previus-args (lambda-opt->args-and-rest e) previus-args))))
                                  ((null? e) '())
                                  ((list? e) (cons (replace-var (car e) previus-args) (replace-var (cdr e) previus-args)))                    
                                  (else e)
                                  ))))
      (replace-var exps '()))
    ))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; box-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-member?
  (lambda (e lst)
    (if (null? lst) #f
        (or (eq? e (car lst)) (is-member? e (cdr lst))))
    ))

(define var->box-get-var
  (lambda (e)
    ((pattern-rule `(var ,(? 'var)) (lambda (var) `(box-get (var ,var)))) e fail!)
    ))

(define set-var?
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var)) ,(? 'val)) (lambda (var val) #t)) e (lambda () #f))
    ))

(define set-var->exp
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) var)) e fail!)
    ))

(define set-var->val
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) val)) e fail!)
    ))

(define set-pvar?
  (lambda (e)
    ( (pattern-rule `(set (pvar ,(? 'var) ,(? 'minor)) ,(? 'val)) (lambda (var minor val) #t)) e (lambda() #f))
    ))

(define set-pvar->var
  (lambda (e)
    ((pattern-rule `(set (pvar ,(? 'var)  ,(? 'minor))  ,(? 'val)) (lambda (var minor val) var)) e fail!)
    ))

(define set-pvar->val
  (lambda (e)
    ((pattern-rule `(set (pvar ,(? 'var) ,(? 'minor)) ,(? 'val)) (lambda (var minor val) val)) e fail!)
    ))

(define set-pvar->minor
  (lambda (e)
    ((pattern-rule `(set (pvar ,(? 'var) ,(? 'minor)) ,(? 'val)) (lambda (var minor val) minor)) e fail!)
    ))

(define set-bvar?
  (lambda (e)
    ( (pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor)) ,(? 'val)) (lambda (var major minor val) #t)) e (lambda() #f))
    ))

(define set-bvar->exp
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) var)) e fail!)
    ))

(define set-bvar->val
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) val)) e fail!)
    ))

(define set-fvar?
  (lambda (e)
    ( (pattern-rule `(set (fvar ,(? 'var)) ,(? 'val)) (lambda (var val) #t)) e (lambda() #f))
    ))

(define set-fvar->exp
  (lambda (e)
    ((pattern-rule `(set (fvar ,(? 'var))  ,(? 'val)) (lambda (var val) var)) e fail!)
    ))

(define set-fvar->val
  (lambda (e)
    ((pattern-rule `(set (fvar ,(? 'var))  ,(? 'val)) (lambda (var val) val)) e fail!)
    ))

(define bvar-pattern?
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) #t)) e (lambda() #f))
    ))

(define bvar->exp
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) var)) e (lambda() #f))
    ))

(define pvar-pattern?
  (lambda (e)
    ((pattern-rule `(pvar ,(? 'var)  ,(? 'minor)) (lambda (var  minor) #t)) e (lambda() #f))
    ))

(define pvar->exp
  (lambda (e)
    ((pattern-rule `(pvar ,(? 'var)  ,(? 'minor)) (lambda (var  minor) var)) e (lambda() #f))
    ))


(define set->box-set
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) `(box-set (var ,var) ,val))) e (lambda() #f))
    ))

(define filter-seq
  (lambda (e)
    ((pattern-rule `((seq . ,(? 'exp) )) (lambda (exp) (car exp))) e (lambda() e))
    ))

(define filter-list
  (lambda (lst lst-to-filter)
    (filter (lambda (e)(not (is-member? e lst))) lst-to-filter)
    ))

(define set?
  (lambda (el exp)
    (cond ((and (set-var? exp)(eq? (set-var->exp exp) el)) #t)
          ((lambda-simple-pattern? exp)(if (is-member? el (lambda-simple->args exp)) #f (set? el (lambda-simple->exps exp))))
          ((lambda-var-pattern? exp)(if (is-member? el (list (lambda-var->args exp))) #f (set? el (lambda-var->exps exp))))
          ((lambda-opt-pattern? exp)(if (is-member? el (lambda-opt->args-and-rest exp)) #f (set? el (lambda-opt->exps exp))))
          ((null? exp) #f)
          ((list? exp) (or (set? el (car exp)) (set? el (cdr exp))))                    
          (else #f))
    ))


(define bound?
  (lambda (el exp)
    (cond ((and (bvar-pattern? exp)(eq? (bvar->exp exp) el) #t))
          ((lambda-simple-pattern? exp)(if (is-member? el (lambda-simple->args exp)) #f (bound? el (lambda-simple->exps exp))))
          ((lambda-var-pattern? exp)(if (is-member? el (list (lambda-var->args exp))) #f (bound? el (lambda-var->exps exp))))
          ((lambda-opt-pattern? exp)(if (is-member? el (lambda-opt->args-and-rest exp)) #f (bound? el (lambda-opt->exps exp))))
          ((null? exp) #f)
          ((list? exp) (or (bound? el (car exp)) (bound? el (cdr exp))))                    
          (else #f))
    ))

(define get?
  (lambda (el exp)
    (cond  ((set-bvar? exp)(get? el (set-bvar->val exp)))
           ((set-pvar? exp)(get? el (set-pvar->val exp)))
           ((set-fvar? exp)(get? el (set-fvar->val exp)))
           ((and (bvar-pattern? exp)(eq? (bvar->exp exp) el) #t))
           ((and (pvar-pattern? exp)(eq? (pvar->exp exp) el) #t))
           ((lambda-simple-pattern? exp)(if (is-member? el (lambda-simple->args exp)) #f (get? el (lambda-simple->exps exp))))
           ((lambda-var-pattern? exp)(if (is-member? el (list (lambda-var->args exp))) #f (get? el (lambda-var->exps exp))))
           ((lambda-opt-pattern? exp)(if (is-member? el (lambda-opt->args-and-rest exp)) #f (get? el (lambda-opt->exps exp))))
           ((null? exp) #f)
           ((list? exp) (or (get? el (car exp)) (get? el (cdr exp))))                    
           (else #f))
    ))

(define set-arg?
  (lambda (el exps)
    (let ((lex-pe-exp (pe->lex-pe exps)))
      (cond ((lambda-simple-pattern? exps)
             (and (set? el (lambda-simple->exps  exps)) (bound? el (lambda-simple->exps lex-pe-exp)) (get? el (lambda-simple->exps lex-pe-exp))))
            ((lambda-var-pattern? exps)
             (and (set? el (lambda-var->exps  exps)) (bound? el (lambda-var->exps lex-pe-exp)) (get? el (lambda-var->exps lex-pe-exp))))
            ((lambda-opt-pattern? exps)
             (and (set? el (lambda-opt->exps  exps)) (bound? el (lambda-opt->exps lex-pe-exp)) (get? el (lambda-opt->exps lex-pe-exp))))
            ))))

(define set-args
  (lambda (args exps)
    (let ((lex-pe-exp (pe->lex-pe exps)))
      (cond ((lambda-simple-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-simple->exps  exps)) (bound? el (lambda-simple->exps lex-pe-exp)) (get? el (lambda-simple->exps lex-pe-exp)))) args))
            ((lambda-var-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-var->exps  exps)) (bound? el (lambda-var->exps lex-pe-exp)) (get? el (lambda-var->exps lex-pe-exp)))) args))
            ((lambda-opt-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-opt->exps  exps)) (bound? el (lambda-opt->exps lex-pe-exp)) (get? el (lambda-opt->exps lex-pe-exp)))) args))
            ))))


(define update-set-list
  (lambda (args old-set-list exp)
    (let((new-set-list (filter-list args old-set-list)))
      (append new-set-list (filter (lambda (e)(set-arg? e exp)) args)))
    ))

(define make-seq-list
  (lambda (lst)
    (fold-right (lambda (e acc) (append  `((set (var ,e) (box (var ,e)))) acc)) '() lst)
    ))

(define box-set
  (lambda (exps) 
    (letrec ((replace-var (lambda (e set-list)
                            (cond ((and (set-var? e) (is-member? (set-var->exp e) set-list))
                                   ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) `(box-set (var ,var) ,(replace-var val set-list)))) e (lambda() e)))
                                  ((var-pattern? e) (if (is-member? (var->exp e) set-list) (var->box-get-var e) e))
                                  ((lambda-simple-pattern? e) (if (null? (set-args (lambda-simple->args e) e)) `(lambda-simple ,(lambda-simple->args e) 
                                                                                                                               ,@(replace-var (lambda-simple->exps e) (update-set-list (lambda-simple->args e) set-list e)))
                                                                  `(lambda-simple ,(lambda-simple->args e) (seq (,@(make-seq-list (set-args (lambda-simple->args e) e))
                                                                                                                 ,@(replace-var (filter-seq (lambda-simple->exps e)) (update-set-list (lambda-simple->args e) set-list e)))))))
                                  ((lambda-var-pattern? e) (if (null? (set-args (list (lambda-var->args e)) e)) `(lambda-var ,(lambda-var->args e) 
                                                                                                                             ,@(replace-var (lambda-var->exps e) (update-set-list (list (lambda-var->args e)) set-list e)))
                                                               `(lambda-var ,(lambda-var->args e) (seq (,@(make-seq-list (set-args (list (lambda-var->args e)) e))
                                                                                                        ,@(replace-var (filter-seq(lambda-var->exps e)) (update-set-list (list (lambda-var->args e)) set-list e)))))))
                                  ((lambda-opt-pattern? e) (if (null? (set-args (lambda-opt->args-and-rest e) e)) `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) 
                                                                                                                               ,@(replace-var (lambda-opt->exps e) (update-set-list (lambda-opt->args-and-rest e) set-list e)))
                                                               `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) (seq (,@(make-seq-list (set-args (lambda-opt->args-and-rest e) e))
                                                                                                                              ,@(replace-var (filter-seq(lambda-opt->exps e)) (update-set-list (lambda-opt->args-and-rest e) set-list e)))))))
                                  ((null? e) '())
                                  ((list? e) (cons (replace-var (car e) set-list) (replace-var (cdr e) set-list)))                    
                                  (else e)
                                  ))))
      (replace-var exps '()))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; annotate tail calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leaf-words '(const var bvar fvar pvar))
(define set-words '(set box-set box-get box))

(define minus1
  (lambda (n)
    (- n 1)))

(define tc-map
  (lambda (exp bools)
    (map (lambda (sub-exp t?) (if (null? sub-exp)
                                  sub-exp
                                  (tc-internal sub-exp t?))) exp bools)))

(define tc-internal
  (lambda (exp tc?)
    (let ((tag (car exp))
          (body (cdr exp)))
      (cond [(member tag leaf-words) exp]
            [(eq? tag 'applic) (let ((bools (make-list (length body) #f)))
                                 (if tc?
                                     `(tc-applic ,@(tc-map body bools))
                                     `(applic ,@(tc-map body bools))))]
            [(eq? tag  'or) (let ((bools (append (make-list (minus1 (length (car body))) #f) `(,tc?))))
                              `(,tag ,(tc-map (car body) bools)))]
            
            [(eq? tag 'if3) (let ((bools `(#f ,tc? ,tc?)))
                              (cons tag (tc-map body bools)))]
            [(eq? tag 'def) (let ((bools '(#f #f)))
                              (cons tag (tc-map body bools)))]
            [(eq? tag 'seq) (let ((bools (append (make-list (minus1 (length (car body))) #f) `(,tc?))))
                              `(,tag ,(tc-map (car body) bools)))]
            
            [(member tag set-words) (let ((bools (make-list (length body) #f)))
                                      (cons tag (tc-map body bools)))]
            [(member tag lambda-words) (if (eq? tag 'lambda-opt)
                                           `(,tag ,(car body) ,(cadr body) ,(tc-internal (caddr body) #t))
                                           `(,tag ,(car body) ,(tc-internal (cadr body) #t)))]
            [(list? tag) (if (null? body)
                             `(,(tc-internal tag tc?))
                             (cons (tc-internal tag tc?) (tc-internal body tc?)))]
            [else `(ERROR in tc-internal. case not covered: ,tag ,exp)] ; reached if we missed something
            ))))

(define annotate-tc
  (lambda (exp)
    (tc-internal exp #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define constants 'klum)
(set! constants '())
(define symbols 'klum)
(set! symbols '())
(define fvars 'klum)
(set! fvars '())
(define label-ctr 'klum)
(set! label-ctr 0)
(define const-ctr 'klum)
(set! const-ctr 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lambda-simple-pattern?
;lambda-simple->args
;lambda-simple->exps
;lambda-var-pattern?
;lambda-var->args
;lambda-var->exps
;lambda-opt-pattern?
;lambda-opt->args
;lambda-opt->rest
;lambda-opt->args-and-rest
;lambda-opt->exps
;member?(element lst)
;set-var?
;set-var->exp
;set-var->val
;set-pvar?
;set-pvar->exp
;set-pvar->val
;set-bvar?
;set-bvar->exp
;set-bvar->val
;set-fvar?
;set-fvar->exp
;set-fvar->val
;set-fvar->e
;set-fvar->fvar
;bvar-pattern?
;bvar->exp
;pvar-pattern?
;pvar->exp
;fvar-pattern?
;fvar->var
;box-pattern?
;box->val
;filter-list(lst lst-to-filter)


(define remove-dups
  (lambda (lst)
    (if (null? lst)
        lst
        (cons (car lst)
              (remove-dups (filter (lambda (e) (not (equal? e (car lst)))) (cdr lst)))))))
     
(define add1
  (lambda (n)
    (+ 1 n)))

(define sexpr?
  (lambda (x)
    (and (list? x) (pair? x))))

(define topo
  (lambda (x)
    (let ((bottom 8))
      (cond [(null? x) 0]
            [(void? x) 1]
            [(eq? #f x) 2]
            [(eq? #t x) 3]
            [(number? x) 4]
            [(char? x) 5]
            [(string? x) 6]
            [(symbol? x) 7]
            [(vector? x) (+ 1 (foldl max bottom (map topo (vector->list x))))]
            [(list? x) (+ 1 (foldl max bottom (map topo x)))]
            [else bottom]))))
  
(define topo-pred?
  (lambda (x y)
    (< (topo x) (topo y))))

(define length-pred? ; longest last
  (lambda (x y)
    (if (list? x)
        (if (list? y)
            (< (length x) (length y))
            #t)
        (list? y))
    ))


;(define length-sort ; racket order of arguments  ; TODO comment out these functions and uncomment the next 3 when moving to chez
;  (lambda (table)
;    (sort table length-pred?)))
;
;(define topo-sort ; racket order of arguments
;  (lambda (table)
;    (sort table topo-pred?)))

(define length-sort ; chez order of arguments
  (lambda (table)
    (sort length-pred? table)))
(define topo-sort ; chez order of arguments
  (lambda (table)
    (sort topo-pred? table)))

(define stringify-2
  (lambda (e)
    (cond [(string? e) e]
          [(symbol? e) (symbol->string e)]
          [(number? e) (number->string e)]
          [(void? e) ""]
          [(null? e) ""]
          [(list? e) (string-append (stringify-2 (car e)) " " (stringify-2 (cdr e)))]
		  [else ""]
          )))

;only uncomment in chez:
(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; files I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file->string_mayer
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (list->string
         (run))))))

(define string->file
  (lambda (filename code)
    (let ((out-port (open-output-file filename)))
      (begin (fold-left (lambda (c acc)
                          (if (null? c)
                              acc
                              (begin (write-char c out-port)
                                     acc)))
                    '()
                  (string->list code))
             (close-output-port out-port)))))

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
         (run)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; label maker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^label
  (lambda args
    (begin (set! label-ctr (add1 label-ctr))
           (string-append "L_"
                          (foldl (lambda (acc sym)
                                   (string-append acc (symbol->string sym) "_"))
                                 "" args)
                          (number->string label-ctr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; core functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; derived functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define derived-funcs '(>bin > + * - / < = remainder gcd simplify-frac
                        integer? number? append list map make-string make-vector))

;definitions in our_lib/derived_funcs.scm  ;;;; TODO include this file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define core-funcs '(;;;;;;;;;;;;;;;pairs:
                     (car FUNC_CAR)
                     (cdr FUNC_CDR)
                     (cons FUNC_CONS)
                     (set-car! FUNC_SET_CAR)
                     (set-cdr! FUNC_SET_CDR)
                     
                     ;;;;;;;;;;;;;;;math:
                     (+bin FUNC_BIN_ADD)
                     (*bin FUNC_BIN_MULL)
                     (-bin FUNC_BIN_SUB)
                     (/bin FUNC_BIN_DIV)
                     (=bin FUNC_BIN_EQUAL)
                     (<bin FUNC_BIN_LESS_THAN)
                     (denominator FUNC_DENOMINATOR)
                     (numerator FUNC_NUMERATOR)
                     
                     ;;;;;;;;;;;;;;;;preds:
                     (rational? FUNC_FRACTION_Q)
                     (procedure? FUNC_CLOSURE_Q);this is simply closure?
                     (symbol? FUNC_SYMBOL_Q)                     
                     (vector? FUNC_VECTOR_Q)
                     (char? FUNC_CHAR_Q)
                     (null? FUNC_NIL_Q)
                     (pair? FUNC_PAIR_Q)
                     (string? FUNC_STRING_Q)
					 (boolean? FUNC_BOOL_Q)
                     
                     ;;;;;;;;;;;;;;;strings:
                     (symbol->string FUNC_SYMBOL_2_STRING)
                     (string->symbol FUNC_STRING_TO_SYMBOL)
                     (make-string-c FUNC_MAKE_STRING_C) ;must take char parameter (the optional one is derived from this)
                     (string-length FUNC_STRLEN)
                     (string-ref FUNC_STRING_REF)
                     (string-set! FUNC_STRING_SET)

                     ;;;;;;;;;;;;;;;vectors:
                     (vector FUNC_VECTOR)
                     (vector-length FUNC_VECTOR_LENGTH)
                     (vector-ref FUNC_VECTOR_REF)
                     (vector-set! FUNC_VECTOR_SET)

                     ;;;;;;;;;;;;;;;others:
                     (apply FUNC_APPLY)
                     (eq? FUNC_EQ_Q)
                     (char->integer FUNC_CHAR_2_INT)
                     (integer->char FUNC_INT_2_CHAR)
                     (print_result_r0 PRV_FUNC_PRINT_RESULT)
                     ))           
                     

(define core-func-names (map car core-funcs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constant table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define basic-consts `(#t #f ,'() ,(void)))

(define add-consts-to-table
  (lambda (exp)
    (let ((add-pair (lambda (p)
                      (cond [(null? p) p]
                            [(pair? p) (begin (add-consts-to-table `(const ,(car p)))
                                              (add-consts-to-table `(const ,(cdr p))))]))))
    (cond [(not (sexpr? exp)) exp]
          [(eq? (car exp) 'const)
           (begin (cond [(vector? (cadr exp))  (map (lambda (e) (add-consts-to-table `(const ,e)))
                                                    (vector->list (cadr exp)))]
                        [(pair? (cadr exp)) (add-pair (cadr exp))]
                        [(symbol? (cadr exp)) (add-consts-to-table `(const ,(symbol->string (cadr exp))))])
                  (set! constants (cons (cadr exp) constants)))]
          [(eq? (car exp) 'fvar) (add-consts-to-table `(const ,(cadr exp)))]
          [else (map add-consts-to-table exp)]))))


(define resolve-entries
  (lambda (table)
    (let* ((n 1)
          (new-entry (lambda (size val . rep)
                       (let ((ent `(,n ,val ,rep)))
                         (begin (set! n (+ n size))
                                (set! const-ctr n)
                                ent))))
          (string-entry (lambda (val)
                          (let ((len (string-length val)))
                            (new-entry (+ 2 1 len) val `(T_STRING ,len ,@(append (map char->integer (string->list val)) '(0))))))))
      (letrec ((iterate (lambda (lst tab)
                       (if  (null? lst)
                            tab
                            (let* ((val  (car lst))
                                   (rest (cdr lst))
                                   (entry (cond [(void? val)   (new-entry 1 *void-object* 'T_VOID)]
                                                [(null? val)   (new-entry 1 '() 'T_NIL)]
                                                [(eq? #f val)  (new-entry 2 #f 'T_BOOL 0)]
                                                [(eq? #t val)  (new-entry 2 #t 'T_BOOL 1)]
                                                [(number? val) (if (integer? val)
                                                               (new-entry 3 val 'T_FRACTION val 1)
                                                               (new-entry 3 val 'T_FRACTION (numerator val) (denominator val)))]
                                                [(char? val)   (new-entry 2 val 'T_CHAR (char->integer val))]
                                                [(symbol? val) (new-entry 2 val)]
                                                [(pair? val)   (new-entry 3 val)]
                                                [(vector? val) (new-entry (+ 2 (vector-length val)) val)]
                                                [(string? val) (string-entry val)]
                                                [else `(ERROR in resolve-address: case not covered: ,val)])))
                              (iterate rest `(,@tab ,entry)))))))
        (iterate table '())))))

(define resolve-compos
  (lambda (table)
    (map (lambda (entry)
           (let ((rep  (caddr entry))
                 (body (cadr entry))
                 (addr (car entry)))
             (cond [(vector? body) `(,addr ,body (T_VECTOR ,(vector-length body)
                                                           ,@(map (lambda (c) (const-lookup c))
                                                                  (vector->list body))))]
                   [(pair? body) `(,addr ,body (T_PAIR ,(const-lookup (car body))
                                                       ,(const-lookup (cdr body))))]
                   [(symbol? body) `(,addr ,body (T_SYMBOL ,(const-lookup (symbol->string body))))]
                   [(string? body) `(,addr ,body ,@rep)]
                   [else entry])))
         table)))

(define add-function-symbols
  (lambda (lst)
    (map (lambda (func)
           (set! constants (cons (symbol->string func) (cons func constants))))
         lst)))

(define add-basic-consts
 (lambda (lst)
   (map (lambda (c)
          (set! constants (cons c constants)))
        lst)))

(define ^const-table
  (lambda (expr)
    (begin (add-basic-consts basic-consts)
           (add-consts-to-table expr)
           (add-function-symbols core-func-names)
           (set! constants (resolve-entries (topo-sort (length-sort (remove-dups constants)))))
           (set! constants (resolve-compos constants)))))

(define const-lookup
  (lambda (val)
    (let ((ent (filter (lambda (entry)
                         (equal? val (cadr entry)))
                       constants)))
      (if (empty? ent)
          `(ERROR: in const-lookup: constant not found: ,val)
          (caar ent)))))	

(define const-table->mem
  (lambda (table)
    (letrec ((table-iter (lambda (lst)
                           (if (null? lst)
                               ""
                               (let* ((entry (car lst))
                                      (rest  (cdr lst))
                                      (rep   (third entry)))
                                 (string-append (rep-iter rep) (table-iter rest))))))
             (rep-iter (lambda (lst)
                         (if (null? lst)
                             ""
                             (let* ((val  (car lst))
                                    (rest (cdr lst)))
                               (string-append (stringify-2 `("MOV(INDD(R0,R1),IMM(",val"));\n"
                                                             "INCR(R1);\n"))
                                              (rep-iter rest)))))))
          (string-append "/**start of constant table copying**/\n"
                         "PUSH(FP);\n"
                         "MOV(FP, SP);\n"
                         (stringify-2 `("MOV(R1,IMM(",const-ctr"));\n")) ; const-ctr holds the next available address after building the constant table
                         "ADD(R1,IMM(1));//for T_END\n"
                         "PUSH(R1);\n"
                         "CALL(MALLOC);\n"
                         "DEBUG(\"copying constants table to memory: size=%ld, address=%ld\\n\",R1,R0);\n"
                         "DROP(1);\n"
                         ;"MOV(M(const_off), R0);\n" ; starting address of contant table will be in M->const_off
                         "MOV(R1,IMM(0));\n"
                         (table-iter table)
                         "MOV(INDD(R0,R1),IMM(T_END));\n"
                         "/**finished constant table copying**/\n"
                         "POP(FP);\n\n\n\n\n")
                   )))
    
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fvar table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define collect-fvars
  (lambda (exp)
    (cond [(null? exp) exp]
          [(list? exp) (map (lambda (e)
                              (if (fvar-pattern? e)
                                  (if (is-member? (cadr e) core-func-names)
                                      e
                                      (set! fvars (cons (cadr e) fvars)))
                                  (collect-fvars e)))
                            exp)]
          [else exp])))

(define ^closure
  (lambda (code_lbl)
    (stringify-2 `("PUSH(IMM(3));\n"
                   "CALL(MALLOC);\n"
                   "DROP(1);\n"
                   "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                   "MOV(INDD(R0,1), EMPTY_ENV);\n"
                   "MOV(INDD(R0,2), LABEL(",code_lbl"));\n"
                   ))))

(define ^fvar-table
  (lambda (funcs vars)
    (begin (set! core-funcs (map (lambda (func)
                                   (cons (const-lookup (car func)) (cdr func)))
                                 funcs))
           (set! fvars (map (lambda (var)
                              (const-lookup var))
                            (remove-dups fvars))))))


							
(define fvar-table->mem
  (lambda (funcs vars)
    (letrec ((iter-funcs (lambda (lst)
                           (if (null? lst)
                               ""
                               (let ((f (car lst))
                                     (rest (cdr lst)))
                                 (string-append (stringify-2 `("MOV(INDD(R1,R2),",(car f)");\n"
                                                               "INCR(R2);\n"
                                                               ,(^closure (cdr f))))
                                                "MOV(INDD(R1,R2),R0);\n"
                                                "INCR(R2);\n"
                                                (iter-funcs rest))))))
             (iter-vars  (lambda (lst)
                           (if (null? lst)
                               ""
                               (let ((v (car lst))
                                     (rest (cdr lst)))
                                 (string-append (stringify-2 `("MOV(INDD(R1,R2),",v");\n"))
                                                "INCR(R2);\n"
                                                "MOV(INDD(R1,R2),IMM(0));\n"
                                                "INCR(R2);\n"
                                                (iter-vars rest)))))))
      (string-append "/**start of fvar table copying**/\n"
                     "PUSH(FP);\n"
                     "MOV(FP, SP);\n"
                     (stringify-2 `("MOV(R1,IMM(",(+ (length core-funcs) (length fvars))"));\n")) ; the only fvars are assembly functions
                     "MUL(R1,2);\n" ; each entry is a (function number, closure pointer) pair
                     "PUSH(R1);\n"
                     "CALL(MALLOC);\n"
                     "DEBUG(\"copying fvar table to memory: size=%ld, address=%ld\\n\",R1,R0);\n"
                     "DROP(1);\n"
                     "MOV(M(fvars), R0);\n" ; starting address of fvar table will be in M->fvars
                     "MOV(R1,R0);\n"
                     "MOV(R2,IMM(0));\n"
                     (iter-funcs funcs)
                     (iter-vars  vars)
                     "POP(FP);\n"
                     "/**finished fvar table copying**/\n\n\n\n\n\n\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; symbol table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^sym-table
  (lambda ()
    (string-append "\n/**initialize symbols link list**/\n"
                   "PUSH(IMM(3));//symbol entry is array of pointers |symbol|string|next|\n"
                   "CALL(MALLOC);\n"
                   "DROP(1);\n"
                   "MOV(M(symbols),R0);//M(symbols) will always hold the head of the linked list\n"
                   "MOV(INDD(R0,0),IMM(0));\n"
                   "MOV(INDD(R0,1),IMM(0));\n"
                   "MOV(INDD(R0,2),IMM(0));\n"
                   "MOV(R1,IMM(1));//start of constant table\n\n"
                   
                   "SYMBOL_TABLE_INIT_LOOP:\n"
                   "CMP(INDD(R1,0),IMM(T_SYMBOL));\n"
                   "JUMP_EQ(SYMBOL_TABLE_LOOP_ADD_NEW_SYMBOL);\n"
                   "CMP(INDD(R1,0),IMM(T_END));\n"
                   "JUMP_EQ(SYMBOL_TABLE_LOOP_EXIT);\n"
                   "INCR(R1);\n"
                   "JUMP(SYMBOL_TABLE_INIT_LOOP);\n\n"
                   
                   "SYMBOL_TABLE_LOOP_ADD_NEW_SYMBOL:\n"
                   "MOV(R2,INDD(R1,1));//the representing string\n"
                   "PUSH(IMM(3));\n"
                   "CALL(MALLOC);\n"
                   "DROP(1);\n"
                   "MOV(INDD(R0,0),R1);//put symbol\n"
                   "MOV(INDD(R0,1),R2);//put string\n"
                   "MOV(INDD(R0,2),M(symbols));//put old head in current's next\n"
                   "MOV(M(symbols),R0);//put current in head\n"
                   "INCR(R1);\n"
                   "JUMP(SYMBOL_TABLE_INIT_LOOP);\n\n"
                   
                   "SYMBOL_TABLE_LOOP_EXIT:\n"
                   "MOV(R0,IMM(SOB_VOID));\n"
                   "/**finished initializing symbols link list**/\n\n\n\n\n\n")))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-fvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define fvar-pattern?
  (lambda (exp)
    ((pattern-rule `(fvar ,(? 'var)) (lambda (var) #t)) exp (lambda() #f))))

(define fvar->var
  (lambda (exp)
    ((pattern-rule `(fvar ,(? 'var)) (lambda (var) var)) exp (lambda() fail!))))

(define cgen-fvar
  (lambda (exp ctr)
    (let ((id (const-lookup (fvar->var exp))))
      (stringify-2 `("PUSH(IMM(",id"));\n"
                     "CALL(PRV_FUNC_LOOKUP_CLOSURE);\n";this already puts the value in R0
                     "DROP(1);\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-set-fvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define set-fvar->fvar
  (lambda (exp)
    ((pattern-rule `(set (fvar ,(? 'x)) ,(? 'e)) (lambda (x e) `(fvar ,x))) exp (lambda () fail!))))

(define set-fvar->var
  (lambda (exp)
    ((pattern-rule `(set (fvar ,(? 'x)) ,(? 'e)) (lambda (x e) x)) exp (lambda () fail!))))

(define cgen-set-fvar
  (lambda (exp ctr)
  (let ((id (const-lookup (set-fvar->var exp))))
    (stringify-2 `("PUSH(IMM(",id"));\n"
                   "CALL(PRV_FUNC_LOOKUP_FVAR);\n";this already puts the pointer in R0
                   "DROP(1);\n"
                  "PUSH(R0);\n"
                  ,(cgen (set-fvar->val exp) ctr)
                  "POP(R1);\n"
                  "MOV(IND(R1),R0);\n"
                  "MOV(R0,SOB_VOID);\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-const ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define const-pattern?
  (lambda (exp)
    ((pattern-rule `(const ,(? 'c)) (lambda (c) #t)) exp (lambda () #f))))

(define const->var
  (lambda (p)
    (cadr p)))

(define cgen-const
  (lambda (exp ctr)
    (let* ((c (const->var exp))
           (addr (const-lookup c)))
      (stringify-2 `("MOV(R0,IMM(",addr"));\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-null ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cgen-null
  (lambda ()
	"" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-if3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define if3?
  (lambda (exp) ((pattern-rule `(if3 ,(? 'test) ,(? 'true) ,(? 'false))
                              (lambda (test true false) #t)) exp (lambda ()#f))))
(define if3->test
  (lambda (exp) ((pattern-rule `(if3 ,(? 'test) ,(? 'true) ,(? 'false))
                              (lambda (test true false) test)) exp (lambda ()'not-if3))))
(define if3->dit
  (lambda (exp) ((pattern-rule `(if3 ,(? 'test) ,(? 'true) ,(? 'false))
                              (lambda (test true false) true)) exp (lambda ()'not-if3))))
(define if3->dif
  (lambda (exp) ((pattern-rule `(if3 ,(? 'test) ,(? 'true) ,(? 'false))
                              (lambda (test true false) false)) exp (lambda ()'not-if3))))

(define cgen-if3
  (lambda (exp counter)
    (let[(test (if3->test exp))
         (dit (if3->dit exp))
         (dif (if3->dif exp))
         (l_if3_exit (^label 'if3_exit))
         (l_if3_else (^label 'if3_else))]
      (string-append 
       (cgen test counter) "\n"
       "CMP(R0,IMM(SOB_FALSE));\n"
       "JUMP_EQ(" l_if3_else ");\n "
       (cgen dit counter) "\n"
       "JUMP(" l_if3_exit ");\n"
       l_if3_else ":\n"
       (cgen dif counter) "\n"
       l_if3_exit ":\n"))                    
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define or?
  (lambda (exp)
    ((pattern-rule `(or ,@(? 'exp-list))(lambda (exp-list)#t))exp (lambda()#f))))

(define or->exp-list
  (lambda (exp)
    ((pattern-rule `(or ,@(? 'exp-list))(lambda (exp-list) exp-list))exp (lambda()"not or exps"))))

(define cgen-or
  (lambda (exp counter)
    (letrec [(L_or_exit (^label 'or_exit))
             (or-list (lambda (lst acc)
                        (if (null? (cdr lst)) (string-append acc (cgen (car lst) counter) 
                                                             L_or_exit ":\n")
                            (or-list (cdr lst)(string-append acc (cgen (car lst) counter) 
                                                             "\n" "CMP(R0,IMM(SOB_FALSE));\n" 
                                                             "JUMP_NE(" L_or_exit ");\n ")))))]     
      (or-list (car (or->exp-list exp)) ""))
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define seq?
  (lambda (exp)
    ((pattern-rule `(seq ,(? 'exps list?)) (lambda (exps) #t)) exp (lambda () #f))))

(define seq->exps
  (lambda (exp)
    ((pattern-rule `(seq ,(? 'exps list?))(lambda (exps) exps)) exp (lambda() "not seq"))))
 
(define cgen-seq
  (lambda (exp counter)
    (fold-right (lambda (e acc) (string-append "\n/*start of Sexpr*/\n"
                                               (cgen e counter)
                                               ;"CALL(PRV_FUNC_PRINT_RESULT);\n\n"
                                               acc))
                "" (seq->exps exp))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-applic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define applic? 
  (lambda (exp)
    ((pattern-rule `(applic ,(? 'proc) ,(? 'args)) (lambda (proc args) #t)) exp (lambda () #f))))

(define applic->proc 
  (lambda (exp)
    ((pattern-rule `(applic ,(? 'proc) ,(? 'args)) (lambda (proc args) proc)) exp (lambda () "not applic"))))

(define applic->args 
  (lambda (exp)
    ((pattern-rule `(applic ,(? 'proc) ,(? 'args)) (lambda (proc args) args)) exp (lambda () "not applic"))))

(define cgen-applic
  (lambda (exp counter)
    (string-append 
	"\n\n/**start of applic**/\n"
     "PUSH(IMM(SOB_NIL));\n"
     (fold-right (lambda (e acc) (string-append  (cgen e counter) "PUSH(R0);\n" acc) ) "" (reverse (applic->args exp)))
     "PUSH(IMM(" (number->string(+ (length  (applic->args exp) ) 1)) "));\n"; +1 because we are counting SOB_NIL
     "/*call cgen on proc of this applic*/\n"
     (cgen (applic->proc exp) counter)
     "CMP(INDD(R0,0), IMM(T_CLOSURE));\n"
     "JUMP_NE(L_error_cannot_apply_non_clos);\n" ;lable?
     "PUSH(INDD(R0,1)); //push env\n"
     "CALLA(INDD(R0,2)); //call label\n"
     "DROP(1);\n"
     "POP(R1);\n"
     "DROP(R1); //discard frame\n"
	 "/**end of applic**/\n\n\n"
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-pvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pvar->minor
  (lambda (e)
    ((pattern-rule `(pvar ,(? 'var) ,(? 'minor)) (lambda (var minor) minor)) e (lambda () "not pvar"))
    ))

(define cgen-pvar
  (lambda (exp counter)
    (stringify-2 `(
     "MOV(R1,IMM(2));\n"
     "ADD(R1,IMM(",(pvar->minor exp)"));\n"
     "MOV(R0,FPARG(R1));\n" 
     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-set-pvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cgen-set-pvar
  (lambda (exp counter)
    (stringify-2 `(
     ,(cgen (set-pvar->val exp) counter)
     "MOV(R1,IMM(2));\n"
     "ADD(R1,IMM(",(set-pvar->minor exp)"));\n"
     "MOV(FPARG(R1),R0);\n" 
     "MOV(R0,IMM(SOB_VOID));\n"
     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-box-get-pvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-get-pvar?
  (lambda (e)
    ((pattern-rule `(box-get (pvar ,(? 'var) ,(? 'minor))) (lambda (var minor) #t)) e (lambda() #f))
    ))

(define box-get-pvar->minor
  (lambda (e)
    ((pattern-rule `(box-get (pvar ,(? 'var) ,(? 'minor))) (lambda (var minor) minor)) e (lambda() "not box-get-pvar"))
    ))

(define cgen-box-get-pvar
  (lambda (exp counter)
    (stringify-2 `(
     "MOV(R1,IMM(2));\n"
     "ADD(R1,IMM(",(box-get-pvar->minor exp)"));\n";;;;;;;;;;;
     "MOV(R0,FPARG(R1));\n"
     "MOV(R0,IND(R0));\n"
    ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-bvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bvar->minor
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) minor)) e fail!)
    ))

(define bvar->major
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) major)) e fail!)
    ))

(define cgen-bvar
  (lambda (exp counter)
    (stringify-2 `(
    "MOV(R0,FPARG(0));\n"
    "MOV(R0,INDD(R0,",(bvar->major exp)"));\n"
    "MOV(R0,INDD(R0,",(bvar->minor exp)"));\n"
    ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-box-get-bvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define box-get-bvar?
  (lambda (e)
    ((pattern-rule `(box-get (bvar ,(? 'var) ,(? 'major) ,(? 'minor))) (lambda (var major minor) #t)) e (lambda () #f))
    ))


(define box-get-bvar->minor
  (lambda (e)
    ((pattern-rule `(box-get (bvar ,(? 'var) ,(? 'major) ,(? 'minor))) (lambda (var major minor) minor)) e fail!)
    ))

(define box-get-bvar->major
  (lambda (e)
    ((pattern-rule `(box-get (bvar ,(? 'var) ,(? 'major) ,(? 'minor))) (lambda (var major minor) major)) e fail!)
    ))

(define cgen-box-get-bvar
  (lambda (exp counter)
    (stringify-2 `(
     "MOV(R0,FPARG(0));\n"
     "MOV(R0,INDD(R0,",(box-get-bvar->major exp)"));\n"
     "MOV(R0,INDD(R0,",(box-get-bvar->minor exp)"));\n"
     "MOV(R0,IND(R0));\n"
     ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-set-bvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define set-bvar->minor
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) minor)) e fail!)
    ))

(define set-bvar->major
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) major)) e fail!)
    ))

(define cgen-set-bvar
  (lambda (exp counter)
    (string-append
     (cgen (set-bvar->val exp) counter)
     (stringify-2 `(
     "MOV(R1,FPARG(0));\n"
     "MOV(R2,INDD(R1,",(set-bvar->major exp)"));\n"
     "MOV(INDD(R2,",(set-bvar->minor exp)"),R0);\n"
     "MOV(R0,IMM(SOB_VOID));\n"
     )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-box-set-pvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-set-pvar?
  (lambda (e)
    ((pattern-rule `(box-set (pvar ,(? 'var) ,(? 'minor))  ,(? 'val)) (lambda (var minor val) #t)) e (lambda () #f))
    ))

(define box-set-pvar->val
  (lambda (e)
    ((pattern-rule `(box-set (pvar ,(? 'var) ,(? 'minor))  ,(? 'val)) (lambda (var minor val) val)) e fail!)
    ))

(define box-set-pvar->minor
  (lambda (e)
    ((pattern-rule `(box-set (pvar ,(? 'var) ,(? 'minor))  ,(? 'val)) (lambda (var  minor val) minor)) e fail!)
    ))

(define cgen-box-set-pvar
  (lambda (exp counter)
    (stringify-2
     `(,(cgen (box-set-pvar->val exp) counter)
     "MOV(R1,IMM(2));\n"
     "ADD(R1,IMM(",(box-set-pvar->minor exp)"));\n"
     "MOV(R2,FPARG(R1));\n"
     "MOV(IND(R2),R0);\n"
     "MOV(R0,IMM(SOB_VOID));\n"
     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-box-set-bvar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-set-bvar?
  (lambda (e)
    ((pattern-rule `(box-set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) #t)) e (lambda () #f))
    ))

(define box-set-bvar->val
  (lambda (e)
    ((pattern-rule `(box-set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) val)) e fail!)
    ))

(define box-set-bvar->minor
  (lambda (e)
    ((pattern-rule `(box-set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) minor)) e fail!)
    ))

(define box-set-bvar->major
  (lambda (e)
    ((pattern-rule `(box-set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) major)) e fail!)
    ))

(define cgen-box-set-bvar
  (lambda (exp counter)
    (string-append
     (cgen (box-set-bvar->val exp) counter)
     (stringify-2 `(
     "MOV(R1,FPARG(0));\n"
     "MOV(R2,INDD(R1,",(box-set-bvar->major exp)"));\n"
     "MOV(R1,INDD(R2," ,(box-set-bvar->minor exp)"));\n"
     "MOV(IND(R1),R0);\n"
     "MOV(R0,IMM(SOB_VOID));\n"
     )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-pattern?
  (lambda (e)
    ((pattern-rule `(box ,(? 'val)) (lambda (val) #t)) e (lambda () #f))))

(define box->val
  (lambda (e)
    ((pattern-rule `(box ,(? 'val)) (lambda (val) val)) e fail!)))

(define cgen-box
  (lambda (exp counter)
    (string-append
     (cgen (box->val exp) counter)
     (stringify-2 `(
     "/*boxing*/\n"
     "MOV(R1,R0);//r1 now points to the value\n"
     "PUSH(IMM(1));\n"
     "CALL(MALLOC);\n"
     "DROP(1);\n"
     "//r0 now points to a place on the heap\n"
     "MOV(IND(R0),R1);//put value on heap\n")))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-lambda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define extend-env-loop
  (lambda (i major acc)
    (if (< i major)
        (extend-env-loop (+ i 1) major (string-append acc "MOV(INDD(R2,"(number->string (+ i 1))"),INDD(R1,"(number->string i)"));\n" ))                      
        acc)))

(define copy-arg-loop
  (lambda ()
    (let ((Loop (^label 'copy_arg_loop))
          (Loop_exit (^label 'copy_arg_loop_exit)))
      (string-append
       "/**start copy-arg-loop**/\n"
       "#define i_arg_loop R5\n"
       "#define j_arg_loop R6\n"
       "MOV(i_arg_loop,IMM(0));\n"
       "MOV(j_arg_loop,IMM(2));\n"
       
       "CMP(IMM(0),R3);\n"
       "JUMP_EQ("Loop_exit");\n"
       Loop ":\n "
       "MOV(INDD(R4,i_arg_loop),FPARG(j_arg_loop));\n"
       "INCR(i_arg_loop);\n"
       "INCR(j_arg_loop);\n"
       "CMP(i_arg_loop,R3);\n"
       "JUMP_NE("Loop");\n"
       Loop_exit ":\n"
       "#undef i_arg_loop\n"
       "#undef j_arg_loop\n"
       "/**end copy-arg-loop**/\n"
       ))))

(define build-lambda-closure
  (lambda (counter L_clos_body L_clos_exit)
    (string-append
	"/**start build-lambda-closure**/\n"
     "MOV(R1,FPARG(0));\n"
     "PUSH(IMM("(number->string (+ 1  counter))"));\n"
     "CALL(MALLOC);\n"
     "DROP(1);\n"
     "MOV(R2,R0);\n"
     "/*call extend-env-loop with counter="(number->string counter)"*/\n"
     (extend-env-loop 0 counter "")
     "MOV(R3,FPARG(1));\n"
     "PUSH(R3);\n"
     "CALL(MALLOC);\n"
     "DROP(1);\n"
     "MOV(INDD(R2,0),R0);\n"
     "MOV(R4,INDD(R2,0));\n"
     (copy-arg-loop)
     "PUSH(IMM(3));\n"
     "CALL(MALLOC);\n"
     "DROP(1);\n"
     "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
     "MOV(INDD(R0,1),R2);\n"
     "MOV(INDD(R0,2),LABEL("L_clos_body"));\n"
     "JUMP("L_clos_exit")\n"  
     )))

(define fix-stack-helper
  (lambda (num-of-args)
    (let ((Loop (^label 'fix_stack_helper_loop))
          (Loop_exit (^label 'fix_stack_helper_loop_exit)))
      (string-append
       "/**start fix-stack-helper**/\n"
       "#define j R7 \n"
       "#define i R8 \n"
       "MOV(i,IMM("(number->string num-of-args)"));\n"
       "ADD(i,IMM(2));\n"
       "MOV(j,FPARG(1));\n"
       "INCR(j);\n"
       
       "CMP(i,IMM(-3));\n"
       "JUMP_LE("Loop_exit");\n"
       
       Loop":\n"
       "MOV(FPARG(j),FPARG(i));\n"
       "DECR(i);\n"
       "DECR(j);\n"
       "CMP(i,IMM(-3));\n"
       "JUMP_GT("Loop");\n"
       Loop_exit":\n"
       "ADD(j,IMM(3));\n"
       "DROP(j);\n"
       "#undef i\n"
       "#undef j\n"
       "/**end fix-stack-helper**/\n"
       ))))

(define fix-stack
  (lambda (num-of-args)
    (let ((Loop (^label 'fix_stack_loop))
          (Loop_exit (^label 'fix_stack_loop_exit)))
      (string-append
       "/**start fix-stack**/\n"
       "#define m R7 \n"
       "#define i R8 \n"
       "MOV(m, IMM("(number->string num-of-args)"));\n"
       "ADD(m,IMM(2));\n"
       "MOV(i,FPARG(1));\n"
       "MOV(R1,IMM(SOB_NIL));\n"
       
       "CMP(i,m);\n"
       "JUMP_LT("Loop_exit");\n"
       
       Loop":\n"
       "PUSH(R1);\n"
       "PUSH(FPARG(i));\n"
       "CALL(MAKE_SOB_PAIR);\n"
       "DROP(2);\n"
       "MOV(R1,R0);\n"
       "DECR(i);\n"
       "CMP(i,m);\n"
       "JUMP_GE("Loop");\n"
       Loop_exit":\n"
       "MOV(FPARG(m),R1);\n"
       "#undef i\n"
       "#undef m\n"
       (fix-stack-helper num-of-args)
	   "MOV(R7,SP);\n"
	   "SUB(R7,IMM(2));\n";check for lambda opt
	   "MOV(FP,SP);\n"
       "MOV(FPARG(1),IMM("(number->string (+ num-of-args 1))"));\n"
       "/**end fix-stack**/\n"
       ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-lambda-simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cgen-lambda-simple
  (lambda (exp counter)
    (let ((L_clos_body (^label 'lambda_simple_clos_body))
          (L_clos_exit (^label 'lambda_simple_clos_exit))
          )
      (string-append
       "/*lambda-simple-start*/\n"
       (build-lambda-closure counter L_clos_body L_clos_exit)
       
       L_clos_body ":\n"
       "PUSH(FP);\n"
       "MOV(FP,SP);\n"
       "CMP(FPARG(1),IMM("(number->string (+(length (lambda-simple->args exp)) 1))"));\n"
       "JUMP_NE(L_error_lambda_args_count);\n" ;lable?
       "/*call cgen on the body of this lambda*/\n"
       (cgen (lambda-simple->exps exp) counter)      
       "POP(FP);\n"
       "RETURN;\n"     
       L_clos_exit ":\n"
	   "/*lambda-simple-end*/\n"
       ))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-lambda-opt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cgen-lambda-opt
  (lambda (exp counter)
    (let ((L_clos_body (^label 'lambda_opt_clos_body))
          (L_clos_exit (^label 'lambda_opt_clos_exit))
          (num-of-args (length (lambda-opt->args exp))))
      (string-append
	  "/*lambda-opt-start*/\n"
       (build-lambda-closure counter L_clos_body L_clos_exit)
       
       L_clos_body ":\n"
       "PUSH(FP);\n"
       "MOV(FP,SP);\n"
       "MOV(R1,SOB_NIL);\n"
       "/*call fix-stack for "(number->string num-of-args)" arguments*/\n"
       (fix-stack num-of-args)
       "/*call cgen on the body of this lambda-opt*/\n"
       (cgen (lambda-opt->exps exp) counter)
       "POP(FP);\n"
       "RETURN;\n"     
       L_clos_exit ":\n"
	   "/*lambda-opt-end*/\n"
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-lambda-var ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cgen-lambda-var
  (lambda (exp counter)
    (let ((L_clos_body (^label 'lambda_var_clos_body))
          (L_clos_exit (^label 'lambda_var_clos_exit)))
      (string-append
	  "/*lambda-var-start*/\n"
       (build-lambda-closure counter L_clos_body L_clos_exit)
       
       L_clos_body ":\n"
       "PUSH(FP);\n"
       "MOV(FP,SP);\n"
       "MOV(R1,SOB_NIL);\n"
       (fix-stack 0)
       (cgen (lambda-var->exps exp) counter)
       "POP(FP);\n"
       "RETURN;\n"     
       L_clos_exit ":\n"
	   "/*lambda-var-end*/\n"
       ))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-tc-applic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tc-applic? 
  (lambda (exp)
    ((pattern-rule `(tc-applic ,(? 'proc) ,(? 'args))(lambda (proc args)#t))exp (lambda()#f))))

(define tc-applic->proc 
  (lambda (exp)
    ((pattern-rule `(tc-applic ,(? 'proc) ,(? 'args))(lambda (proc args)proc))exp (lambda()"not tc-applic"))))

(define tc-applic->args 
  (lambda (exp)
    ((pattern-rule `(tc-applic ,(? 'proc) ,(? 'args))(lambda (proc args)args))exp (lambda()"not tc-applic"))))

(define tc-fix-stack
  (lambda ()
    (let ((Loop (^label 'tc_fix_stack_loop))
          (Loop_exit (^label 'tc_fix_stack_loop_exit)))
      (string-append
       "/**start tc-fix-stack**/\n"
       "MOV(R8,FPARG(1)); // new argc\n"
       "MOV(R9,R8);\n"
       "ADD(R9,IMM(5)); //+5 to reach old argc\n"
       "MOV(R7,FPARG(R9)); //old argc\n"
       "INCR(R8); //need to grab 1 more\n"
       "ADD(R7,R9);\n"
       "ADD(R7,IMM(3)); //distance from FP to last of old args\n"
       "MOV(R15,FP);\n"
       "SUB(R15,R7);\n"
       "MOV(SP,R15); //now we fill the stack with the new frame from the bottom of the old frame\n"
       
       Loop":\n"
       "PUSH(FPARG(R8));\n"
       "DECR(R8);\n"
       "CMP(R8,IMM(-2));\n"
       "JUMP_GT("Loop");\n"
       Loop_exit":\n"
       "/**end tc-fix-stack**/\n"
       ))))

(define cgen-tc-applic
  (lambda (exp counter)
    (string-append 
	"/**start of tc-applic**/\n"
     "PUSH(IMM(SOB_NIL));\n"
     (fold-right (lambda (e acc) (string-append  (cgen e counter) "PUSH(R0);\n" acc) ) "" (reverse (tc-applic->args exp)))
     "PUSH(IMM(" (number->string(+ (length  (tc-applic->args exp) ) 1)) "));\n"
     "/*call cgen on proc from cgen-tc-applic*/\n"
     (cgen (tc-applic->proc exp) counter)
     "CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
     "JUMP_NE(L_error_cannot_apply_non_clos);\n" ;lable?
     "PUSH(INDD(R0,1)); //push new env\n";push the new environment
     "PUSH(FPARG(-1)); //push the old ret\n";push the old ret
     "MOV(R1,FPARG(-2)); //save old FP\n";save the old fp
     "MOV(FP,SP);\n"
     "INCR(FP); //so FPARG macro is correct\n" ; this is so we can use FPARG macro
     (tc-fix-stack)
     "MOV(FP,R1); //restore old FP\n"
     "MOV(R1,INDD(R0,2)); //get closure label\n"
     "JUMPA(R1);\n"
	 "/**end of tc-applic**/\n"
     )))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cgen-def ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define def? 
  (lambda (exp)
    ((pattern-rule `(def (fvar ,(? 'var)) ,(? 'val))(lambda (var val) #t))exp (lambda()#f))))

(define def->var 
  (lambda (exp)
    ((pattern-rule `(def (fvar ,(? 'var)) ,(? 'val))(lambda (var val) var))exp (lambda()"not def"))))

(define def->val 
  (lambda (exp)
    ((pattern-rule `(def ,(? 'var) ,(? 'val))(lambda (var val) val))exp (lambda()"not def"))))

(define cgen-def
  (lambda (exp counter)
    (let ((id (const-lookup (def->var exp))))
    (stringify-2 `("PUSH(IMM(",id"));\n"
                   "CALL(PRV_FUNC_LOOKUP_FVAR);\n";this already puts the pointer in R0
                   "DROP(1);\n"
                  "PUSH(R0);\n"
                  ,(cgen (def->val exp) counter)
                  "POP(R1);\n"
                  "MOV(IND(R1),R0);\n"
                  "MOV(R0,SOB_VOID);\n")))))     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code-gen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-all (lambda (e)(annotate-tc (pe->lex-pe(box-set (remove-applic-lambda-nil(eliminate-nested-defines(parse e))))))))

(define cgen
  (lambda (code counter)
    (cond [(null? code) (cgen-null)]
	      [(def? code) (cgen-def code counter)]
          [(if3? code) (cgen-if3 code counter)]
          [(or? code) (cgen-or code counter)]
          [(seq? code) (cgen-seq code counter)]
          [(applic? code) (cgen-applic code counter)]
          [(tc-applic? code) (cgen-tc-applic code counter)]
          [(box-set-bvar? code) (cgen-box-set-bvar code counter)]
          [(box-get-bvar? code) (cgen-box-get-bvar code counter)]
          [(set-bvar? code) (cgen-set-bvar code counter)]
          [(bvar-pattern? code) (cgen-bvar code counter)]
          [(box-set-pvar? code) (cgen-box-set-pvar code counter)]
          [(box-get-pvar? code) (cgen-box-get-pvar code counter)]
          [(box-pattern? code) (cgen-box code counter)]
          [(set-pvar? code) (cgen-set-pvar code counter)]
          [(pvar-pattern? code)(cgen-pvar code counter)]
          [(fvar-pattern? code) (cgen-fvar code counter)]
          [(set-fvar? code) (cgen-set-fvar code counter)]
          [(lambda-simple-pattern? code) (cgen-lambda-simple code (+ 1 counter))]
          [(lambda-opt-pattern? code) (cgen-lambda-opt code (+ 1 counter))]
          [(lambda-var-pattern? code) (cgen-lambda-var code (+ 1 counter))]
          [(const-pattern? code) (cgen-const code counter)]
          [(list? code) (begin ;(display code)
                               (stringify-2 (map (lambda (sub-codes)
                                             (cgen sub-codes counter))
                                           code)))]
          [else (begin ;(display code)
                       (stringify-2 `("\n\nError: no cgen for:\n"
                                      ,code
                                      "\n\n")))]
          )))

(define generate-code
  (lambda (code)
    (stringify-2 `(,(const-table->mem constants)
                   ,(fvar-table->mem core-funcs fvars)
                   ,(^sym-table)
                   ,(cgen code 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code-creator  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-code-string 
  (lambda (src)
    (letrec ((iter-sexpr (lambda (acc remaining)
                           (if (null? remaining)
                               acc
                               (<sexpr> remaining 
                                        (lambda (match rem)
                                          (iter-sexpr `(,@acc (print_result_r0 ,match)) rem))
                                        (lambda (e) `(reading failed on ,e)))))))

      (iter-sexpr '() `(,@(file->list "arch/our_lib/derived_funcs.scm")
                         #\newline
                         ,@(file->list src)))
      )))
                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p 'klum); for debugging

(define compile-scheme-file
  (lambda (src trgt)
    (let* ((code (make-code-string src))
           (parsed (cdr (parse-all code)))
           (prol (file->string_mayer "prologue.c"))
           (epil (file->string_mayer "epilogue.c")))
      (begin (set! p parsed); for debugging only
             (^const-table parsed)
             (collect-fvars parsed)
             (^fvar-table core-funcs fvars)
             (string->file trgt (string-append prol
                                               "\n\n\n/**************** START OF CODE ****************/\n\n"
                                               (generate-code parsed)
                                               "\n\n\n/***************** END OF CODE *****************/\n\n"
                                               epil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(define const-parse (lambda (e) (^const-table (parse-all e))))
