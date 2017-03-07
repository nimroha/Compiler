
;(define +bin +)
;(define -bin -)
;(define *bin *)
;(define /bin /)
;(define <bin <)
;(define =bin =)
;(define make-string-c (lambda (n c) (make-string n c)))

(define binary->variadic
  (lambda (proc unit)
    (letrec ((proc-var (lambda (args acc)
                         (cond ((null? args)  (proc unit acc))
                               ((null? (cdr args)) (proc acc (car args)))
                               (else (proc-var (cdr args) (proc acc (car args))))))))
      (lambda args
        (if (null? args)
            unit
            (proc-var (cdr args) (car args)))))))

(define cmpr->variadic
  (lambda (cmp)
    (letrec ((cmp-var (lambda (a lst)
                         (cond ((null? lst) #t)
                               ((null? (cdr lst))  (cmp a (car lst) ))
                               (else (and (cmp a (car lst)) (cmp-var (car lst) (cdr lst))))))))
      (lambda (a . args)
        (cmp-var a args)))))


(define number?
  (lambda (x)
    (rational? x)))

;(define map ; one-list map
;  (lambda (proc lst)
;    (if (null? lst)
;        lst
;        (cons (proc (car lst)) (map (cdr lst))))
;    ))

(define map
  (lambda (proc first-list . rest-list)
    (if (null? first-list)
        '()
        (if (null? rest-list)
            (cons (proc (car first-list)) (map proc (cdr first-list)))
            (cons (apply proc (cons (car first-list) (map car rest-list)))
                  (apply map (cons proc (cons (cdr first-list) (map cdr rest-list)))))
            ))))
 
(define list
  (lambda args args))

;(define append ; 2-list append
;  (lambda (a b)
;    (if (null? a)
;        b
;        (cons (car a) (append (cdr a) b)))))

(define append
  (lambda lists
    (if (null? lists)
        lists            
        (let ((first (car lists))
              (rest  (cdr lists)))
          (if (null? rest)
              first
              (if (null? first)
                  (apply append rest)
                  (cons (car first) (apply append (cons (cdr first) rest)))))))))

(define >bin 
  (lambda (a b)
    (not (or (=bin a b) (<bin a b)))))

(define > (cmpr->variadic >bin))
(define < (cmpr->variadic <bin))
(define = (cmpr->variadic =bin))
(define + (binary->variadic +bin 0))
(define - (binary->variadic -bin 0))
(define * (binary->variadic *bin 1))
(define / (binary->variadic /bin 1))

(define make-string
  (lambda (n . c)
    (if (null? c)
        (make-string-c n #\x0)
        (make-string-c n (car c)))))

(define integer?
  (lambda (x)
    (and (rational? x)
         (=bin 1 (denominator x)))))

(define remainder
  (lambda (x n)
    (letrec ((remainder-abs 
              (lambda (a b)
                (if (<bin a b)
                    a
                    (remainder-abs (-bin a b) b)))))             
      (cond ((<bin 0 x) (if (<bin 0 n ) (remainder-abs x n)
                            (remainder-abs x (-bin 0 n))))
            ((>bin 0 x) (if (<bin 0 n ) (-bin 0 (remainder-abs (-bin 0 x) n))
                            (-bin 0 (remainder-abs (-bin 0 x) (-bin 0 n)))))
			(else x)
            ))))

(define not
  (lambda (x)
    (if x
        #f
        #t)))

(define zero?
  (lambda (x)
    (and (rational? x)
         (=bin 0 x))))

;(define gcd ;unused
;  (lambda (a b)
;    (if (= 0 b)
;        a
;        (gcd b (remainder a b)))))
;
;(define simplify-frac ;unused
;  (lambda (p)
;    (let* ((num (car p))
;           (den (cdr p))
;           (div (gcd num den)))
;      (cons (/ num div) (/ den div)))))
;
;(define vector
;  (lambda (args) ; not needed!!!
;    (vector-n `(,(length args) ,@args))))

(define make-vector
  (lambda (n . obj)
    (letrec ((make-args (lambda (n el)
                          (if (=bin n 0)
                              '()
                              (cons el (make-args (-bin n 1) el))))))
    (if (null? obj)
        (apply vector (make-args n 0))
        (apply vector (make-args n (car obj)))))))

(define reverse
  (lambda (lst)
    (if (null? lst)
        lst
        (append (reverse (cdr lst)) (list (car lst))))))