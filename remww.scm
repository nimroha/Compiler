(load "pattern-matcher.scm")

(define var?
  (lambda (x)
    (and (symbol? x) (not (member x *reserved-words*)))))
	
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))
		
(define is-member?
  (lambda (e lst)
    (if (null? lst) #f
        (or (eq? e (car lst)) (member? e (cdr lst))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remww ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inst->input-regs 
  (lambda (e)
    ((pattern-rule `(,(? 'name var?) ,(? 'in-regs list?)  ,(? 'out-regs list?)) (lambda (name in-regs  out-regs) in-regs)) e (lambda() e))
    ))

(define inst->output-regs 
  (lambda (e)
    ((pattern-rule `(,(? 'name var?) ,(? 'in-regs list?)  ,(? 'out-regs list?)) (lambda (name in-regs  out-regs) out-regs)) e (lambda() e))
    ))

(define redundant?
  (lambda (instruction inst-list)
    (letrec ((redundant-reg? 
              (lambda (reg)
                (cond [(null? inst-list) #f]
                      [(is-member? reg (inst->input-regs (car inst-list))) #f]
                      [(is-member? reg (inst->output-regs (car inst-list))) #t]
                      [else (redundant? (list reg) (cdr inst-list))]
                      ))))
      (andmap redundant-reg?  (inst->output-regs instruction))
      )))

(define remww 
  (lambda (exps)
    (letrec ((filter-redundant 
              (lambda (inst-list acc)
                (cond [(null? inst-list) acc]
                      [(redundant? (car inst-list) (cdr inst-list))(filter-redundant (cdr inst-list) acc)]
                      [else (filter-redundant (cdr inst-list) (append acc (list (car inst-list)) ))]
                      ))))
      (let ((filtered (filter-redundant exps '())))
        (if (equal? filtered exps) filtered (remww filtered)))
      )))