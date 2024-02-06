#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct .. (h t) #:transparent)
(struct empty () #:transparent)
(struct ?empty (expr) #:transparent)
(struct ?int (expr) #:transparent)
(struct ?bool (expr) #:transparent)
(struct ?exception (expr) #:transparent)
(struct ?.. (expr) #:transparent)
(struct ?seq (expr) #:transparent)
(struct add (l r) #:transparent)
(struct mul (l r) #:transparent)
(struct ?leq (l r) #:transparent)
(struct ?= (l r) #:transparent)
(struct ~ (val) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)
(struct ?any (e) #:transparent)
(struct ?all (e) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (cnd e1 e2) #:transparent)
(struct exception (exn) #:transparent)
(struct vars (s e1 e2) #:transparent)
(struct valof (name) #:transparent)
(struct trigger (e) #:transparent)
(struct triggered (exc) #:transparent)
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

(define (isBool expr)
    (or (true? expr) (false? expr)))

(define (add_new_env_var name val env)
  (if (null? env)
      (list (cons name val))
      (append env (list (cons name val)))
  )
)

(define (toBool expr)
    (cond
      ((and (boolean? expr) expr) (true))
      ((and (boolean? expr) (not expr)) (false))
      (else (error "toBool: Invalid expression"))
      )
)

(define (get_env_var_value name env)
  (define (get_env_var_value_helper name lst found_it)
      (cond
        ((and (null? lst) (not found_it)) (raise (exception "valof: undefined variable")))
        ((and (null? lst) found_it) null)
        ((string=? (car (car lst)) name)
            (let ((another_value (get_env_var_value_helper name (cdr lst) #t)))
                (if (null? another_value)
                    (cdr (car lst))
                    another_value
                 )
            )
         )
        (else (get_env_var_value_helper name (cdr lst) found_it))
      )
  )
  (get_env_var_value_helper name env #f)
)

(define (eval expr)
    (cond
        ((int? expr) (int-n expr))
;        ((valof? expr) (get_env_var_value (valof-name expr) env))
        ((true? expr) #t)
        ((false? expr) #f)
        (else #f)
    )
)

(define (is_seq s)
  (cond
    ((empty? s) #f)
    ((..? s) #t)
    (else #f)
  )
)

(define (evaluate_seq expr env)
   (cond
     ((empty? expr) (empty))
     ((is_seq expr) (.. (evaluate_expr (..-h expr) env) (evaluate_seq (..-t expr) env)))
     (else (evaluate_expr expr env))
   )
)


(define (combine_seq s1 s2)
      (cond
        ((empty? s1) s2)
        ((empty? s2) s1)
        (else (.. (..-h s1) (combine_seq (..-t s1) s2)))
      )
)

(define (evaluate_seq_add l r env)
   (let ((eval_left (evaluate_seq l env))
         (eval_right (evaluate_seq r env)))

     (if (and (eval (is_real_eval_seq eval_left)) (eval (is_real_eval_seq eval_right)))
         (combine_seq eval_left eval_right)
         (raise (exception "add: wrong argument type"))
     )
   )
)
    
    
(define (evaluate_add expr env)
  (let ((left_expr (evaluate_expr (add-l expr) env))
        (right_expr (evaluate_expr (add-r expr) env)))
    (cond
;        ((valof? left_expr) (evaluate_add (add (get_env_var_value (valof-name left_expr)) right_expr)))
;        ((valof? right_expr) (evaluate_add (add left_expr (get_env_var_value (valof-name right_expr)))))
        ((and (int? left_expr) (int? right_expr)) (int (+ (eval left_expr) 
                                                       (eval right_expr)))) ; both int
        ((and (isBool left_expr) (isBool right_expr)) (toBool (or (eval left_expr) (eval right_expr)))) ; both bool
        ((and (is_seq left_expr) (is_seq right_expr)) (evaluate_seq_add left_expr right_expr env))
        ((and (is_seq left_expr) (empty? right_expr)) left_expr)
        ((and (is_seq right_expr) (empty? left_expr)) right_expr)
        ((and (empty? left_expr) (empty? right_expr)) (empty))
        ((add? expr) (raise (exception "add: wrong argument type")))
    )
  )
)

(define (evaluate_mul expr env)
  (let ((eval_left (evaluate_expr (mul-l expr) env))
        (eval_right (evaluate_expr (mul-r expr) env)))
    (cond
;       ((valof? (mul-l expr)) (evaluate_mul (mul (get_env_var_value (valof-name (mul-l expr))) (mul-r expr))))
;       ((valof? (mul-r expr)) (evaluate_mul (mul (mul-l expr) (get_env_var_value (valof-name (mul-r expr))))))
       ((and (int? eval_left) (int? eval_right)) (int (* (eval eval_left) (eval eval_right)))) ; both int
       ((and (isBool eval_left) (isBool eval_right)) (toBool (and (eval eval_left) (eval eval_right)))) ; both bool
       (else (raise (exception "mul: wrong argument type")))
;       ((mul? expr) (evaluate_complex_mul expr env))
   )
  )
)

(define (evaluate_leq_for_seq s1 s2)
       (cond
          ((empty? s1) (true))
          ((and (not (empty? s1)) (empty? s2)) (false))
          (else (evaluate_leq_for_seq (..-t s1) (..-t s2)) )
     )
)

(define (evaluate_leq_seq l r env)
  (let ((eval_left (evaluate_seq l env))
         (eval_right (evaluate_seq r env)))

     (if (and (eval (is_real_eval_seq eval_left)) (eval (is_real_eval_seq eval_right)))
         (evaluate_leq_for_seq eval_left eval_right)
         (raise (exception "?leq: wrong argument type"))
     )
   )  
)

(define (evaluate_leq expr env)
  (let ((eval_left (evaluate_expr (?leq-l expr) env))
        (eval_right (evaluate_expr (?leq-r expr) env)))
                   
    (cond
       ((and (int? eval_left) (int? eval_right)) (toBool (<= (eval eval_left) (eval eval_right)))) ; both int
       ((and (isBool eval_left) (isBool eval_right)) (toBool (or (not (eval eval_left)) (and (eval eval_left) (eval eval_right))))) ; both bool
       ((and (is_seq eval_left) (is_seq eval_right)) (evaluate_leq_seq eval_left eval_right))
       ((and (is_seq eval_left) (empty? eval_right)) (false))
       ((and (is_seq eval_left) (empty? eval_right)) (false))
       ((and (empty? eval_left) (empty? eval_right)) (true))
       (else (raise (exception "?leq: wrong argument type")))
   )
  )
)

(define (evaluate_inv expr env)
  (let ((eval_expr (evaluate_expr (~-val expr) env)))
    (cond
;       ((valof? (~-val expr)) (evaluate_inv (get_env_var_value (valof-name expr) env) env))
       ((int? eval_expr)  (int (-(eval eval_expr)))) ; inv int
       ((isBool eval_expr) (toBool (not (eval eval_expr)))) ; inv bool
       ((~? expr) (raise (exception "~: wrong argument type")))
    )
         
  )
)

(define (evaluate_branching expr env)
   (with-handlers
       ([exn:fail? (if-then-else-e1 expr)])
     
     (if (not(eval (evaluate_expr (if-then-else-cnd expr) env)))
         (evaluate_expr (if-then-else-e2 expr) env)
         (evaluate_expr (if-then-else-e1 expr) env))
   )
)

(define (evaluate_seq_equality s1 s2 env)
    (cond
       ((and (int? s1) (int? s2)) (toBool (= (eval s1) (eval s2))))
       ((and (isBool s1) (isBool s2)) (toBool (and (eval s1) (eval s2))))
       ((and (empty? s1) (empty? s2)) (true))
       ((and (is_seq s1) (is_seq s2) (eval (evaluate_seq_equality (..-h s1) (..-h s2) env))) (evaluate_seq_equality (..-t s1) (..-t s2) env))
       (else (false))
    )
)


(define (evaluate_equal expr env)
  (let ((eval_left (evaluate_expr (?=-l expr) env))
        (eval_right (evaluate_expr (?=-r expr) env)))
    (cond
       ((and (int? eval_left) (int? eval_right)) (toBool (= (eval eval_left) (eval eval_right))))
       ((and (isBool eval_left) (isBool eval_right)) (toBool (and (eval eval_left) (eval eval_right))))
       ((and (empty? eval_left) (empty? eval_right)) (true))
       ((and (is_seq eval_left) (is_seq eval_right)) (evaluate_seq_equality (evaluate_expr eval_left env) (evaluate_expr eval_right env) env))
       ((?=? expr) (false))
    )
  )
)

(define (evaluate_head expr env)
    (cond
       ((empty? (evaluate_expr expr env)) (raise (exception "head: empty sequence")))
       ((is_seq (evaluate_expr expr env)) (..-h (evaluate_expr expr env)))          
       (else (raise (exception "head: wrong argument type")))
    )
)

(define (evaluate_tail expr env)
    (cond
       ((empty? (evaluate_expr expr env)) (raise (exception "tail: empty sequence")))
       ((is_seq (evaluate_expr expr env)) (..-t (evaluate_expr expr env)))
       (else (raise (exception "tail: wrong argument type")))
    )
)

(define (evaluate_any_for_seq s env)
  (cond
    ((empty? s) (false))
    (true? (evaluate_expr (..-h s) env) (true))
    (else (evaluate_any_for_seq (..-t s) env))
   )
)

(define (evaluate_any expr env)
  (let ((eval_seq (evaluate_seq expr env)))
      (if (eval (is_real_eval_seq  eval_seq))
          (evaluate_any_for_seq eval_seq env)
          (raise (exception "?any: wrong argument type")))
  )
)

(define (evaluate_all_for_seq s env)
  (cond
     ((empty? s) (true))
     (else (toBool (and (eval (evaluate_expr (..-h s) env))
               (if (empty? (..-t s))
                   (eval (true))
                   (eval (evaluate_all_for_seq (..-t s) env))
               )
           ))
     )
   )
)

(define (evaluate_all expr env)
   (let ((eval_seq (evaluate_seq expr env)))
      (if (eval (is_real_eval_seq eval_seq))
          (evaluate_all_for_seq eval_seq env)
          (raise (exception "?all: wrong argument type"))
      )
  )
)

; checks if evaluated expression s is a sequence that also ends with (empty)
(define (is_real_eval_seq s)
  (does_seq_ends_on_empty s)
)

(define (does_seq_ends_on_empty s)
   (cond
    ((empty? s) (true))
    ((and (is_seq s)) (does_seq_ends_on_empty (..-t s)))
    (else (false))
  )
)

(define (compare_exceptions e1 e2)
   (string=? (exception-exn e1) (exception-exn e2))
)

(define (contains-duplicate? lst)
  (let ((seen '()))
    (define (helper lst)
      (cond
        ((null? lst) #f)
        ((memq (car lst) seen) #t)
        (else (set! seen (cons (car lst) seen))
              (helper (cdr lst)))))
    (helper lst)))

(define (extend_environment names values env new_env)
 ; (if(and( not (null? values)) (list? values))
 ;                (display (evaluate_expr (car values) env))
 ;                (display "ignore"))
;  (newline)
 ; (if(and( not (null? values)) (list? values))
 ;          (display (car values))
 ;          (display "ignore also"))
  ;(display env)
 ; (newline)
 ; (newline)
 ; (newline)
  (cond
    ((and (not (null? names)) (not (null? values)) (list? names) (list? values))
                          (extend_environment (cdr names) (cdr values)
                                              env
                                              (add_new_env_var (car names) (evaluate_expr (car values) env) new_env)))
    ((string? names) (add_new_env_var names (evaluate_expr values env) new_env))
    (else new_env)
  )
)

(define (handle_vars s e1 e2 env)
  (cond
    ((and (list? s) (list? e1)=
           (= (length s) (length e1))
           (not (contains-duplicate? s)))
                     (evaluate_expr e2 (extend_environment s e1 env env)))
     ((string? s) (evaluate_expr e2 (extend_environment s e1 env env)))
     (else (raise (exception "vars: duplicate identifier")))
  )
)

(define (handle_trigger e env)
  (with-handlers
        ([exception?
                   (lambda (e)
                   (raise (exception (exception-exn e))))])
       (let ((res (evaluate_expr e env)))
               (if(exception? res)
                  (raise res)
                  (raise (exception "trigger: wrong argument type"))
               )
       )
  ) 
)


(define (handle_e2 e1 e2 e3 env)
    (with-handlers
        ([exception?
                   (lambda (e)
                   (if (string=? (exception-exn e1)
                                 (exception-exn e))
                       (evaluate_expr e3 env)
                       (raise e)))])
       (evaluate_expr e2 env)
    )
)


(define (evaluate_handle e1 e2 e3 env)
  (with-handlers
        ([exception?
                   (lambda (e)
                   (raise (exception (exception-exn e))))])
     (let ((e1_eval (evaluate_expr e1 env)))
          (if (not (exception? e1_eval))
              (raise (exception "handle: wrong argument type"))
              (handle_e2 e1_eval e2 e3 env)
           )
     )
  )           
)

(define (handle_fun expr env)
    (closure env expr)
)

(define (execute_function f env)
;  (newline)
  (evaluate_expr (fun-body f) env)
)

(define (get_env args_names args_values f env cl_env)
  (extend_environment args_names
                      args_values
                      env
                      (extend_environment (fun-name f) f env cl_env))
)


(define (execute_closure e args_values env)
   (let ((args_names (fun-farg (closure-f e)))
         (f (closure-f e))
         (cl_env (closure-env e)))
      (if (= (length args_names)
             (length args_values))
          (execute_function f (get_env args_names
                                       args_values
                                       f
                                       env
                                       cl_env))
          (raise (exception "call: arity mismatch"))
      )
   )
)

(define (handle_call e args_values env)
;  (display args_values)
;  (newline)
 ; (display env)
 ; (newline)
;  (newline)
  (if (closure? e)
      (execute_closure e args_values env)
      (raise (exception "call: wrong argument type"))
  )
)


(define (evaluate_expr expr env)
;  (newline)
;  (display env)
;  (newline)
;  (display expr)
;  (newline)
;  (newline)
    (cond
        ((int? expr) expr)
        ((true? expr) expr)
        ((false? expr) expr)
        ((empty? expr) expr)
        ((exception? expr) expr)
        ((valof? expr) (get_env_var_value (valof-name expr) env))
        ((is_seq expr) (evaluate_seq expr env))
        ((add? expr) (evaluate_add expr env))
        ((mul? expr) (evaluate_mul expr env))
        ((?leq? expr) (evaluate_leq expr env))
        ((~? expr) (evaluate_inv expr env))
        ((if-then-else? expr) (evaluate_branching expr env))
        ((?=? expr) (evaluate_equal expr env))
        ((head? expr) (evaluate_head (head-e expr) env))
        ((tail? expr) (evaluate_tail (tail-e expr) env))
        ((?any? expr) (evaluate_any (?any-e expr) env))
        ((?all? expr) (evaluate_all (?all-e expr) env))
        ((?int? expr) (toBool (int? (evaluate_expr (?int-expr expr) env))))
        ((?empty? expr) (toBool (empty? (evaluate_expr (?empty-expr expr) env))))
        ((?bool? expr) (toBool (isBool (evaluate_expr (?bool-expr expr) env))))
        ((?exception? expr) (toBool (exception? (evaluate_expr (?exception-expr expr) env))))
        ((?..? expr) (toBool (is_seq (?..-expr expr))))
        ((?seq? expr) (is_real_eval_seq (evaluate_seq (?seq-expr expr) env)))
        ((vars? expr) (handle_vars (vars-s expr) (vars-e1 expr) (vars-e2 expr) env))
        ((trigger? expr) (handle_trigger (trigger-e expr) env))
        ((handle? expr) (evaluate_handle (handle-e1 expr) (handle-e2 expr) (handle-e3 expr) env))
        ((fun? expr) (handle_fun expr env))
        ;((proc? expr) (handle_proc (proc-name expr) (proc-body expr) env))
        ((call? expr) (handle_call (evaluate_expr (call-e expr) env) (call-args expr) env))
        (else (raise (exception "command not yet supported")))
    )
)

(define-syntax-rule (greater e1 e2)
      (if (eval (evaluate_leq (?leq e1 e2)))
          (false)
          (true))
)


(define (reverse_helper lst acc)
  (if (empty? lst)
      acc
      (reverse_helper (..-t lst) (.. (..-h lst) acc))))
  


(define-syntax-rule (rev e)
  (let ((eval_expr (evaluate_expr e null)))
       
      (if (eval (is_real_eval_seq eval_expr) )
          (reverse_helper eval_expr (empty))
          (raise (triggered (exception "rev: argument must be of type ?seq")))
      )
  )
)

(define (build_binary_list n acc)
  (if (= (eval n) 0)
      acc
      (build_binary_list (int (quotient (eval n) 2)) (.. (int (remainder (eval n) 2)) acc))
  )
)
 

(define-syntax-rule (binary e1)
  (let ((e (evaluate_expr e1 null)))
    (if (and (int? e) (positive? (eval e)))
        (if (= (eval e) 0)
          (.. (int 0))
          (build_binary_list  e (empty)))
        (raise (triggered (exception "binary: argument must be a positive integer")))
    )
  )
)

(define (map_helper f lst)
    (if (empty? lst)
        (empty)
        (.. (f (..-h lst)) (map_helper (..-t lst)))
    )
)

(define-syntax-rule (mapping f seq)
    (map_helper f (evaluate_expr seq null))
)

(define (filter_helper f lst)
  (if (empty? lst)
      (empty)
      (if (eval (f (..-h lst)))
          (.. (..-h lst) (filter_helper (..-t lst)))
          (filter_helper (..-t lst))
      )
  )
)

(define-syntax-rule (filtering f seq)
    (filter_helper f (evaluate_expr seq null))
)

(define (folding_helper f init seq)
   (if (empty? seq)
       init
       (folding_helper f (f (..-h seq) init) (..-t seq))
   )
)

(define-syntax-rule (folding f init seq)
  (folding_helper f (evaluate_expr init null) (evaluate_expr seq null))
)
             
(define (start expr env)
  (evaluate_expr expr env)
)


(define (fri expr env)
    (with-handlers
        ([exception?
                   (lambda (e)
                   (triggered (exception (exception-exn e))))]
         [triggered? (lambda (e) (triggered (triggered-exc e)))]
;         [exn? (lambda (e)
;                 (triggered (exception "unknown exception")))]
         )
       (start expr env)
  ) 
)

