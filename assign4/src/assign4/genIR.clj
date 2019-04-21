(ns assign4.genIR
	(:use [assign4.parser])
	(:use [assign4.machine]))


; 1c

(defn gen-formal [] (gensym "x*"))

(defn gen-thunk-name [] (gensym "f*"))

; 1d

(declare match-pattern)

(defn formal? [vari]
	(and (variable? vari) (= (first (str vari)) \x) (= (second (str vari)) \*)))

(defn patToLetHelper [vars num exp identM identF]
	(if (empty? vars)
		exp
		(let [
			bond (gen-formal)
			exp2 (match-pattern (last vars) exp bond identF)
			exp1 (make-let1 bond (make-call '*getTuple* (make-tuple (list identM (dec num)))) exp2)
			]
			(recur (drop-last vars) (dec num) exp1 identM identF)
			)))

(defn patToLet [vars exp identM identF]
	(let [
		num (count vars)
		]
		(patToLetHelper vars num exp identM identF)))

;The pattern operand is either Unit(() ), pattern tuple(TuplePat ........) or a pattern (call-pat .......)
; Need to be modified
(defn match-pattern [pat exp identM identF]
	(let [
		topIfCond (make-call 'isTupleLike identM)
		condElse (make-tail-call-thunk identF)
		namePair (make-tuple (list (make-call '*getTupleName* identM) (arg1 pat)))
		condOneCond (make-call '*isEqual* namePair)
		cOneC1 (patToLet pat exp identM identF)
		if2 (make-if condOneCond cOneC1 condElse)
		if1 (make-if topIfCond if2 condElse)
		]
		if1))

(defn match-pattern [pat exp identM identF]
	(do 
	(cond
		(variable? pat)
			(make-let1 pat identM exp)
		(constant-pat? pat) 
			(make-if 
				(cond 
					(empty-list? pat)
						(make-call '*isEmptyList* identM)
					(unit? pat)
						(make-call '*isUnit* identM)
					:else	(make-call '*isEqual* (make-tuple (list identM pat))))
				exp
				(make-tail-call-thunk identF))
		(tuple-pat? pat)
			(make-if
				(make-call '*isTupleLike* identM)
				(make-if
					(make-call '*isEqual* (make-tuple (list (make-call '*getTupleName* identM) "*anonymous*")))
					(patToLet (arg1 pat) exp identM identF)
					(make-tail-call-thunk identF))
				(make-tail-call-thunk identF))
		(call-pat? pat) 
			(make-if
				(make-call '*isTupleLike* identM)
					(make-if
						(make-call '*isEqual* (make-tuple (list (make-call '*getTupleName* identM) (str (arg1 pat)))))
						(cond
							(tuple-pat? (arg2 pat)) (patToLet (arg1 (arg2 pat)) exp identM identF)
							(unit? (arg2 pat)) (identM)
							(tuple? pat) ()
						)
						(make-tail-call-thunk identF))
				(make-tail-call-thunk identF))
		))
	)




(def cc1 '(case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))))
)
(def caseexp1 '(case x*1526 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))

(def caseexp2 '(lambda1 x*1533 (case x*1533 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2))))))))))))))



;(println (match-pattern pat1 false 'lst 'label))

; (defn transform-exp [exp])
(declare transform-exp)
(declare transform-case)

; 1e
; (defn transform-case [case-exp]
;   (let [
; 		label (gen-thunk-name) ;;AKA the (fail) indentifier
; 		var (nth case-exp 1) ;; AKA the (match) identifier
; 		pair (first (arg2 case-exp))
; 		e (first pair)
; 		p (second pair)
; 		]
;     (make-let-thunk label (transform-exp (first (nth case-exp 2))) (match-pattern p e label var))
; ))
(defn transform-case [case-exp]
	(do 
		;(println "case-exp" case-exp)
		(cond
			(empty? (arg2 case-exp)) (make-error  "No patterns matched.")
		 	(or (= (first (first (arg2 case-exp))) true) (= (first (first (arg2 case-exp))) false))
				(make-if (arg1 case-exp) (arg1 (first (arg2 case-exp))) (arg1 (second (arg2 case-exp))))
			:else
				(do 
					;(println "seocnd--" (second (first (arg2 case-exp))))
					(if (formal? (arg1 case-exp))
						(let [
							tn (gen-thunk-name)
							top (first (arg2 case-exp))
							]
							(make-let-thunk 
								tn 
								(transform-case (make-case (arg1 case-exp) (rest (arg2 case-exp))))
								(match-pattern (first top) (transform-exp (second top)) (arg1 case-exp) tn)))
						(let [
							fm (gen-formal)
							target (arg1 case-exp)
							tn (gen-thunk-name)
							top (first (arg2 case-exp))
							new-case (make-case fm (arg2 case-exp))
							]
							(make-let1 
								fm 
								target
								(make-let-thunk 
									tn 
									(transform-case (make-case (arg1 new-case) (rest (arg2 new-case))))
									(match-pattern (first top) (transform-exp (second top)) (arg1 new-case) tn)))))))))

 ;(case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))
; 1f
(defn transform-exp [exp]
	(do 
	;(println "comming in first" exp)
          (cond
            (not (list? exp)) exp 	
            (and (list? exp) (= (first exp) 'case)) 
            (transform-case exp)
                                        ; =(and (list? (first exp)) (= (first (first exp)) 'case))
                                        ; 	(cons (transform-case (first exp)) (transform-exp (rest exp)))
            (empty? exp) () ;; empty
            (and (list? exp) (not (= (first exp) 'case)))
            (do 
                                        ;(println exp)
              (cons (transform-exp (first exp)) (transform-exp (rest exp))))
            :else 
            (do 
              (cons (first exp) (transform-exp (rest exp))) 
              )
            ))
  )


(def letcase '(case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))
)

;(println (transform-case caseexp1));---> tested
;(println (transform-case letcase));---> tested  why (if (call *isEqual* (Tuple (L1 (EmptyList)))) <----> 	(if (call *isEmptyList* L1)
;(println (transform-exp caseexp2));--->tested


; 1g
(defn gen-rename [] (gensym "y*"))

; 1h



; (defn renameHelper [exp map]
; 	(if (empty? exp)
; 		()
; 		(let [
; 			top (first exp)
; 			]
; 			(cond 
; 				(formal? top) 
; 					(if (nil? (@map top))
; 						(do
; 							(reset! map (assoc @map top (gen-rename)))
; 							(cons (@map top) (renameHelper (rest exp) map)))
; 						(cons (@map top) (renameHelper (rest exp) map)))
; 				(list? top)
; 					(cons (renameHelper top map) (renameHelper (rest exp) map))
; 				:else (cons top (renameHelper (rest exp) map))
; 			)))
; )
(declare renameHelper)

(defn renameHelperTuple [lst map] 
  (if (empty? lst)
    ()
    (cons (renameHelper (first lst) map) (renameHelperTuple (rest lst) map))))


(defn renameHelper [exp map]
  (cond 
    (variable? exp) 
    (if (= \* (first (str exp)))
      exp
      (do
        (if (nil? (@map (str exp))) 
          (reset! map (assoc @map (str exp) (gen-rename))))
        (@map (str exp))
        ))
    (tuple? exp)
    (make-tuple (renameHelperTuple (arg1 exp) map) )
    (constant? exp) exp
    (if? exp)
    (make-if (renameHelper (arg1 exp) map) (renameHelper (arg2 exp) map) (renameHelper (arg3 exp) map))
    (let1? exp) 
    (make-let1 (renameHelper (arg1 exp) map) (renameHelper (arg2 exp) map) (renameHelper (arg3 exp) map))
    (call? exp)
    (make-call (renameHelper (arg1 exp) map) (renameHelper (arg2 exp) map))
    (lambda1? exp)
    (make-lambda1 (renameHelper (arg1 exp) map) (renameHelper (arg2 exp) map))
    (let-thunk? exp) 
    (make-let-thunk (arg1 exp) (renameHelper (arg2 exp) map) (renameHelper (arg3 exp) map))
    :else exp
    ))

(defn rename-locals [exp]
	(let [
		map (atom (hash-map))
		]
		(renameHelper exp map)))

;1i
(defn gen-lambda-name[] (gensym "*anon*"))

;1j

 (defn funs-from-exp-helper [exp idList]
   (cond 
     ;; 1. Variable
     (variable? exp) 
     (vector exp ())
     
     ;; 2. Tail-call-thunk
     (tail-call-thunk? exp)
     (vector exp ())
     
     ;; 3. Tuple
     (tuple? exp)
     (let [
           tuple-vs (map #(funs-from-exp-helper % idList) (arg1 exp))
           modified-exp (map first tuple-vs)
           idList-updated (apply concat (map second tuple-vs))
           ] 
       (vector (make-tuple modified-exp) idList-updated))
     
     ;; 4. Constant
     (constant? exp) 
     (vector exp ())
     
     ;; 5. If
     (if? exp)
     (let [
           v1 (funs-from-exp-helper (arg1 exp) idList)
           v2 (funs-from-exp-helper (arg2 exp) idList)
           v3 (funs-from-exp-helper (arg3 exp) idList)
           exp-updated (make-if (first v1) (first v2) (first v3))
           idList-updated (concat (second v1) (second v2) (second v3))
           v-updated (vector exp-updated idList-updated)
           ]
       v-updated)
     
     ;; 6. Let1
     (let1? exp) 
     (let [
           v2 (funs-from-exp-helper (arg2 exp) idList)
           v3 (funs-from-exp-helper (arg3 exp) idList)
           exp-updated (make-let1 (arg1 exp) (first v2) (first v3))
           idList-updated (concat (second v2) (second v3))
           v-updated (vector exp-updated idList-updated)
           ]
       v-updated)

     ;; 7. Let-thunk
     (let-thunk? exp)
     (let [
           v2 (funs-from-exp-helper (arg2 exp) idList)
           v3 (funs-from-exp-helper (arg3 exp) idList)
           exp-updated (make-let-thunk (arg1 exp) (first v2) (first v3))
           idList-updated (concat (second v2) (second v3))
           v-updated (vector exp-updated idList-updated)
           ]
       v-updated) 
     
     ;; 8. Call
     (call? exp)
     (let [
           operator (arg1 exp)
           operand (arg2 exp)
           v (funs-from-exp-helper operand idList)
           operand-updated (first v)
           idList-updated (second v)
           ]
       (cond
         (lambda1? operator)
         (let [
               let-operand (funs-from-exp-helper (arg2 operator) idList)
               ]
           (vector (make-let1 (arg1 operator) operand-updated (first let-operand)) (concat idList-updated (second let-operand))))
         (and (variable? operator) (not (= nil (some #(= % operator) (map first idList)))))
         (vector (make-global-call operator operand-updated) idList-updated)
         :else
         (let [
               var-v (funs-from-exp-helper operator idList)
               operand-updated-new (first var-v)
               idList-updated-new (concat idList-updated (second var-v))
               ]
            (vector (make-call operand-updated-new operand-updated) idList-updated-new))))
     
     ;; 9. Lambda1
     (lambda1? exp)
     (let [
           anno (gen-lambda-name)
           v (funs-from-exp-helper (arg2 exp) idList)
           exp-updated (first v)
           idList-updated (second v)
           lambda-updated (make-lambda1 (arg1 exp) exp-updated)
           
           new-let (make-let '! closure-var '!!!)
           
           exp-closure (make-closure anno '!!)
           new-pair-body (make-lambda1 (arg1 exp) new-let)
           
           new-idList (concat (list (list anno new-pair-body)) idList-updated)
           ]
       (vector exp-closure new-idList)
       )    
         
     ;; 10. error
     (error? exp)
     (let [
           v (funs-from-exp-helper (arg1 exp) idList)
           exp-updated (first v)
           idList-updated (second v)
           v-updated (vector (make-error exp-updated) idList-updated) 
           ]
       v-updated)

     ;; 11. Exception
     :else 
     (throw (Exception. "exp is invalid"))))

(defn funs-from-exp-top [exp idList] 
  (if (lambda1? exp)
    (let [
          v (funs-from-exp-helper (arg2 exp) idList)
          exp-updated (first v)
          idList-updated (second v)
          lambda-updated (make-lambda1 (arg1 exp) exp-updated)
          v-updated (vector lambda-updated idList-updated)
          ]
      v-updated
      )
    (funs-from-exp-helper exp idList)))




(def exp1 '(lambda1 x*1546 (case x*1546 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1))))))))

;; (def exp2 '(fact (lambda1 x*1526 (case x*1526 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))))
;; (println (rename-locals (transform-exp exp1)))

(println (funs-from-exp-top (rename-locals (transform-exp exp1)) '() ))
;(println (transform-exp exp1))

; (def pat1 '(TuplePat (x (TuplePat (x xs)))))
; ;(println (match-pattern pat1 false 'lst 'label))

; (defn transform-5tuple [])

; (defn gen-from-6tuple [])
