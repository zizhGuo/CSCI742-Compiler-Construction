; Auther: Tao Yang, Zizhun Guo
; Part A of HW 2


(ns assign4.genPar
	(:use [assign4.lexer :only (make-next token-lexeme token-name token-value)])
	)

(def rule-lhs first)

(def rule-rhs second)

(def variable? symbol?)

(defn terminal? [a] (and (seq? a) (not (empty? a)) (= (first a) 'quote)))

(defn in? 
  ;true if coll contains elm
  [coll elm]  
  (some #(= elm %) coll))

; (println (terminal? (quote 'a)))
;1

(defn epsilon? [e] (and (list? e) (empty? e)))

(defn without-empty [l]
	(remove 
		epsilon? l))

(defn remove-quote [t] (second t))

(defn vars-from-grammar [g] 
	(if (empty? g)
		()
		(let [l (vars-from-grammar (rest g))]
			(if (in? l (first (first g)))
				l
				(cons (first (first g)) l)))))

(def par-spec '( (S (E 'EOF))
					(E (T E2))
					(E2 ('PLUS T E2))
					(E2 ('MINUS T E2))
					(E2 ())
					(T (F T2))
					(T2 ('STAR F T2))
					(T2 ('SLASH F T2))
					(T2 ())
					(F ('INT))
					(F ('ID))
					(F ('MINUS F))
					(F ('OP E 'CP))))
(defn make-sumi [e1 e2] (list '+i e1 e2))

(defn make-diffi [e1 e2] (list '-i e1 e2))

(defn make-prodi [e1 e2] (list '*i e1 e2))

(defn make-quoi [e1 e2] (list 'divi e1 e2))

(defn make-negi [e] (list 'negi e))

(def par-spec-actions
      (list 
      	(fn [$1 $2] $1)
        (fn [$1 $2] ($2 $1))
		    (fn [$1 $2 $3] (fn [T] ($3 (make-sumi T $2))))
		    (fn [$1 $2 $3] (fn [T] ($3 (make-diffi T $2))))
		    (fn [] (fn [T] T))
		    (fn [$1 $2] ($2 $1))
		    (fn [$1 $2 $3] (fn [F] ($3 (make-prodi F $2))))
		    (fn [$1 $2 $3] (fn [F] ($3 (make-quoi F $2))))
		    (fn [] (fn [F] F))
		    (fn [$1] $1)
		    (fn [$1] $1)
		    (fn [$1 $2] (make-negi $2))
		    (fn [$1 $2 $3] $2)))

	    
;(println (vars-from-grammar par-spec))

(defn terminals-from-grammar [g]
	(if (empty? g)
		()
		(let [
			l (second (first g))
			res (terminals-from-grammar (rest g))
			terminals (filter terminal? l) 
			rqTerminals (map #(remove-quote %) terminals)
			]
			(distinct (concat rqTerminals res)))))

;(println (terminals-from-grammar g))

(defn apply-grammar [g]
	(fn [vari]
		(remove #(not (= (first %) vari)) g)))

;(println ((apply-grammar par-spec) 'T2))

(defn append [L1 L2]
   (if (empty? L1)
       L2
       (conj (append (rest L1) L2) (first L1))))


;1a
(defn first-alpha2 [hashmap gs]  ;gs = ('PLUS F E2)  or (F E2)  or ()
 	(cond (empty? gs) '(())
              (terminal? (first gs)) (hashmap (remove-quote (first gs)))
              :else (let [X (first gs)                       ; X is a grammar symbol
                          firstX (hashmap X)]
                          (if (some #{(list)} firstX)
	    	          (distinct (concat (first-alpha2 hashmap (rest gs)) (without-empty firstX)))
	    	          firstX))))


;1b
(defn build-first-sym-table [g]
	(let [
		vars (vars-from-grammar g)
		ters (terminals-from-grammar g)
		varsList (map #(list %, '()) vars)
		tersList (map #(list %, (list %)) ters)
		mergeList (concat varsList tersList)
		hashList (apply concat mergeList)
		FXhashMap (atom (apply hash-map hashList)); atom, so it can change
		modify (atom 1); as a sign of hashmap modification
		]
		(while (> @modify 0); if there is modification, no matter how many... will loop
			(do
				(reset! modify 0); reset modification to 0
				(loop [rules g]; loop the element in g
					(when (not (empty? rules))
						;# for test, check the process
						; (println "in hashmap" (@FXhashMap (rule-lhs (first rules))))
						; (println "inputs " (rule-rhs (first rules)))
						; (println "returned " (first-alpha2 @FXhashMap (rule-rhs (first rules))))
					 	(let [newRhs (distinct ; get the new right hand by call first-alpha2
												 		(concat
												 			(@FXhashMap (rule-lhs (first rules)))
															(first-alpha2 @FXhashMap (rule-rhs (first rules)))))
					 	]
					 	; (println newRhs)
					 	; (println (@FXhashMap (rule-lhs (first rules))))
					 	(if (not (= newRhs (@FXhashMap (rule-lhs (first rules)))))
					 	; if there is difference update the hashmap
					 		(do 
					 			(swap! modify inc); mark the modification by add 1
					 			(swap! FXhashMap assoc (rule-lhs (first rules)) newRhs)))

					(recur (rest rules))))))); recur for the loop
		@FXhashMap))

;(println (build-first-sym-table par-spec))


;1c
(defn first-alpha [g]
	(let [
		table (build-first-sym-table g)
		]
		(fn [gsList]
			(first-alpha2 table gsList)
			)))

;(println ((first-alpha par-spec) '(E2 F)))


; 2a
; (defn getBeta [sym symList]
; 	; return the beta in problem
; 	(if (= (first symList) sym)
; 		(rest symList)
; 		(recur sym (rest symList))))


(defn build-follow-sym-table [g firstSetFun]
	(let [
		vars (vars-from-grammar g)
		varsList (apply concat (map #(list %, ()) vars))
		FolhashMap (atom (apply hash-map varsList)); atom, so it can change
		modify (atom 1); as a sign of hashmap modification
		]
		(while (> @modify 0); if there is modification, no matter how many... will loop
			(do
				;(println "in while")
				(reset! modify 0); reset modification to 0
				(loop [rules g]
					(when (not (empty? rules))
						;(println "loop1")
						(let [
							k (rule-lhs (first rules))
							r (rule-rhs (first rules))
							]

							(loop [rhs r]
								(when (not (empty? rhs))
									;(println "loop2")
									(if (variable? (first rhs))
										(let [
											v (first rhs)
											nextFirstSet (if (> (count rhs) 1) (firstSetFun (rest rhs)) ())
											leftFollowSet (if (in? (firstSetFun (rest rhs)) ()) (@FolhashMap k) ())
											oldFollowset (@FolhashMap v)
											newFollowSet (without-empty (distinct (concat nextFirstSet leftFollowSet oldFollowset)))
											]
											(if (not= (count oldFollowset) (count newFollowSet))
												(do
													(swap! modify inc)
													(swap! FolhashMap assoc v newFollowSet)))))
								(recur (rest rhs))))
							)
					(recur (rest rules))))
				))
		@FolhashMap))


;(println (build-follow-sym-table par-spec (first-alpha par-spec)))
;(println (getBeta 'B '(A B C)))
;(defn build-follow-sym-table [g firstSetFun]
;	(let [
;		vars (vars-from-grammar par-spec)
;		getRhss (map #(rule-rhs %) par-spec)
;		filterRhs (fn [sym] (filter #(in? % sym) etRhss))
;
;		]
;		getRhss)
;	)
;(println "follow hashmap is ->" (build-follow-sym-table par-spec (first-alpha par-spec)))



(defn follow-var [g firstSetFun]
	(let [
		table (build-follow-sym-table g firstSetFun)
		]
		(fn [gs]
			(table gs))))

;(println "follow set of T2 -> " ((follow-var par-spec (first-alpha par-spec)) 'T2))


;3
; helper: check if a list contains epsilon.
; (defn f [l]
;  	(if (empty? l) false
;  	(or (epsilon? (first l)) (f (rest l)))))

; ; check if the list of right hand side contains a epsilon.
; (defn contains-epsilon? [g lhs]
;  	(let [rhsList ((apply-grammar par-spec) (first lhs))
;        deList (map second rhsList)]
;  	(f deList)))
           

(defn make-predict [g]
  (let [
  	firstF (first-alpha g)
    followF (follow-var g firstF)
        ]
  (fn [rule]   ; a list i.e. (E (T E2))
      (let [firstSetAlpha (firstF (rule-rhs rule))]
        (if (some #(= () %) firstSetAlpha)
					(let [followSetA (followF (rule-lhs rule))]
						(distinct (concat (without-empty firstSetAlpha) followSetA)))
					(without-empty firstSetAlpha))))))

;;;test for 3 print all predict
 ; (loop [
 ; 	n (count par-spec)
 ; 	i 0]
 ; 	(when (< i n)
 ; 		; (println "predict of " (nth par-spec i) "is : " ((make-predict par-spec) (nth par-spec i)))
 ; 		(recur n (+ i 1))))

 ;(println "predict of T2 = () is : " ((make-predict par-spec) (nth par-spec 4)))

;4

(defn make-actionSym [n f] (list 'action n f))

;(defn actionSym? [l] (if (empty? l) false (= (first l) 'action))); will not pass special case. 
(defn actionSym? [l] (and (list? l) (not (empty? l)) (= (first l) 'action)))
; special test case for 4b
;(println (actionSym? ['action 1 1]))

; (defn actionSym-count [l] (if (actionSym? l) (nth l 1) -1))
; (defn actionSym-action [l] (if (actionSym? l) (nth l 2) -1))
(defn actionSym-count [l] (nth l 1))

(defn actionSym-action [l] (nth l 2))

;5 
(defn make-oracle [g actions]
	(let [
		nList (map #(count (rule-rhs %)) g)
		mergeNF (partition 2 (interleave nList actions))
		actionList (map #(make-actionSym (first %) (second %)) mergeNF)
		extendG (map #(list (rule-lhs (first %)) (rule-rhs (first %)) (second %)) 
			(partition 2 (interleave g actionList)))
		vars (vars-from-grammar g)
		ters (terminals-from-grammar g)
		tersHList (apply concat (map #(list % ()) ters))
		oracleUnit (atom (apply hash-map tersHList))
		varsHList (apply concat (map #(list % @oracleUnit) vars))
		oracle (atom (apply hash-map varsHList))
		PredictList (map #(list % ((make-predict g) %)) extendG)
		]
		;(println actionList)
		;(println mergeNF)
		;(println (count mergeNF))
		(loop [
			ps PredictList
			g g
			]
			(when (not (empty? ps))
				(loop [
					extRhs (concat
						(second (first (first ps)))
						(list (nth (first (first ps)) 2)))
					rule (first g)
					p (second (first ps))
					]
					;(println extRhs)
					(when (not (empty? p))
						(swap! oracle assoc-in [(rule-lhs rule) (first p)] extRhs)
						;(println (@oracle (rule-lhs rule)))
						(recur extRhs rule (rest p)))
					)
				(recur (rest ps) (rest g))))

		; 	;print oracle
		;   (loop [
		; 	 	vars (vars-from-grammar par-spec)
		; 	 		]
		; 	 	(when (not (empty? vars))
		; 	 		;(println (first vars) "->" (@oracle (first vars)))
		; 	 		(recur (rest vars))))
		; ;(println @oracle )

		(let [
			getOne 
			(fn [vari termi] 
				(do
					;(println vari termi "------")
					(if (empty? ((@oracle vari) termi))
						(throw (Exception. "There is no right-hand-side entry"))
						((@oracle vari) termi)))
				)
			]
			(fn [vars ters] 
				(let [inputs (partition 2 (interleave vars ters))]
					;(println inputs)
					(map #(getOne (first %) (second %)) inputs)))
			)))

;;test 5
;(println ((make-oracle par-spec par-spec-actions) '(E) '(INT)))



;6
;(defn peekf (fn [nt] (nth (nt) 2)))
;(def next-token (make-next "4 * 5")


; Test1: let peekf -> (fn [] '())
;        let advancef -> (fn [] '())
;	 Initialize stack with ('E eof)
;	 Initialize semanticstack with '()


(defn parse [oraclef peekf advancef stack semanticstack]   
	; oraclef  <- function [(Varieable) (grammar-terminals)] 
	;             output: (extended RHS) i.e. (E T2 ('action ...))                                                     
	; peekf    <- function [?] output: The "name" of the first token
	; advancef <- function [?] output: N/A  
	; stack    <- list
	; semanticstack <- list
  (let [
	  stk (atom stack)                                ;make stack as "atom"
	  sstk (atom semanticstack)                       ;make seman-stack as "atom"
	  pop-update (fn [s] (reset! s (rest @s)))        ;pop: modification for updating original stack/semanStack
	  pop-first (fn [s] (first @s))                   ;pop: get top element
	  pop-update-sstk-n	(fn [n] (reset! sstk (drop n @sstk)))
	  push-update-stk (fn [x] (reset! stk (concat x @stk)))       ;push: modification for updating original stack; x: right-hand sides symbols
	  push-update-sstk (fn [x] (reset! sstk (cons x @sstk)))    ;push: modification for updating original semantic stack; x: values
	  push-update-sstk-value (fn [value] (reset! sstk (cons value @sstk))) ;push: modification for updating original semanStack; token: token
	  ]
    (do
     	(while (first @stk)
     		;(println "stack" @stk)
    		;(println "semantic stack" @sstk)
				;??: possible to consider if (pop-first) starts with "'quote" ??: peekf already did advance
        (cond 
        	(and (terminal? (pop-first stk)) (= (remove-quote (pop-first stk)) (token-name (peekf))))             
	          (do 
	          	(push-update-sstk-value (token-value (peekf)))
	          	;(println "semantic-stack------>" @sstk)
	          	(advancef)
	          	;(println "stack is -------------------------->" @stk)
				      (pop-update stk)
	          	)

					(variable? (pop-first stk))
					  (let [temp-first (pop-first stk)]
			        (do
			        	;(println temp-first)
                (pop-update stk)
                 ;look up on oracle table and return the first rhs rule; Zizhun
                ;(println (= (list temp-first) '(E)) (= '(INT) (list (token-name (peekf))) ))
                ; (println "************************------------>" (oraclef '(E) '(INT)))
                ; (println "************************------------>" (oraclef (list temp-first) (list (token-name (peekf)))))
                (push-update-stk (first (oraclef (list temp-first) (list (token-name (peekf))))))
               
                ))
					(actionSym? (pop-first stk))
					  (let [
					  		n (actionSym-count (pop-first stk))
					    action (actionSym-action (pop-first stk))
					    topN (reverse (take n @sstk)); get the top N
							]
						  (do
						  	;(println "n =" n)
											; (println "|---->" @stk)
											; (println "topN : |---->" topN)
					      (pop-update stk)
	        		(pop-update-sstk-n n)
	        		; (println "pop number" n)
	        		; (println "before action" @sstk)
      					(push-update-sstk (apply action topN))
      					; (println "after" @sstk);test
      					))))
 			(first @sstk))))


;7
; ??issue: peekf and advance function are not defined here.
(defn make-parser [g actionList]
      (let [
      	oraclef (make-oracle g actionList)
      	stack (concat (rule-rhs (first g)) (list (make-actionSym (count (rule-rhs (first g))) (first actionList))))
      	semantic-stack '()]
	      (fn [next-token]
          (let [
          	token (atom '())
          	advancef (fn [] (reset! token (next-token)))
          	peekf (fn [] (if (empty? @token) (do (advancef) @token) @token))
	          ]
	          (do
	          	;(println "next token" (peekf))
          		(parse oraclef peekf advancef stack semantic-stack)))))
      			)

