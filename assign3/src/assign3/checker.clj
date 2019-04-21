(ns assign3.checker
	(:use [assign3.parser])
	(:use [assign3.unification :only (unifyTerm4 
		failure
		extend-history
		theta-identity
		logic-variable?
		applyUnifier
		unifyTerm2)])
	(:use [assign3.unification :only (extendUnifier)])
	)

; ;1a

; (defn arg1[l] (nth l 1))
; (defn arg2[l] (nth l 2))
; (defn arg3[l] (nth l 3))
; (defn arg4[l] (nth l 4))

; (defn variable? [l] (symbol? l))
; (defn boolean? [l] (or (= false l) (= true l)))
; (defn double? [l] (and (number? l) (not (integer? l))))
; (defn unit? [l] (= (first l) 'Unit))
; (defn empty-list? [l] (= (first l) 'Emptylist))
; (defn constant? [l] (or (boolean? l) (double? l) (unit? l) (empty-list? l) (integer? l) (string? l)))
; (defn tuple? [l] (= (first l) 'Tuple))
; (defn decls? [l] (= (first l) 'decls))
; (defn data-decls? [l] (= (first l) 'decl-data))
; (defn data-construct? [l] (= (first l) 'constructor))
; (defn type-decl? [l] (= (first l) 'decl-type))
; (defn def? [l] (= (first l) 'def))
; (defn fun-def? [l] (= (first l) 'defun))
; (defn or? [l] (= (first l) 'or))
; (defn and? [l] (= (first l) 'and))
; (defn if? [l] (= (first l) 'if))
; (defn case? [l] (= (first l) 'case))
; (defn let? [l] (= (first l) 'let))
; (defn lambda? [l] (= (first l) 'lambda))
; (defn lambda1? [l] (= (first l) 'lambda1));?
; (defn error? [l] (= (first l) 'error))
; (defn call? [l] (= (first l) 'call))
; (defn boolean-type? [l] (= (first l) 'BoolType))
; (defn integer-type? [l] (= (first l) 'IntType))
; (defn double-type? [l] (= (first l) 'DoubleType))
; (defn string-type? [l] (= (first l) 'StringType))
; (defn unit-type? [l] (= (first l) 'UnitType))
; (defn (constant-type? )[l] (or (boolean-type? l) (integer-type? l) (double-type? l) (string-type? l) (unit-type? l)))
; (defn tuple-type? [l] (= (first l) 'TupleType))
; (defn arrow-type? [l] (= (first l) 'ArrowType))
; (defn named-type? [l] (= (first l) 'NamedType))
; (defn forall-type? [l] (= (first l) 'forall))
; (defn unit-pat? [l] (= (first l) 'Unit))
; (defn empty-list-pat? [l] (= (first l) 'EmptyList))
; (defn constant-pat? [l] (or (boolean? l) (integer? l) (double? l) (string? l) (unit-pat? l) (empty-list-pat? l)))
; (defn tuple-pat? [l] (= (first l) 'TuplePat))
; (defn call-pat? [l] (= (first l) 'call-pat))


; (defn make-forall-type [L type] (list 'forall L type))

; (defn make-lambda1 [idL exp] (list 'lambda1 idL exp))

;2a i
(defn free-variables-from-type [type]
	(cond
		(variable? type) type
		(constant-type? type) ()
		(tuple-type? type) 
			(let [res (atom ())]
				(loop [t (arg1 type)]
					(when (not (empty? t))
						(let [r (free-variables-from-type (first t))]
							(if (list? r)
								(reset! res (into '() (distinct (concat @res r))))
								(reset! res (into '() (distinct (conj @res r)))))
						(recur (rest t)))))
				@res)
		(arrow-type? type) 
			(let [
				prev (free-variables-from-type (arg1 type)) 
				poss (free-variables-from-type (arg2 type)) 
				]
				(do 
					;(println "type==--->" prev prev)
					(cond
						(and (list? prev) (list? poss)) (into '() (distinct (concat prev poss)))
						(and (list? prev) (not (list? poss))) (into '() (distinct (conj prev poss)))
						(and (not (list? prev)) (list? poss)) (into '() (distinct (conj poss prev)))
						:else (into '() (distinct (list prev poss)))
						)))
		(named-type? type) (free-variables-from-type (arg2 type))
		(variable? type) type
		:else (do (println "this --->" type) (throw (Exception. "Unexpected Input --- not a type")))))

; (def tree '(ArrowType (TupleType ((ArrowType a b) (NamedType *List* a))) (NamedType *List* b)))
; (print (free-variables-from-type tree))


;2a ii
(defn universalize-type [type]
	(do 
		(make-forall-type (free-variables-from-type type) type)))

; (def tree '(TupleType ((ArrowType a b) (NamedType *List* a))))
; (print (universalize-type tree))

;2b
(defn types-from-data [triList]
	(let [
		split 
			(reverse (into '() (apply concat (map 
				(fn [item] (map #(list (first item) (second item) %) (arg2 item))) triList))))
		bindList 
			(map 
				#(list 
					(arg1 (arg2 %)) 
					(universalize-type 
						(make-arrow-type 
              (arg2 (arg2 %))
							(make-named-type
								(first %) 
								(make-type-parens (second %))))))
			split)
		]
		bindList
		))

;2c

 (def init '(
	(*==i* ((IntType) (IntType)) (BoolType))
	(*!=i* ((IntType) (IntType)) (BoolType))
	(*<=i* ((IntType) (IntType)) (BoolType)) 
	(*<i* ((IntType) (IntType)) (BoolType))
	(*>=i* ((IntType) (IntType)) (BoolType)) 
	(*>i* ((IntType) (IntType)) (BoolType)) 
	(*==d* ((DoubleType) (DoubleType)) (BoolType)) 
	(*!=d* ((DoubleType) (DoubleType)) (BoolType))
	(*<=d* ((DoubleType) (DoubleType)) (BoolType)) 
	(*<d* ((DoubleType) (DoubleType)) (BoolType))
	(*>=d* ((DoubleType) (DoubleType)) (BoolType)) 
	(*>d* ((DoubleType) (DoubleType)) (BoolType))
	(*+i* ((IntType) (IntType)) (IntType))
	(*-i* ((IntType) (IntType)) (IntType)) 
	(**i* ((IntType) (IntType)) (IntType)) 
	(*divi* ((IntType) (IntType)) (IntType)) 
	(*negi* (IntType) (IntType))
	(*+d* ((DoubleType) (DoubleType)) (DoubleType))
	(*-d* ((DoubleType) (DoubleType)) (DoubleType)) 
	(**d* ((DoubleType) (DoubleType)) (DoubleType)) 
	(*divd* ((DoubleType) (DoubleType)) (DoubleType)) 
	(*negd* (DoubleType) (DoubleType))
	(*Cons* (a (NamedType *List* a)) (NamedType *List* a))))
(def init-type-env (map #(list 
	(first %)
	(universalize-type
		(make-arrow-type
			(if (> (count (second %)) 1)
				(make-tuple-type (second %))
				(second %))
			(arg2 %)))
	)
	init))

;2d

(defn gen-formal [] (gensym "x*"))


;2f

; (def exp2 '(let (  (  (TuplePat (r1 r2))   (call quad (Tuple (a b c)))  )  )r1))
; (def exp1 '(let ((e (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c))))))) (d (call sqrt e))) (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))))))
; )
; (def exp3 '(lambda (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a)))))))))) 

(defn reduce-forms [exp]
	(do 
		;(println "----exp----->" exp)
     (cond
       (or? exp) (reduce-forms (list 'if (reduce-forms (arg1 exp)) true (reduce-forms (arg2 exp))))
       (and? exp) (reduce-forms (list 'if (reduce-forms (arg1 exp)) (reduce-forms (arg2 exp)) false))
       (if? exp) (list 'case (reduce-forms (arg1 exp)) (list (list true (reduce-forms (arg2 exp))) (list false (reduce-forms (arg3 exp)))))
       (let? exp) 
	       (let 
	           [expList (reverse (nth exp 1))
	            E (atom (nth exp 2))
	            res (atom ())
	            ]
	         (do
	           ;; (println "expList = " expList)
	           ;; (println "E" @E)
	           ;; (println "res" @res)
	           (loop [L expList] 
	             (when (not (empty? L))
	               (let [e (second (first L))
	                     p (first (first L))
	                     e-red (reduce-forms e)
	                     ]                                
	                 (if (nil? @E)
	                   (reset! res (list 'case e-red (list (list p @res))))
	                   (do
	                     (reset! res (list 'case e-red (list (list p (reduce-forms @E)))))
	                     (reset! E nil)
	                     ;; (println "-----------------")
	                     )))
	               (recur (rest L))))
	           ;; (println "@res = " @res)
	           @res))
	    (lambda? exp)
       	(let [
              pairs-orin (arg1 exp)
              exps (map #(second %) pairs-orin)
              pats (map #(first %) pairs-orin)
              exps-red (map reduce-forms exps)
              pairs-new (partition 2 (interleave pats exps))
              ]
	        (do
           ;; (println "pairs-orin" pairs-orin)
           ;; (println "exps" exps)
           ;; (println "pats" pats)
           ;; (println "exps-red" exps-red)
           ;; (println "pairs-new" pairs-new)
           (list 'lambda1 (gen-formal) (list 'case (gen-formal) pairs-new))))
      :else exp)))


;2e

(defn process-helper [ids exp]
	(do 
		; (println ids exp)
		(if (empty? ids)
			exp
			(recur (rest ids) (make-lambda1 (first ids) exp)))))

(defn groupFunHelper [funs fun]
	(do 
		;(println funs fun)
		(if (empty? funs)
			(list (list fun))
			(let [
				lastGroup (first funs)
				lastOne  (first lastGroup)]
				(if (= (arg1 lastOne) (arg1 fun))
					(conj (rest funs) (conj lastGroup fun))

					(conj (conj (rest funs) (reverse lastGroup)) (list fun))
					)))))

(defn groupFun [fList res]
	(do 
		;(println res)
		(if (empty? fList)
			(reverse res)
			(recur (rest fList) (groupFunHelper res (first fList))))))


(defn process-funs [funList]
	(let [
		fGroup (groupFun funList ())
		countArgList (map (fn [group] (map #(count (arg2 %)) group)) fGroup)
		tfList (map (fn [item] (apply = item)) countArgList)
		flag	(apply = tfList)
		]
		(if (not flag)
			(throw (AssertionError. "Wrong input."))
			(let [
				mergFunList (map (fn [group] (list (second (first group)) (map #(list (arg2 %) (arg3 %)) group))) fGroup)
				headList (map #(first %) mergFunList)
				bodyList (map #(second %) mergFunList)
				cBodyList (map (fn [item] 
					(map #(list 
						(if (> (count (first %)) 1)
							(make-tuple-pat (first %))
							(first (first %)))
							(reduce-forms (second %)))
					item)) bodyList)
				idList (map (fn [item] (map (fn [x] (gen-formal)) (first (first item)))) bodyList)
				caseList (map (fn [id exp] 
					(list 'case (if (> (count id) 1) (make-tuple id) (first id))  exp)) idList cBodyList)
				lambdaList (map (fn [case] 
					(process-helper 
						(if (list? (second case))
							(second (second case))
							(list (second case))
							)
						case)) 
					caseList)
				res  (reverse (into '() (partition 2 (interleave headList lambdaList))))
				]
				res)
		)))
		;)
; (def funs '(
; (defun append ((TuplePat (L1 L2))) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) 
; (defun curAppend ((EmptyList) L) L)
; (defun curAppend ((call-pat *Cons* (TuplePat (x xs))) L) (call *Cons* (Tuple (x (call (call curAppend xs) L)))))))

 


; (def funs '(
; (defun append ((TuplePat (L1 L2))) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) 
; (defun curAppend ((EmptyList) L) L)
; (defun curAppend ((call-pat *Cons* (TuplePat (x xs))) L) (call *Cons* (Tuple (x (call (call curAppend xs) L)))))))

(def tree '(decls ((decl-type pi (DoubleType)) (def pi 3.14) (decl-data Float () ((constructor Box (DoubleType)))) (decl-data MyList (a) ((constructor MT (UnitType)) (constructor Cons (TupleType (a (NamedType MyList a)))))) (decl-data Either (a b) ((constructor Left a) (constructor Right b))) (decl-type arith (IntType)) (def arith (call *+i* (Tuple (2 (call **i* (Tuple (3 4))))))) (decl-type lst1 (NamedType *List* (IntType))) (def lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList))))))))))) (decl-type triple (TupleType ((UnitType) (NamedType *List* a) (NamedType *List* (IntType))))) (def triple (Tuple ((Unit) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList))))))) (decl-type fact (ArrowType (IntType) (IntType))) (defun fact (0) 1) (defun fact (n) (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))) (decl-type mkAdd (ArrowType (IntType) (ArrowType (IntType) (IntType)))) (defun mkAdd (n m) (call *+i* (Tuple (n m)))) (decl-type seven (IntType)) (def seven (call (call mkAdd 2) 5)) (decl-type facta (ArrowType (TupleType ((IntType) (IntType))) (IntType))) (defun facta ((TuplePat (n a))) (if (call *==i* (Tuple (n 0))) a (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))) (decl-type map (ArrowType (TupleType ((ArrowType a b) (NamedType *List* a))) (NamedType *List* b))) (defun map ((TuplePat (f (EmptyList)))) (EmptyList)) (defun map ((TuplePat (f (call-pat *Cons* (TuplePat (x xs)))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs))))))) (decl-type reva (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t))) (defun reva ((TuplePat ((EmptyList) a))) a) (defun reva ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a))) (call reva (Tuple (xs (call *Cons* (Tuple (x a))))))) (decl-type rev2 (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t))) (def rev2 (lambda (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a)))))))))) (decl-type foo (ArrowType (DoubleType) (DoubleType))) (defun foo (-2.718) 1.0) (decl-type bar (ArrowType (TupleType ((ArrowType a b) (NamedType MyList a))) (NamedType MyList b))) (decl-type baz (NamedType Either (TupleType (a b)))) (decl-type test (BoolType)) (def test (or (or (or true false) true) false)) (decl-type test2 (IntType)) (def test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4)))) (decl-type test3 (BoolType)) (def test3 (or (or true (and false true)) false)) (decl-type append (ArrowType (TupleType ((NamedType *List* a) (NamedType *List* a))) (NamedType *List* a))) (defun append ((TuplePat (L1 L2))) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) (decl-type curAppend (ArrowType (NamedType *List* a) (ArrowType (NamedType *List* a) (NamedType *List* a)))) (defun curAppend ((EmptyList) L) L) (defun curAppend ((call-pat *Cons* (TuplePat (x xs))) L) (call *Cons* (Tuple (x (call (call curAppend xs) L))))) (decl-type myAppend (ArrowType (TupleType ((NamedType MyList a) (NamedType MyList a))) (NamedType MyList a))) (defun myAppend ((TuplePat (L1 L2))) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2)))))))))) (decl-type insert (ArrowType (TupleType ((IntType) (NamedType *List* (IntType)))) (NamedType *List* (IntType)))) (defun insert ((TuplePat (x (EmptyList)))) (call *Cons* (Tuple (x (EmptyList))))) (defun insert ((TuplePat (x (call-pat *Cons* (TuplePat (y ys)))))) (if (call *<=i* (Tuple (x y))) (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys)))))) (call *Cons* (Tuple (y (call insert (Tuple (x ys)))))))) (decl-type iSort (ArrowType (NamedType *List* (IntType)) (NamedType *List* (IntType)))) (defun iSort ((EmptyList)) (EmptyList)) (defun iSort ((call-pat *Cons* (TuplePat (x xs)))) (call insert (Tuple (x (call iSort xs))))) (decl-type abs (ArrowType (DoubleType) (DoubleType))) (defun abs (x) (if (call *<d* (Tuple (x 0.0))) (call *negd* x) x)) (decl-type average (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))) (defun average ((TuplePat (x y))) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0)))) (decl-type improveSqrtGuess (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))) (defun improveSqrtGuess ((TuplePat (a g))) (call average (Tuple (g (call *divd* (Tuple (a g))))))) (decl-type epsilon (DoubleType)) (def epsilon 1.0E-10) (decl-type isCloseEnough (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))) (defun isCloseEnough ((TuplePat (x y))) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon)))) (decl-type sqrtIter (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))) (defun sqrtIter ((TuplePat (a g))) (if (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) g (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g)))))))) (decl-type sqrt (ArrowType (DoubleType) (DoubleType))) (defun sqrt (x) (call sqrtIter (Tuple (x 1.0)))) (decl-type quad (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (TupleType ((DoubleType) (DoubleType))))) (defun quad ((TuplePat (a b c))) (let ((e (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c))))))) (d (call sqrt e))) (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))))))) (decl-type maxRoot (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (DoubleType))) (defun maxRoot ((TuplePat (a b c))) (let (((TuplePat (r1 r2)) (call quad (Tuple (a b c))))) r1)) (decl-type head (ArrowType (NamedType *List* a) a)) (defun head (L) (case L (((EmptyList) (error Can't take head.)) ((call-pat *Cons* (TuplePat (x xs))) x)))) (decl-type head2 (ArrowType (NamedType *List* a) (NamedType Either (TupleType ((StringType) a))))) (defun head2 ((EmptyList)) (call Left Oops)) (defun head2 ((call-pat *Cons* (TuplePat (x xs)))) (call Right x)))))
;2g

;'((fact (lambda1 x*1526 (case x*1526 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))) (mkAdd (lambda1 x*1527 (lambda1 x*1528 (case (Tuple (x*1527 x*1528)) (((TuplePat (n m)) (call *+i* (Tuple (n m))))))))) (facta (lambda1 x*1529 (case x*1529 (((TuplePat (n a)) (case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))))))))) (map (lambda1 x*1530 (case x*1530 (((TuplePat (f (EmptyList))) (EmptyList)) ((TuplePat (f (call-pat *Cons* (TuplePat (x xs))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs))))))))))) (reva (lambda1 x*1531 (case x*1531 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call reva (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (foo (lambda1 x*1532 (case x*1532 ((-2.718 1.0))))) (append (lambda1 x*1533 (case x*1533 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))))))) (curAppend (lambda1 x*1534 (lambda1 x*1535 (case (Tuple (x*1534 x*1535)) (((TuplePat ((EmptyList) L)) L) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) L)) (call *Cons* (Tuple (x (call (call curAppend xs) L)))))))))) (myAppend (lambda1 x*1536 (case x*1536 (((TuplePat (L1 L2)) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2)))))))))))))) (insert (lambda1 x*1537 (case x*1537 (((TuplePat (x (EmptyList))) (call *Cons* (Tuple (x (EmptyList))))) ((TuplePat (x (call-pat *Cons* (TuplePat (y ys))))) (case (call *<=i* (Tuple (x y))) ((true (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys))))))) (false (call *Cons* (Tuple (y (call insert (Tuple (x ys)))))))))))))) (iSort (lambda1 x*1538 (case x*1538 (((EmptyList) (EmptyList)) ((call-pat *Cons* (TuplePat (x xs))) (call insert (Tuple (x (call iSort xs))))))))) (abs (lambda1 x*1539 (case x*1539 ((x (case (call *<d* (Tuple (x 0.0))) ((true (call *negd* x)) (false x)))))))) (average (lambda1 x*1540 (case x*1540 (((TuplePat (x y)) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0)))))))) (improveSqrtGuess (lambda1 x*1541 (case x*1541 (((TuplePat (a g)) (call average (Tuple (g (call *divd* (Tuple (a g))))))))))) (isCloseEnough (lambda1 x*1542 (case x*1542 (((TuplePat (x y)) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon)))))))) (sqrtIter (lambda1 x*1543 (case x*1543 (((TuplePat (a g)) (case (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) ((true g) (false (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g)))))))))))))) (sqrt (lambda1 x*1544 (case x*1544 ((x (call sqrtIter (Tuple (x 1.0)))))))) (quad (lambda1 x*1545 (case x*1545 (((TuplePat (a b c)) (case (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c)))))) ((e (case (call sqrt e) ((d (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a))))))))))))))))))) (maxRoot (lambda1 x*1546 (case x*1546 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1)))))))) (head (lambda1 x*1547 (case x*1547 ((L (case L (((EmptyList) (error Can't)) ((call-pat *Cons* (TuplePat (x xs))) x)))))))) (head2 (lambda1 x*1548 (case x*1548 (((EmptyList) (call Left Oops)) ((call-pat *Cons* (TuplePat (x xs))) (call Right x)))))) (pi 3.14) (arith (call *+i* (Tuple (2 (call **i* (Tuple (3 4))))))) (lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList))))))))))) (triple (Tuple ((Unit) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList))))))) (seven (call (call mkAdd 2) 5)) (rev2 (lambda1 x*1549 (case x*1549 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (test (case (case (case true ((true true) (false false))) ((true true) (false true))) ((true true) (false false)))) (test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4)))) (test3 (case (case true ((true true) (false (case false ((true true) (false false)))))) ((true true) (false false)))) (epsilon 1.0E-10)))



(defn split-decl [tree] 
	(let [
		tree (second tree)
		dataList (filter data-decls? tree)
		l1 (map #(rest %) dataList)
		l2 (types-from-data l1)
		l3 init-type-env
		typeList (filter type-decl? tree)
		l4 (map #(list (first (rest %)) (universalize-type (second (rest %)))) typeList)
		funList (filter fun-def? tree)
		reFunList (map #(list (first %) (arg1 %) (arg2 %) (reduce-forms (arg3 %))) funList)
		defList (filter def? tree)
		reDefList (map #(list (arg1 %) (reduce-forms (arg2 %))) defList)
		;funList2 (process-funs funList)
		; lambdaList (map #(second %) funList2)
		; caseList (map #(arg2 %) lambdaList)
		; caseList2 (map #(list (first %) (second %) (map 
		; 	(fn [pair] (list (first pair) (reduce-forms (second pair)))) (arg2 %))) caseList)
		; l5 (map #(list (first %1) %2) funList2 (map (fn [item] (make-lambda1 (second item) ())) caseList2))
		proReFunList (process-funs reFunList)
		l5 (reverse (into '() (concat proReFunList reDefList)))
		]
		(list l1 l2 l3 l4 l5)))

; (defn pp [p]
; 	(loop [list p]
; 		(when (not (empty? list))
; 			(println (first list))
; 			(recur (rest list)))))

; (pp (nth (split-decl tree) 0))

; Gamma for usage of testing
;(def var-type '((Left (forall (a b) (ArrowType a (NamedType Either (TupleType (a b)))))) (Right (forall (a b) (ArrowType b (NamedType Either (TupleType (a b)))))) (*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a))))  (arith (forall () (IntType)))))

; ; 3a
(defn gen-type-var [] (gensym "?t*"))

;3b (i)
(defn extendGamma [gamma var type] (conj (list var type) gamma))


;3b (ii)
(defn copy-type [hashmap type]
	(cond
		(forall-type? type) (make-forall-type (copy-type hashmap (second type)) (copy-type hashmap (arg2 type)))
		(constant-type? type) type
		(tuple-type? type)
			(let [res (atom ())]
				(loop [t (arg1 type)]
					(when (not (empty? t))
						(reset! res (conj @res (copy-type hashmap (first t))))
						(recur (rest t))))
				(make-tuple-type (reverse @res)))
		(arrow-type? type) 
			(let [
				prev (copy-type hashmap (arg1 type)) 
				poss (copy-type hashmap (arg2 type)) 
				]
				(make-arrow-type prev poss))
		(named-type? type) (make-named-type (arg1 type) (copy-type hashmap (arg2 type)))
		(list? type) (map #(copy-type hashmap %) type)
		(variable? type) 
			(if (= nil (hashmap type))
				type
				(hashmap type))

		:else (throw (Exception. "copy-type: Unexpected Input --- not a type"))))



; (def hp {'a 'x*123 'b 'x*dfd})
; (def atype '(*+d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))))
; (def atype2 '(*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a)))))
; (println (copy-type hp atype2))

;3b (iii)
(defn lookupGamma [gamma var]
  (cond 
    (nil? (first gamma)) ();(do (throw (Exception. "lookupGamma: The variable is undeclared." ) ))
    (= (first (first gamma)) var) (second (first gamma))
    :else (lookupGamma (rest gamma) var)))

; (def gamma '(1 2 3 4))


;3b (iv)
(defn applyGamma [gamma var]
	(do 
	  ;(println  "---->" var)
	  (let [        
	        type (lookupGamma gamma var)
	        ]
	    (do 
	      ;(println var "---->" type)
	      (cond 
	        ( = (first type) 'forall)  
	        (do
	          (let 
	              [hashmap (atom (hash-map)) ]
	            (do
	              (loop [L (arg1 type)]
	                (when (not (empty? L))
	                  (reset! hashmap (merge @hashmap (hash-map (first L) (gen-type-var))))
	                  ;; (println @hashmap)
	                  (recur (rest L))))
	              ;; (println @hashmap)
	              ;; (@hashmap var)
	              (copy-type @hashmap (arg2 type))
	              ;; (println @hashmap)
	              ;; (@hashmap var)
	            )))
	        :else (second type)
)))))

; 3c
(declare judge-type)

(defn applyUnifyTerm4 [make-base-type] 
     (fn [gamma exp type unifier hist] 
     	(do 
     	;(println gamma)
	     	;(println "u4 return " (unifyTerm4 make-base-type type unifier hist))
	      (unifyTerm4 make-base-type type unifier hist))))

(def judge-int (applyUnifyTerm4 (make-integer-type)))
(def judge-string (applyUnifyTerm4 (make-string-type)))
(def judge-boolean (applyUnifyTerm4 (make-boolean-type)))
(def judge-double (applyUnifyTerm4 (make-double-type)))
(def judge-unit (applyUnifyTerm4 (make-unit-type)))
(def judge-empty (applyUnifyTerm4 (make-empty-list)))


(defn judge-variable [gamma exp type unifier hist]
	(let [
		eType (applyGamma gamma exp)
		]
		(do 
			;(println "exp: " exp)
			;(println "etype----> come here" eType "-----" type)
			(if (nil? eType)
				(unifyTerm4 (gen-type-var) type unifier hist)
				(unifyTerm4 eType type unifier hist)
	  ))))



(defn judge-lambda [gamma exp type unifier hist]
	(do 
		; (println "labda type " type )
		(let [
				unifier2 (judge-type gamma (arg2 exp) (arg2 type) unifier hist)
			]
				unifier2
		)))
 
(defn judge-call [gamma exp type unifier hist]
	(do 
			;(println "unifier--- " unifier5)
			; (println "---------judge call exp-->" exp "-type: " type)
			(let [
				func (gen-type-var)
				input (gen-type-var)
				eType (make-arrow-type input type )
				unifier2 (judge-type gamma (arg1 exp) func unifier hist)
				unifier3 (judge-type gamma (arg2 exp) input unifier2 hist)
				unifier4 (unifyTerm4 eType func unifier3 hist)
				]
				unifier3
				)))
		       

		(defn judge-tuple [gamma exp type unifier hist]
			(do 
				; (println "judge-tuple type" type)
				; (println "judge-tuple exp" exp)
				; (println "unifier--- " unifier)
				(let [
					tupleList 
						(do  
							(map (fn [x] (gen-type-var)) (arg1 exp)))
					etype (make-tuple-type tupleList)
					unifier2 (unifyTerm4 etype type unifier hist)
					]
					unifier2
				)))
	 	
(defn judge-case [gamma exp type unifier hist]
	(if (empty? (arg2 exp))
		unifier
		(do 
			; (println "case exp----> " exp)
			; (println "case type----> " type)
			(let [
					top (first (arg2 exp))
					left (rest (arg2 exp))
					input (gen-type-var)
					condition (gen-type-var)
					output (gen-type-var)
					;arrType (make-arrow-type condition output)
					unifier2 (judge-type gamma (arg1 exp) input unifier hist)
					unifier3 (judge-type gamma (first top) condition unifier2 hist)
					unifier4 (judge-type gamma (second top) output unifier3 hist)
					;unifier5 (unifyTerm4 arrType type unifier4 hist)
					unifier5 (unifyTerm4 condition input unifier4 hist)
					
				]
				(do 
					(recur gamma (make-case (arg1 exp) left) type unifier hist)
				)))))

(defn judge-name [gamma exp type unifier hist]
	(do 
			; (println "case type---> " type)
			; (println "case exp----> " exp)
		(let [
				name (gen-type-var)
				inName (gen-type-var)
				unifier2 (unifyTerm4 exp name unifier hist)
				unifier3 (unifyTerm4 (arg2 exp) inName unifier2 hist)			
			]
			(do 
				unifier3
			))))


(declare judge-pat-type)

(defn judge-type [gamma exp type unifier hist]
	(do 
		; (println "exp in judgetype-->" exp)
		; (println "type--->" type)
		(cond 
			(integer? exp) (judge-int gamma exp type unifier hist);doen
			(string? exp) (judge-string gamma exp type unifier hist);done
			(boolean? exp) (judge-boolean gamma exp type unifier hist);done
			(double? exp) (judge-double gamma exp type unifier hist );done
			(unit? exp) (judge-unit gamma exp type unifier hist);done
			(variable? exp) (judge-variable gamma exp type unifier hist);done
			(tuple? exp) (judge-tuple gamma exp type unifier hist);done
			(lambda1? exp) (judge-lambda gamma exp type unifier hist);done
			(call? exp) (judge-call gamma exp type unifier hist);done
			(case? exp) (judge-case gamma exp type unifier hist);done
			(named-type? exp) (judge-name gamma exp type unifier hist)
			(error? exp) (judge-type gamma (arg1 exp) (make-string-type) unifier hist)
			(list? exp) (judge-pat-type gamma exp type unifier hist)
			:else (do (println exp) (throw (Exception. "judge-type: expression not exist"))))))

;3d

(defn judge-pat-constant [gamma exp type unifier hist]
	(do
		; (println "in judgepat constant exp" exp)
		; (println "in judgepat constant type" type)
		; (unifyTerm4 exp type unifier hist)
		unifier
	)
)


(defn judge-pat-tuple [gamma exp type unifier hist]
	(do 
		; (println "tuple pat type" type)
		(let [
			tupleList 
				(do 
					(map (fn [x] (gen-type-var)) (arg1 exp)))
			etype (make-tuple-type tupleList)
			unifier2 (unifyTerm4 etype type unifier hist)
			]
			unifier2)
	)
)

(defn judge-pat-call [gamma exp type unifier hist]
	(do 
		; (println "enter call" exp)
		; (println "enter call type" type)
		(let [
				arrType (applyGamma gamma (arg1 exp))
				unifier2 (judge-type gamma (arg2 exp) (arg1 arrType) unifier hist)
				unifier3 (unifyTerm4 type (arg2 arrType) unifier2 hist)
			]
			unifier3
	)
))


(defn judge-pat-type [gamma pat type unifier hist]
		(cond 
			(constant-pat? pat) (judge-pat-constant gamma pat type unifier hist)
			(variable? pat) (judge-type gamma pat type unifier hist)
			(tuple-pat? pat) (judge-pat-tuple gamma pat type unifier hist)
			(call-pat? pat) (judge-pat-call gamma pat type unifier hist)
			:else (throw (Exception. "judge-pat-type: expression not exist"))))


;3e

(defn check-program-5tuple [listFive]
  (let [
  			rfive (arg4 listFive)
        gamma (reverse (into '() (concat (arg1 listFive) (arg2 listFive) (arg3 listFive))))
        ]
    (do
      ;(println rfive)
      ;(loop [L (arg4 listFive)]
      (loop [L rfive]
        (when (not (empty? L))
        	(do
	          ;(println "-testing->" (first L))
	          (judge-type gamma (second (first L)) (applyGamma gamma (first (first L))) theta-identity ())
	          ;(judge-type gamma (second (first L)) (applyGamma gamma (first (first L))) () ())
	          )
          
          (recur (rest L))))
      (println "Type check successful. "))))

