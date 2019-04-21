;; (ns assign4.textIO
;;    (:import java.io.FileNotFoundException)
;;    (:use [assign4.genIR])
;;    (:use clojure.pprint))

(ns assign4.test
   ;; (:import java.io.FileNotFoundException)
   (:use [assign4.genIR])
   (:use clojure.pprint))

; ;l1
(def l1 '((Float () ((constructor Box (DoubleType)))) (MyLi'st (a) ((constructor MT (UnitType)) (constructor Cons (TupleType (a (NamedType MyList a)))))) (Either (a b) ((constructor Left a) (constructor Right b)))))

; ;l2
(def l2 '((Box (forall () (ArrowType (DoubleType) (NamedType Float (UnitType))))) (MT (forall (a) (ArrowType (UnitType) (NamedType MyList a)))) (Cons (forall (a) (ArrowType (TupleType (a (NamedType MyList a))) (NamedType MyList a)))) (Left (forall (a b) (ArrowType a (NamedType Either (TupleType (a b)))))) (Right (forall (a b) (ArrowType b (NamedType Either (TupleType (a b))))))))

; ;l3
(def l3 '((*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a)))) (*==i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*!=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*==d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*!=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*+i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*-i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (**i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*divi* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*negi* (forall () (ArrowType (IntType) (IntType)))) (*+d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*-d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (**d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*divd* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*negd* (forall () (ArrowType (DoubleType) (DoubleType)))) (print (forall () (ArrowType (StringType) (UnitType))))))

; ;l4
(def l4 '((pi (forall () (DoubleType))) (arith (forall () (IntType))) (lst1 (forall () (NamedType *List* (IntType)))) (triple (forall (a) (TupleType ((UnitType) (NamedType *List* a) (NamedType *List* (IntType)))))) (fact (forall () (ArrowType (IntType) (IntType)))) (mkAdd (forall () (ArrowType (IntType) (ArrowType (IntType) (IntType))))) (seven (forall () (IntType))) (facta (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (map (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType *List* a))) (NamedType *List* b)))) (reva (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (rev2 (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (foo (forall () (ArrowType (DoubleType) (DoubleType)))) (bar (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType MyList a))) (NamedType MyList b)))) (baz (forall (a b) (NamedType Either (TupleType (a b))))) (test (forall () (BoolType))) (test2 (forall () (IntType))) (test3 (forall () (BoolType))) (append (forall (a) (ArrowType (TupleType ((NamedType *List* a) (NamedType *List* a))) (NamedType *List* a)))) (curAppend (forall (a) (ArrowType (NamedType *List* a) (ArrowType (NamedType *List* a) (NamedType *List* a))))) (myAppend (forall (a) (ArrowType (TupleType ((NamedType MyList a) (NamedType MyList a))) (NamedType MyList a)))) (insert (forall () (ArrowType (TupleType ((IntType) (NamedType *List* (IntType)))) (NamedType *List* (IntType))))) (iSort (forall () (ArrowType (NamedType *List* (IntType)) (NamedType *List* (IntType))))) (abs (forall () (ArrowType (DoubleType) (DoubleType)))) (average (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (improveSqrtGuess (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (epsilon (forall () (DoubleType))) (isCloseEnough (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (sqrtIter (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (sqrt (forall () (ArrowType (DoubleType) (DoubleType)))) (quad (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (TupleType ((DoubleType) (DoubleType)))))) (maxRoot (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (DoubleType)))) (head (forall (a) (ArrowType (NamedType *List* a) a))) (head2 (forall (a) (ArrowType (NamedType *List* a) (NamedType Either (TupleType ((StringType) a))))))))
; ;l5

(def l5 '(
(fact (lambda1 x*1526 (case x*1526 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))) 
(mkAdd (lambda1 x*1527 (lambda1 x*1528 (case (Tuple (x*1527 x*1528)) (((TuplePat (n m)) (call *+i* (Tuple (n m))))))))) (facta (lambda1 x*1529 (case x*1529 (((TuplePat (n a)) 
(case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))))
))))) (map (lambda1 x*1530 (case x*1530 (((TuplePat (f (EmptyList))) (EmptyList)) ((TuplePat (f (call-pat *Cons* (TuplePat (x xs))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs))))))))))) (reva (lambda1 x*1531 (case x*1531 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call reva (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (foo (lambda1 x*1532 (case x*1532 ((-2.718 1.0))))) (append (lambda1 x*1533 (case x*1533 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))))))) (curAppend (lambda1 x*1534 (lambda1 x*1535 (case (Tuple (x*1534 x*1535)) (((TuplePat ((EmptyList) L)) L) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) L)) (call *Cons* (Tuple (x (call (call curAppend xs) L)))))))))) (myAppend (lambda1 x*1536 (case x*1536 (((TuplePat (L1 L2)) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2)))))))))))))) (insert (lambda1 x*1537 (case x*1537 (((TuplePat (x (EmptyList))) (call *Cons* (Tuple (x (EmptyList))))) ((TuplePat (x (call-pat *Cons* (TuplePat (y ys))))) (case (call *<=i* (Tuple (x y))) ((true (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys))))))) (false (call *Cons* (Tuple (y (call insert (Tuple (x ys)))))))))))))) (iSort (lambda1 x*1538 (case x*1538 (((EmptyList) (EmptyList)) ((call-pat *Cons* (TuplePat (x xs))) (call insert (Tuple (x (call iSort xs))))))))) (abs (lambda1 x*1539 (case x*1539 ((x (case (call *<d* (Tuple (x 0.0))) ((true (call *negd* x)) (false x)))))))) (average (lambda1 x*1540 (case x*1540 (((TuplePat (x y)) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0)))))))) (improveSqrtGuess (lambda1 x*1541 (case x*1541 (((TuplePat (a g)) (call average (Tuple (g (call *divd* (Tuple (a g))))))))))) (isCloseEnough (lambda1 x*1542 (case x*1542 (((TuplePat (x y)) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon)))))))) (sqrtIter (lambda1 x*1543 (case x*1543 (((TuplePat (a g)) (case (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) ((true g) (false (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g)))))))))))))) (sqrt (lambda1 x*1544 (case x*1544 ((x (call sqrtIter (Tuple (x 1.0)))))))) (quad (lambda1 x*1545 (case x*1545 (((TuplePat (a b c)) (case (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c)))))) ((e (case (call sqrt e) ((d (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a))))))))))))))))))) (maxRoot (lambda1 x*1546 (case x*1546 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1)))))))) (head (lambda1 x*1547 (case x*1547 ((L (case L (((EmptyList) (error Can't)) ((call-pat *Cons* (TuplePat (x xs))) x)))))))) (head2 (lambda1 x*1548 (case x*1548 (((EmptyList) (call Left Oops)) ((call-pat *Cons* (TuplePat (x xs))) (call Right x)))))) (pi 3.14) (arith (call *+i* (Tuple (2 (call **i* (Tuple (3 4))))))) (lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList))))))))))) (triple (Tuple ((Unit) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList))))))) (seven (call (call mkAdd 2) 5)) (rev2 (lambda1 x*1549 (case x*1549 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (test (case (case (case true ((true true) (false false))) ((true true) (false true))) ((true true) (false false)))) (test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4)))) (test3 (case (case true ((true true) (false (case false ((true true) (false false)))))) ((true true) (false false)))) (epsilon 1.0E-10)))


(def abc "hello world")
(println abc)

(def caseexp1 
	'(case x*1526 
									(
									(0 1)
									(n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1))))))))
									)))
; (let-thunk f*1640 
; 	(let-thunk f*1641 
; 		(error No patterns matched.) 
; 		(let1 n x*1526 (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))
; 	(if (call *isEqual* (Tuple (x*1526 0)))
; 	 	1 
; 	 	(tail-call-thunk f*1640)))



(def caseexp2 '(lambda1 x*1533 (case x*1533 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2))))))))))))))


; (lambda1 x*1533 
; 	(let-thunk f*1586 
; 		(error No patterns matched.)
; 		(if (call *isTupleLike* x*1533) 
; 			(if (call *isEqual* (Tuple ((call *getTupleName* x*1533) *anonymous*))) 
; 				(let1 x*1596 (call *getTuple* (Tuple (x*1533 0))) 
; 					(let1 L1 x*1596 
; 						(let1 x*1595 (call *getTuple* (Tuple (x*1533 1))) 
; 							(let1 L2 x*1595 
; 								(let-thunk f*1589 
; 									(let-thunk f*1590 
; 										(error No patterns matched.) 
; 										(if (call *isTupleLike* L1) 
; 											(if (call *isEqual* (Tuple ((call *getTupleName* L1) *Cons*))) 
; 												(let1 x*1594 (call *getTuple* (Tuple (L1 0))) 
; 													(let1 x x*1594 
; 														(let1 x*1593 (call *getTuple* (Tuple (L1 1))) 
; 															(let1 xs x*1593 (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) 
; 											(tail-call-thunk f*1590)) 
; 										(tail-call-thunk f*1590))) 
; 								(if (call *isEmptyList* L1) 
; 									L2 
; 									(tail-call-thunk f*1589)))))))
; 				(tail-call-thunk f*1586)) 
; 			(tail-call-thunk f*1586))))

; (lambda1 x*1533 
; 	(let-thunk f*2158 
; 		(error No patterns matched.) 
; 		(if (call *isTupleLike* x*1533) 
; 			(if (call *isEqual* (Tuple ((call *getTupleName* x*1533) *anonymous*))) 
; 				(let1 x*2164 (call *getTuple* (Tuple (x*1533 0))) 
; 					(let1 L1 x*2164 
; 						(let1 x*2163 (call *getTuple* (Tuple (x*1533 1))) 
; 							(let1 L2 x*2163 
; 								(let-thunk f*2159 
; 									(let-thunk f*2160 
; 										(error No patterns matched.) 
; 										(if (call *isTupleLike* L1) 
; 											(if (call *isEqual* (Tuple ((call *getTupleName* L1) *Cons*))) 
; 												(let1 x*2162 (call *getTuple* (Tuple (L1 0))) 
; 													(let1 x x*2162 
; 														(let1 x*2161 (call *getTuple* (Tuple (L1 1))) 
; 															(let1 xs x*2161 (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) 
; 													(tail-call-thunk f*2160)) 
; 												(tail-call-thunk f*2160))) 
; 									(if (call *isEqual* (Tuple (L1 (EmptyList))))
; 										L2 
; 										(tail-call-thunk f*2159))))))) 
; 					(tail-call-thunk f*2158)) 
; 			(tail-call-thunk f*2158))) ())



(def letcase '(case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))
)
; (let-thunk 
; 	f*1543 
; 	(let-thunk 
; 		f*1544 
; 		(error No patterns matched.)
; 		(if (call *isTupleLike* L1) 
; 			(if (call *isEqual* (Tuple ((call *getTupleName* L1) *Cons*))) 
; 				(let1 x*1548 (call *getTuple* (Tuple (L1 0))) 
; 					(let1 x x*1548 
; 						(let1 x*1547 (call *getTuple* (Tuple (L1 1))) 
; 							(let1 xs x*1547 
; 								(call *Cons* (Tuple (x (call append (Tuple (xs L2))))))))))
; 				(tail-call-thunk f*1544)) 
; 			(tail-call-thunk f*1544)))
; 	(if (call *isEmptyList* L1)
; 		L2 
; 		(tail-call-thunk f*1543)))

; (let-thunk f*1947 
; 	(let-thunk f*1948 
; 		(error No patterns matched.) 
; 		(if (call *isTupleLike* L1) 
; 			(if (call *isEqual* (Tuple ((call *getTupleName* L1) *Cons*))) 
; 				(let1 x*1950 (call *getTuple* (Tuple (L1 1))) 
; 					(let1 x x*1950 
; 						(let1 x*1949 (call *getTuple* (Tuple (L1 2))) 
; 							(let1 xs x*1949 
; 								(call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))) 
; 						(tail-call-thunk f*1948)) 
; 				(tail-call-thunk f*1948))) 
; 	(if (call *isEqual* (Tuple (L1 (EmptyList)))) 
; 	L2 
; 	(tail-call-thunk f*1947)))


(def exp1 
; '(maxRoot 
'(lambda1 x*1546 
	(case x*1546 
		(
			(
				(TuplePat (a b c)) 
				(case 
					(call quad (Tuple (a b c))) 
					(((TuplePat (r1 r2)) r1)))
				)))))


(def exp2 '(fact (lambda1 x*1526 (case x*1526 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))))


; (def pat1 '(TuplePat ((TuplePat (x xs)) (TuplePat (x xs)))))
; (println (match-pattern pat1 false 'lst 'label))
; (def pat2 '(call-pat *Cons* (TuplePat (x xs aa))))
; ; (println (match-pattern pat2 true 'lsss 'laaa))

; (if (call *isTupleLike* lst) 
; 	(if (call *isEqual* (Tuple ((call *getTupleName* lst) *anonymous*))) 
; 		(let1 x*1880 (call *getTuple* (Tuple (lst 0))) 
; 			(let1 x x*1880 
; 				(let1 x*1875 (call *getTuple* (Tuple (lst 1))) 
; 					(if (call *isTupleLike* x*1875) 
; 						(if (call *isEqual* (Tuple ((call *getTupleName* x*1875) *anonymous*))) 
; 							(let1 x*1879 (call *getTuple* (Tuple (x*1875 0))) 
; 								(let1 x x*1879 
; 									(let1 x*1878 (call *getTuple* (Tuple (x*1875 1))) 
; 										(let1 xs x*1878 false)))) 
; 							(tail-call-thunk label)) 
; 						(tail-call-thunk label))))) 
; 		(tail-call-thunk label)) 
; 	(tail-call-thunk label))

; (if (call *isTupleLike* lst) 
; 	(if (call *isEqual* (Tuple ((call *getTupleName* lst) *anonymous*))) 
; 		(let1 x*2865 (call *getTuple* (Tuple (lst 0))) 
; 			(let1 x x*2865 
; 				(let1 x*2862 (call *getTuple* (Tuple (lst 1))) 
; 					(if (call *isTupleLike* x*2862) 
; 						(if (call *isEqual* (Tuple ((call *getTupleName* x*2862) *anonymous*))) 
; 							(let1 x*2864 (call *getTuple* (Tuple (x*2862 0))) 
; 								(let1 x x*2864 
; 									(let1 x*2863 (call *getTuple* (Tuple (x*2862 1))) 
; 										(let1 xs x*2863 false)))) 
; 							(tail-call-thunk label)) 
; 						(tail-call-thunk label))))) 
; 		(tail-call-thunk label)) 
; 	(tail-call-thunk label))






; (if (call *isTupleLike* lst) 
; 	(if (call *isEqual* (Tuple ((call *getTupleName* lst) *anonymous*))) 
; 		(let1 x*1900 (call *getTuple* (Tuple (lst 0)))

; 			(if (call *isTupleLike* x*1900) 
; 				(if (call *isEqual* (Tuple ((call *getTupleName* x*1900) *anonymous*))) 
; 					(let1 x*1904 (call *getTuple* (Tuple (x*1900 0))) 
; 						(let1 x x*1904 
; 							(let1 x*1903 (call *getTuple* (Tuple (x*1900 1))) 
; 								(let1 xs x*1903 
; 									(let1 x*1899 (call *getTuple* (Tuple (lst 1))) 
; 										(let1 xs x*1899 false)))))) 
; 						(tail-call-thunk label)) 
; 					(tail-call-thunk label))
; 		) 
; 			(tail-call-thunk label)) 
; 		(tail-call-thunk label))

; (if (call *isTupleLike* lst) 
; 	(if (call *isEqual* (Tuple ((call *getTupleName* lst) *anonymous*))) 
; 		(let1 x*1929 (call *getTuple* (Tuple (lst 0))) 
; 			(if (call *isTupleLike* x*1929) 
; 				(if (call *isEqual* (Tuple ((call *getTupleName* x*1929) *anonymous*))) 
; 					(let1 x*1933 (call *getTuple* (Tuple (x*1929 0))) 
; 						(let1 x x*1933 
; 							(let1 x*1932 (call *getTuple* (Tuple (x*1929 1))) 
; 								(let1 xs x*1932 
; 									(let1 x*1924 (call *getTuple* (Tuple (lst 1))) 
; 										(if (call *isTupleLike* x*1924) 
; 											(if (call *isEqual* (Tuple ((call *getTupleName* x*1924) *anonymous*))) 
; 												(let1 x*1928 (call *getTuple* (Tuple (x*1924 0))) 
; 													(let1 x x*1928 
; 														(let1 x*1927 (call *getTuple* (Tuple (x*1924 1))) 
; 															(let1 xs x*1927 false)))) 
; 												(tail-call-thunk label)) (tail-call-thunk label))))))) (tail-call-thunk label)) (tail-call-thunk label))) (tail-call-thunk label)) (tail-call-thunk label))



(lambda1 y*1909 
	(let-thunk f*1896 (error No patterns matched.) 
		(if (call *isTupleLike* y*1909) 
			(if (call *isEqual* (Tuple ((call *getTupleName* y*1909) *anonymous*))) 
				(let1 y*1910 (call *getTuple* (Tuple (y*1909 0))) 
					(let1 y*1911 y*1910 
						(let1 y*1912 (call *getTuple* (Tuple (y*1909 1))) 
							(let1 y*1913 y*1912 
								(let1 y*1914 (call *getTuple* (Tuple (y*1909 2))) 
									(let1 y*1915 y*1914 
										(let1 y*1916 (call quad (Tuple (y*1911 y*1913 y*1915))) 
											(let-thunk f*1901 (error No patterns matched.) 
												(if (call *isTupleLike* y*1916) 
													(if (call *isEqual* (Tuple ((call *getTupleName* y*1916) *anonymous*))) 
														(let1 y*1917 (call *getTuple* (Tuple (y*1916 0))) 
															(let1 y*1918 y*1917 
																(let1 y*1919 (call *getTuple* (Tuple (y*1916 1))) 
																	(let1 y*1920 y*1919 y*1918)))) 
																(tail-call-thunk f*1901)) 
														(tail-call-thunk f*1901)))))))))) 
					(tail-call-thunk f*1896)) 
				(tail-call-thunk f*1896))))

(lambda1 y*3523 
	(let-thunk f*3515 
		(error No patterns matched.) 
			(if (call *isTupleLike* y*3523) 
				(if (call *isEqual* (Tuple ((call *getTupleName* y*3523) *anonymous*))) 
					(let1 y*3524 (call *getTuple* (Tuple (y*3523 0))) 
						(let1 y*3525 y*3524 
							(let1 y*3526 (call *getTuple* (Tuple (y*3523 1))) 
								(let1 y*3527 y*3526 
									(let1 y*3528 (call *getTuple* (Tuple (y*3523 2))) 
										(let1 y*3529 y*3528 
											(let1 y*3530 (call y*3531 (Tuple (y*3525 y*3527 y*3529))) 
												(let-thunk f*3517 (error No patterns matched.) 
													(if (call *isTupleLike* y*3530) 
														(if (call *isEqual* (Tuple ((call *getTupleName* y*3530) *anonymous*))) 
															(let1 y*3532 (call *getTuple* (Tuple (y*3530 0))) 
																(let1 y*3533 y*3532 
																	(let1 y*3534 (call *getTuple* (Tuple (y*3530 1))) 
																		(let1 y*3535 y*3534 y*3533)))) (tail-call-thunk f*3517)) (tail-call-thunk f*3517)))))))))) (tail-call-thunk f*3515)) (tail-call-thunk f*3515))))


; (lambda1 y*2374 
; 	(let-thunk f*2367 
; 		(error No patterns matched.) 
; 			(if (call *isTupleLike* y*2374) 
; 				(if (call *isEqual* (Tuple ((call *getTupleName* y*2374) *anonymous*))) 
; 					(let1 y*2375 (call *getTuple* (Tuple (y*2374 0))) 
; 						(let1 a y*2375 
; 							(let1 y*2376 (call *getTuple* (Tuple (y*2374 1))) 	
; 								(let1 b y*2376 
; 									(let1 y*2377 (call *getTuple* (Tuple (y*2374 2))) 
; 										(let1 c y*2377 
; 											(let-thunk f*2368 (error No patterns matched.) 
; 												(if (call *isTupleLike* (call quad (Tuple (a b c)))) 
; 													(if (call *isEqual* (Tuple ((call *getTupleName* (call quad (Tuple (a b c)))) *anonymous*))) 
; 														(let1 y*2378 (call *getTuple* (Tuple ((call quad (Tuple (a b c))) 0))) 
; 															(let1 r1 y*2378 
; 																(let1 y*2379 (call *getTuple* (Tuple ((call quad (Tuple (a b c))) 1))) 
; 																	(let1 r2 y*2379 r1)))) 
; 														(tail-call-thunk f*2368)) 
; 													(tail-call-thunk f*2368))))))))) 
; 					(tail-call-thunk f*2367)) 
; 				(tail-call-thunk f*2367))))

(println (rename-locals (transform-exp exp1)))

;; (println transform-exp caseexp1)
;; (print;ln (transform-exp caseexp2))
;; (println transform-)

; (let-thunk f*1889 
; 	(let-thunk f*1890 
; 		(error No patterns matched.) 
; 		(let1 n x*1526 (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1))))))))) 
; 	(if (call *isEqual* (Tuple (x*1526 0)))
; 	 1 (tail-call-thunk f*1889)))

; (let-thunk f*1640 
; 	(let-thunk f*1641 
; 		(error No patterns matched.) 
; 		(let1 n x*1526 (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))
; 	(if (call *isEqual* (Tuple (x*1526 0)))
; 	 	1 
; 	 	(tail-call-thunk f*1640)))
; (lambda1 x*1546 (case x*1546 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1)))))))