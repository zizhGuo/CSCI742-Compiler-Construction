(ns assign3.textIO
   (:import java.io.FileNotFoundException)
   (:use [assign3.checker])
   (:use clojure.pprint))

(def five '(
((Float () ((constructor Box (DoubleType)))) (MyList (a) ((constructor MT (UnitType)) (constructor Cons (TupleType (a (NamedType MyList a)))))) (Either (a b) ((constructor Left a) (constructor Right b))))
((Box (forall () (ArrowType (DoubleType) (NamedType Float (UnitType))))) (MT (forall (a) (ArrowType (UnitType) (NamedType MyList a)))) (Cons (forall (a) (ArrowType (TupleType (a (NamedType MyList a))) (NamedType MyList a)))) (Left (forall (a b) (ArrowType a (NamedType Either (TupleType (a b)))))) (Right (forall (a b) (ArrowType b (NamedType Either (TupleType (a b)))))))
((*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a)))) (*==i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*!=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*==d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*!=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*+i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*-i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (**i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*divi* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*negi* (forall () (ArrowType (IntType) (IntType)))) (*+d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*-d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (**d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*divd* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*negd* (forall () (ArrowType (DoubleType) (DoubleType)))) (print (forall () (ArrowType (StringType) (UnitType))))) ((pi (forall () (DoubleType))) (arith (forall () (IntType))) (lst1 (forall () (NamedType *List* (IntType)))) (triple (forall (a) (TupleType ((UnitType) (NamedType *List* a) (NamedType *List* (IntType)))))) (fact (forall () (ArrowType (IntType) (IntType)))) (mkAdd (forall () (ArrowType (IntType) (ArrowType (IntType) (IntType))))) (seven (forall () (IntType))) (facta (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (map (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType *List* a))) (NamedType *List* b)))) (reva (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (rev2 (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (foo (forall () (ArrowType (DoubleType) (DoubleType)))) (bar (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType MyList a))) (NamedType MyList b)))) (baz (forall (a b) (NamedType Either (TupleType (a b))))) (test (forall () (BoolType))) (test2 (forall () (IntType))) (test3 (forall () (BoolType))) (append (forall (a) (ArrowType (TupleType ((NamedType *List* a) (NamedType *List* a))) (NamedType *List* a)))) (curAppend (forall (a) (ArrowType (NamedType *List* a) (ArrowType (NamedType *List* a) (NamedType *List* a))))) (myAppend (forall (a) (ArrowType (TupleType ((NamedType MyList a) (NamedType MyList a))) (NamedType MyList a)))) (insert (forall () (ArrowType (TupleType ((IntType) (NamedType *List* (IntType)))) (NamedType *List* (IntType))))) (iSort (forall () (ArrowType (NamedType *List* (IntType)) (NamedType *List* (IntType))))) (abs (forall () (ArrowType (DoubleType) (DoubleType)))) (average (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (improveSqrtGuess (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (epsilon (forall () (DoubleType))) (isCloseEnough (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (sqrtIter (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (sqrt (forall () (ArrowType (DoubleType) (DoubleType)))) (quad (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (TupleType ((DoubleType) (DoubleType)))))) (maxRoot (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (DoubleType)))) (head (forall (a) (ArrowType (NamedType *List* a) a))) (head2 (forall (a) (ArrowType (NamedType *List* a) (NamedType Either (TupleType ((StringType) a))))))) 


(
(fact (lambda1 x*1434 (case x*1434 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1))))))))))))
(mkAdd (lambda1 x*1435 (lambda1 x*1436 (case (Tuple (x*1435 x*1436)) (((TuplePat (n m)) (call *+i* (Tuple (n m)))))))))
(facta (lambda1 x*1437 (case x*1437 (((TuplePat (n a)) (case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a))))))))))))))
(map (lambda1 x*1438 (case x*1438 (((TuplePat (f (EmptyList))) (EmptyList)) ((TuplePat (f (call-pat *Cons* (TuplePat (x xs))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs)))))))))))
(reva (lambda1 x*1439 (case x*1439 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call reva (Tuple (xs (call *Cons* (Tuple (x a)))))))))))
(foo (lambda1 x*1440 (case x*1440 ((-2.718 1.0)))))
;;;; (foo,, (forall () (ArrowType (DoubleType) (DoubleType))))
(append (lambda1 x*1441 (case x*1441 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2))))))))))))))
(curAppend (lambda1 x*1442 (lambda1 x*1443 (case (Tuple (x*1442 x*1443)) (((TuplePat ((EmptyList) L)) L) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) L)) (call *Cons* (Tuple (x (call (call curAppend xs) L))))))))))
(myAppend (lambda1 x*1444 (case x*1444 (((TuplePat (L1 L2)) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2))))))))))))))
(insert (lambda1 x*1445 (case x*1445 (((TuplePat (x (EmptyList))) (call *Cons* (Tuple (x (EmptyList))))) ((TuplePat (x (call-pat *Cons* (TuplePat (y ys))))) (case (call *<=i* (Tuple (x y))) ((true (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys))))))) (false (call *Cons* (Tuple (y (call insert (Tuple (x ys))))))))))))))
(iSort (lambda1 x*1446 (case x*1446 (((EmptyList) (EmptyList)) ((call-pat *Cons* (TuplePat (x xs))) (call insert (Tuple (x (call iSort xs)))))))))
(abs (lambda1 x*1447 (case x*1447 ((x (case (call *<d* (Tuple (x 0.0))) ((true (call *negd* x)) (false x))))))))
(average (lambda1 x*1448 (case x*1448 (((TuplePat (x y)) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0))))))))
(improveSqrtGuess (lambda1 x*1449 (case x*1449 (((TuplePat (a g)) (call average (Tuple (g (call *divd* (Tuple (a g)))))))))))
(isCloseEnough (lambda1 x*1450 (case x*1450 (((TuplePat (x y)) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon))))))))
(sqrtIter (lambda1 x*1451 (case x*1451 (((TuplePat (a g)) (case (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) ((true g) (false (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g))))))))))))))
(sqrt (lambda1 x*1452 (case x*1452 ((x (call sqrtIter (Tuple (x 1.0))))))))
(quad (lambda1 x*1453 (case x*1453 (((TuplePat (a b c)) (case (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c)))))) ((e (case (call sqrt e) ((d (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))))))))))))))))
(maxRoot (lambda1 x*1454 (case x*1454 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1))))))))
(head (lambda1 x*1455 (case x*1455 ((L (case L (((EmptyList) (error "Can't")) ((call-pat *Cons* (TuplePat (x xs))) x))))))))
;(head2 (lambda1 x*1456 (case x*1456 (((EmptyList) (call Left Oops)) ((call-pat *Cons* (TuplePat (x xs))) (call Right x))))))
(pi 3.14)
(seven (call (call mkAdd 2) 5))
(foo (lambda1 x*1532 (case x*1532 ((-2.718 1.0) (0.1 0.2)))))
(arith (call *+i* (Tuple (2 (call **i* (Tuple (3 2)))))))
(lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList)))))))))))
;(triple (Tuple ((Integer) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList)))))))
(seven (call (call mkAdd 2) 5))
(rev2 (lambda1 x*1457 (case x*1457 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a)))))))))))
(test (case (case (case true ((true true) (false false))) ((true true) (false true))) ((true true) (false false))))
(test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4))))
(test3 (case (case true ((true true) (false (case false ((true true) (false false)))))) ((true true) (false false))))
(epsilon 1.0E-10)
)

))


(check-program-5tuple five)




















