(ns assign0.core)

;;  (CSCI-742) Compiler Construction
;;
;;  ASSIGNMENT 0
;;
;;
;;  Author: Zizhun Guo
;;
;;  zg2808@rit.edu




;; Utility
;; This function is referenced from the provided "Initial Clojure Example" that is
;; used for concatenate one list with another.

(defn append
  ;; i.e. input: (1 2 3) (4 5 6) output: (1 2 3 4 5 6) 
  [L1 L2]
  (if (empty? L1)
  L2
  (conj (append (rest L1) L2) (first L1))))




;;  Utility
;;  This function is referenced from the "Initial Clojure Example" example that is
;;  used for find the ele that is smaller than n from nums and form a new list.

(defn find-less [n nums]
   (cond (empty? nums) '()
         (< (first nums) n) (conj (find-less n (rest nums)) (first nums))
         :else (find-less n (rest nums))))




;;  Utility
;;  This function is referenced from the "Initial Clojure Example" example that is
;;  used for getting the length of the input list L.

(defn len [L]
        "This function literally returns the lenth of the input list"
        (if (empty? L) 0
	(+ 1 (len (rest L)))))




;;  This function is used for getting the middle index of the list L, and round it down.

(defn pivot [L]
        (if (empty? L) 0
	(int (/ (len L) 2))))     




;;  Question 1(a):
;;  Keyword: insert
;;  This function used for inserting an ele into a sorted list
;;  in which after it has been inserted, the list is in ascending order.
;;  Time complexity: O(n).

(defn insert [n L]
   (cond (empty? L) (append L (list n))
         (< n (first L)) (append (list n) L)
	 :else (append (list (first L)) (insert n (rest L)))))




;;  Question 1(b):
;;  Keyword: insertion-sort
;;  This function implements insertion-sort algorithm in recusive way.
;;  Time complexity: O(n^2).

(defn insertion-sort [L]
      "This is literally the insertion-sort:)"
      (if (empty? L) '()
          (insert (first L) (insertion-sort (rest L)))))





;;  Question 2:
;;  Keyword: index
;;  This function works for finding the index of ele x from list lst.

(defn index [x lst]
      "This is literally the index searching function:)"
       (try 
            (cond (empty? lst)
	          (throw (Exception."Exception Value not found in list." ))
                  (= x (first lst))
		  0
	          :else (+ 1 (index x (rest lst))))
       (catch Exception e (println (str " " (.toString e))))))




;;  Question 3:
;;  Keyword: filterList
;;  This function adopts a concept of "High-order Function" that takes a function as a input.
;;  This function works for filtering out the same type of ele that regulated by function f.

(defn filterList [f L]
        (cond (empty? L) '()
	      (f (first L)) (append (list (first L)) (filterList f (rest L)))
	      :else (filterList f (rest L))))




;;  Question 4:
;;  Keyword: mergeList
;;  This function works for merging two ascending sorted list into one single list sorted in ascending order.\
;;  Time complexity: O(n).

(defn mergeList [L1 L2]
        (cond (empty? L1) L2
	      (empty? L2) L1
	      :else (let [x (first L1) y (first L2)]
	                 (cond (< x y) (append (list x) (mergeList (rest L1) L2))
			       (> x y) (append (list y) (mergeList L1 (rest L2)))
			       :else (append (append (list x) (list y)) (mergeList (rest L1) (rest L2)))))))




;;  Question 5:
;;  Keyword: merge-sort
;;  This function implements classical merge-sort algorithm in recursive way.
;;  Time complexity: O(nLogn).

(defn merge-sort [L]
        (let [p  (pivot L)]
	     (mergeList ((fn getFront [n L]
                             (if (= 0 n) '()
                             (append (first L) (getFront (- n 1) (rest L))))) pivot L)
		        ((fn getBack [n L]
                             (if (empty? L) '()
                             (append (nth L n) (getBack (+ n 1) (rest L))))) pivot L))))






;;  The area down below show the answers for question 6 and question 7.
;;  Some function is developed for utility using purpose with both old&
;;  improved implementation code details.




;; The referenced default function to contructor to represent summation.

;(defn make-sum [e1 e2] (list '+ e1 e2))(




;;  Question 6 & 7
;;  Keyword: make-sum
;;  This function works for:
;;    1. Same role as commented "make-sum" function.
;;    2. Simplify the formation of representation.

(defn make-sum [e1 e2]
    (if (number? e1)
        (if (= e1 0)
	    (if (symbol? e2)
	        (list e2)
	         e2)
	    (if (number? e2)
	         (+ e1 e2)
		 (list '+ e1 e2)))
        (if (number? e2)
	    (if (= e2 0)
	         e1
		 (list '+ e1 e2))
            (list '+ e1 e2))))
	        
		 


;;  Utility
;;  Keyword: sum?
;;  This function constructs a funtion to justify whether the list is in summation form.

(defn sum? [a] (and (list? a) (= (first a) '+)))




;; The referenced default function to contructor to represent multiplication.

;(defn make-product [e1 e2] (list '* e1 e2))




;;  Question 6 & 7
;;  Keyword: make-product
;;  This function works for:
;;    1. Same role as commented "make-product" function.
;;    2. Simplify the formation of representation.

(defn make-product [e1 e2]
      (cond  (number? e1)
             (cond (= e1 0)
	           0
		   (= e1 1)
		   (cond (symbol? e2)
		      (list e2)
		      (number? e2)
		      e2
		      :else e2)
		   :else
		   (cond (symbol? e2)
		         (list '* e1 e2)
			 (number? e2)
			 (* e1 e2)
		         :else
			 (list '* e1 e2)))
             (symbol? e1)
	     (cond (= e2 0)
	           0
		   (= e2 1)
		   (list e2)
		   :else (list '* e1 e2))
	     :else
	     (list '* e1 e2)))




;;  Utility
;;  Keyword: product?
;;  This function constructs a funtion to justify whether the list is in multiplycation form.

(defn product? [a] (and (list? a) (= (first a) '*)))




;;  Get the second ele from the list.

(defn arg1 [sp] (nth sp 1))

;;  Get the third ele from the list.

(defn arg2 [sp] (nth sp 2))




;;  Question 7
;;  Keyword: expt
;;  This functions works for computing numerical powers.

(defn expt [e n]
       (try (if (= n 0)
              1
              (* e (expt e (- n 1))))
	(catch Exception e (println (str "Either n or e is not a number." (.toString e))))))




;;  Default function to represent the power formation.

;(defn make-power [e n] (list '** e n))




;;  Question 6 & 7
;;  Keyword: make-power
;;  This function works for:
;;    1. Same role as commented "make-power" function.
;;    2. Simplify the formation of representation.

(defn make-power [e n]
        (cond (= n 0)
	      1
	      (= n 1)
	         (cond (number? e)
		       e
                       (symbol? e)
		       (list e)
		       :else
		       e)
	      (number? n)
	          (cond (number? e)
		        (expt e n)
			:else
			(list '** e n))
	      :else
	      (list '** e n)))




;;  Question 6
;;  Keyword: power?
;;  This function constructs a funtion to justify whether the list is in power form.

(defn power? [a] (and (list? a) (= (first a) '**)))



;;  Construct "variable?" as same as the function of "symbol?".

(def variable? symbol?)




;;  Qustion 6
;;  Keyword: deriv
;;  This function handles powers of expression-form of power, summation and multiplication, and computes
;;  the derivative of such form.

(defn deriv [aExp var]
   (cond (number? aExp) 0
         (variable? aExp) 
          (if (= aExp var) 1 0)
         (sum? aExp) 
          (make-sum (deriv (arg1 aExp) var) (deriv (arg2 aExp) var))
         (product? aExp)
          (make-sum (make-product (arg1 aExp) (deriv (arg2 aExp) var))
                    (make-product (arg2 aExp) (deriv (arg1 aExp) var)))
         (power? aExp)
	  (make-product (arg2 aExp)
	            (make-product
		               (make-power (arg1 aExp) (- (arg2 aExp) 1)) (deriv (arg1 aExp) var)))
         :else (throw (Exception. "Unexpected Input -- not an arithmetic expression"))))