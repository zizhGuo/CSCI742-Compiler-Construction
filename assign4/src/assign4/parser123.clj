(ns assign4.parser
	(:use [assign4.genPar :only (make-parser)])
	;(:use [csci742.genPar :only (make-parser)])
)




;;; hw4 extra
; 1a
(defn make-let1 [ids exp1 exp2] (list 'let1 ids exp1 exp2))
(defn make-let-thunk [ids exp1 exp2] (list 'let-thunk ids exp1 exp2))
(defn make-tail-call-thunk [ids] (list 'tail-call-thunk ids))
(defn make-global-call [ids exp] (list 'global-call ids exp))
(defn make-closure [ids idenList] (list 'closure ids idenList))
; 1b
(defn let1? [i] (= (first i) 'let1))
(defn let-thunk? [i] (= (first i) 'let-thunk))
(defn tail-call-thunk? [i] (= (first i) 'tail-call-thunk))
(defn global-call? [i] (= (first i) 'global-call))
(defn closure? [i] (= (first i) 'closure))



;;; hw4 extra end 
