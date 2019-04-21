(ns assign4.genLex)
; Auther: Tao Yang, Zizhun Guo
; Part A of HW 1
; Testing code is keeped for unit test.
; The code is in the questions' order

(defn arg1 [sp] (nth sp 1))

(defn arg2 [sp] (nth sp 2))

(defn arg3 [sp] (nth sp 3))

(defn arg4 [sp] (nth sp 4))

; 1.a

(defn epsilon? [e] (and (symbol? e) (= e '*epsilon*)) )

(defn all? [e] (and (symbol? e) (= e '*all*)) )

(defn all-except? [e] (and (list? e) (= (first e) 'all-except)))

(defn range? [e] (and (list? e) (= (first e) 'range)))

(defn or? [e]  (and (list? e) (= (first e) 'or)))

(defn seqn? [e]  (and (list? e) (= (first e) 'seqn)))

(defn optional? [e] (and (list? e) (= (first e) '?)))

(defn star? [e]  (and (list? e) (= (first e) '*)))

(defn plus? [e]  (and (list? e) (= (first e) '+)))

(defn name? [e]
	(and (symbol? e) (not (all? e)) (not (epsilon? e))))

; 1.b

(defn make-all-except [l] (list 'all-except l))

(defn make-range [s e] (list 'range s e))

(defn make-or [a b] (list 'or a b))

(defn make-seqn [a b] (list 'seqn a b))

(defn make-star [a] (list '* a))

(defn make-optional [a] (list '? a))

(defn make-plus [a] (list '+ a))

; 2.a

(defn search [x rbl]
    (if (empty? rbl) 
        (throw (Exception. ("search:result not found.")))
        (if (= x (first (first rbl)))
            (second (first rbl))
            (search x (rest rbl)))))

(defn extendedFromNames [nre rbl]
    (cond (epsilon? nre) nre
          (char? nre) nre
          (all? nre) nre
		  (all-except? nre) nre
		  (range? nre) nre
		  (string? nre) nre
		  (symbol? nre) (search nre rbl)
		  (or? nre) (make-or (extendedFromNames (arg1 nre) rbl) (extendedFromNames (arg2 nre) rbl))
		  (seqn? nre) (make-seqn (extendedFromNames (arg1 nre) rbl) (extendedFromNames (arg2 nre) rbl))
		  (star? nre) (make-star (extendedFromNames (arg1 nre) rbl))
		  (optional? nre) (make-optional (extendedFromNames (arg1 nre) rbl))
		  (plus? nre) (make-plus (extendedFromNames (arg1 nre) rbl))
		  :else (throw (Exception. ((str nre) " nre not in the regular binding list.")))))

; test 2.a
; (def l '((bcd \b) (abc \a) (dd \d) (ee (or (or \a \b) \c))))
; (println (extendedFromNames '(or (or bcd abc) ee) l))

; test for new search
; (def l '((abc 0) (bcd abc) (ddd 3) (ccc abc)))
; (println (search 'ddd l))

; test for extendedFromNames
; (def l '((abc 0) (bcd abc) (ddd 3) (ccc abc)))
; (println (extendedFromNames 'ddd l))




;(defn searchRBL [nm regularBL]
;	(loop [i regularBL]
;		(if (or (= nm (first (first i))) (= nil (first i)))
;			(arg1 (first i))
;			(recur (next i)))))

; test for search
; (def l '((abc 0) (bcd abc) (ddd 3) (ccc abc)))
; (println (searchRBL 'ddd l))

; (defn extendedFromNames [namedRE regularBL] 
; 	(cond
; 		(or)))

; 2.b

(defn searchHelper [L]
  (cond (empty? L) '()
        (empty? (rest L)) L
        :else (let [res (searchHelper (rest L))
                    curr (first L)]
                (conj res (list (first curr) (extendedFromNames (second curr) res))))))

(defn buildEnv [L] (reverse (searchHelper (reverse L ))))

;(def l '((bcd \b) (abc \a) (dd \d) (ee (or (or abc bcd) dd)) (aa ee)))
; (def l '((a *all*) (b a)))
; (println (buildEnv l))

;; Test case
;; (def l2 '( (a *all*) (b a)))
;; output: ((a *all*) (b *all))

; 2.c

(defn exRange [s e a]
	(if (>= s e)
		a
		(recur (+ s 1) e (make-or a (char (+ s 1))))))

(defn inList [chr l]
	;(println chr l)
	(cond
		(empty? l) false
		(= (first l) chr) true
		:else (recur chr (rest l))))

(defn exAllExcept [s e a l]
	(if (>= s e)
		a
		(if (inList (char s) l) ;if char s is in except list return a without consider char s
			(recur (+ s 1) e a l)
			(if (epsilon? a) ; if a is still empty return char s, else a or char s
				(recur (+ s 1) e (char s) l)
				(recur (+ s 1) e (make-or a (char s)) l)))))


(defn exString [st seqn]
	(if (empty? st)
		seqn
		(recur (subs st 1) (make-seqn seqn (first st)))))


(defn regularFromExtended [extendedRE]
	(cond
		(all? extendedRE) (exRange 0 126 (char 0))
		(all-except? extendedRE) (exAllExcept 0 126 '*epsilon* (arg1 extendedRE))
		(range? extendedRE) (exRange (int (arg1 extendedRE)) (int (arg2 extendedRE)) (arg1 extendedRE))
		(seqn? extendedRE) (make-seqn (regularFromExtended (arg1 extendedRE)) (regularFromExtended (arg2 extendedRE)))
		(or? extendedRE) (make-or (regularFromExtended (arg1 extendedRE)) (regularFromExtended (arg2 extendedRE)))
		(star? extendedRE) (make-star (regularFromExtended (arg1 extendedRE)))
		(string? extendedRE) (exString extendedRE '*epsilon*)
		(optional? extendedRE) (make-or '*epsilon* (regularFromExtended (arg1 extendedRE)))
		(plus? extendedRE) (make-seqn (regularFromExtended (arg1 extendedRE)) (make-star (regularFromExtended (arg1 extendedRE))))
		:else extendedRE))

;3.a 
(defn nfa? [e] (and (list? e) (= (first e) 'nfa) (list? (nth e 3))))

;3.b
(defn make-nfa [tFnTree iState fStates] (list 'nfa tFnTree iState fStates))

;3.c
(defn nfa-delta [nfa] 
	(if (nfa? nfa)
		(arg1 nfa)
		(throw (Exception. "nfa-delta input error, excepting nfa"))))

(defn nfa-q0 [nfa]
	(if (nfa? nfa)
		(arg2 nfa)
		(throw (Exception. "nfa-q0 input error, excepting nfa"))))

(defn nfa-final [nfa] 
	(if (nfa? nfa)
		(nth nfa 3)
		(throw (Exception. "nfa-final input error, excepting nfa"))))



; 4.a
(defn simple-trans? [e] (and (list? e) (= (first e) 'simpleTr)))

(defn compound-trans? [e] (and (list? e) (= (first e) 'compoundTr)))

; 4.b
(defn make-simple-trans [state chr l] (list 'simpleTr state chr l))
; 4.c
(defn make-compound-trans [tr1 tr2] (list 'compoundTr tr1 tr2))

; 4.d
(defn simple-istate [sTans] (nth sTans 1))
(defn simple-isyme [sTans] (nth sTans 2))
(defn simple-ostates [sTans] (nth sTans 3))

; 4.e
(defn compound-fst [cTrans] (nth cTrans 1))
(defn compound-snd [cTrans] (nth cTrans 2))

; 5.a
(def counter (atom 0))

; 5.b
(defn reset-counter [] (reset! counter 0))

; 5.c
(defn gen-state-num [] (swap! counter inc) (- (deref counter) 1))

; test 5
; (println (gen-state-num))
; (println (gen-state-num))
; (println (reset-counter))


; (println (gen-state-num))
; (println (gen-state-num))
; (println (reset-counter))

; 6
(defn nfaFromRegExp [re]
	(cond 
	    (epsilon? re) (let [q0 (gen-state-num)
                                q1 (gen-state-num)]
                       (make-nfa (make-simple-trans q0 re (list q1)) q0 (list q1)))
        (char? re) (let [q0 (gen-state-num)
                             q1 (gen-state-num)]
                       (make-nfa (make-simple-trans q0 re (list q1)) q0 (list q1)))
        (or? re)  (let [q0 (gen-state-num)
        				nfa1 (nfaFromRegExp (arg1 re))
	               	    nfa2 (nfaFromRegExp (arg2 re))
			    		q1 (gen-state-num)]
					    (make-nfa 
					        (make-compound-trans
						     (make-compound-trans
						          (make-compound-trans
							       (make-simple-trans q0 '*epsilon* (list (nfa-q0 nfa1) (nfa-q0 nfa2)))
							       (make-compound-trans
							             (make-simple-trans (first (nfa-final nfa1)) '*epsilon* (list q1))
								     (make-simple-trans (first (nfa-final nfa2)) '*epsilon* (list q1))))
						          (nfa-delta nfa1))
						     (nfa-delta nfa2))
						q0 (list q1)))
            
		; start of star
		 (star? re) (let [q0 (gen-state-num)
						nfa (nfaFromRegExp (arg1 re))
						f0 (gen-state-num)
						addStartTree (make-compound-trans (make-simple-trans q0 '*epsilon* (list (nfa-q0 nfa))) (nfa-delta nfa))
						addJumpTree (make-compound-trans (make-simple-trans q0 '*epsilon* (list f0)) addStartTree)
						addBackTree (make-compound-trans (make-simple-trans (first (nfa-final nfa)) '*epsilon* (list (nfa-q0 nfa))) addJumpTree)
						addEndTree (make-compound-trans (make-simple-trans (first (nfa-final nfa)) '*epsilon* (list f0)) addBackTree)
						]
						(make-nfa addEndTree q0 (list f0)))
		; end of star

		;start of seqn
		(seqn? re) (let [nfa1 (nfaFromRegExp (arg1 re))
						nfa2 (nfaFromRegExp (arg2 re))
						conTree (make-compound-trans (nfa-delta nfa1) (nfa-delta nfa2))
						addBridgeTree (make-compound-trans (make-simple-trans (first (nfa-final nfa1)) '*epsilon* (list (nfa-q0 nfa2))) conTree)
						]
						(make-nfa addBridgeTree (nfa-q0 nfa1) (list (first (nfa-final nfa2)))))
		:else (throw (Exception. "Unexpected Input --- not a Reg Exp"))))
		;end of seqn
; 6-test-case
;(println (nfaFromRegExp (make-seqn \a \b)))

; 7 (helper-a)
; return the list of compounded transition functions

(defn transNFAs [nfas q0]
      (if (empty? (rest nfas))
            (make-compound-trans
	        (make-simple-trans q0 '*epsilon* (list (nfa-q0 (first nfas))))
	        (nfa-delta (first nfas)))
            (let [comp (transNFAs (rest nfas) q0)]
            (make-compound-trans
	        (make-simple-trans q0 '*epsilon* (list (nfa-q0 (first nfas))))
	        (make-compound-trans
	            (nfa-delta (first nfas))
		    comp)))))
; test 7a  transNFAs
;  (transNFAs (list (nfaFromRegExp '(or \a \b)) (gen-state-num)))
;  (def l3 (list (nfaFromRegExp '*epsilon*) (nfaFromRegExp \c)))

; 7 (helper-b)
;return the list of all final states

;(defn finalNFAs [nfas]
;      (if (empty? (rest nfas))
;          (nfa-final (first nfas))
;	  (let [final (finalNFAs (rest nfas))]
;	       (concat (nfa-final (first nfas)) final))))
; 7
(defn finalNFAs [nfas]
      (map
          (fn [nfass] (first (nfa-final nfass))) nfas))

(defn mapToList [mapSeq]
      (cond (empty? (rest mapSeq)) (list (first mapSeq))
            :else (conj (mapToList (rest mapSeq)) (first mapSeq))))

; test 7b finalNFAs
; (def nfas1 (list (nfaFromRegExp '(or \a \b)) (nfaFromRegExp '*epsilon*)))
; (def nfas2 (list (nfaFromRegExp '*epsilon*)))

(defn glueNFAs [nfas]
      (let [q0 (gen-state-num)
           	delta (transNFAs nfas q0)
	   		final (mapToList (finalNFAs nfas))]
	   		;(println "final states" final)
            (make-nfa delta q0 final)))

; test 7
; (def l3 (list (nfaFromRegExp '*epsilon*) (nfaFromRegExp \c)))
; (println (glueNFAs l3))

;8
(defn parseTree [transTree aList] 
	(if (simple-trans? transTree)
		(conj aList transTree)
		(parseTree (compound-snd transTree) (parseTree (compound-fst transTree) aList))))

(defn findState [tList stateN chr aList]
	(if (empty? tList)
		aList
		(if (and (= (simple-isyme (first tList)) chr) (= stateN (simple-istate (first tList))))
			(recur (rest tList) stateN chr (reduce conj aList (simple-ostates (first tList))))
			(recur (rest tList) stateN chr aList))))

		
(defn applyTrans [transTree stateN chr] 
	(let [treeList (parseTree transTree ())] ; put all transition in a list
		(findState treeList stateN chr ()))) ; recursively check the list, get output list.


 ;  (def comT (make-compound-trans 
	; (make-compound-trans 
	; 	(make-simple-trans 0 '*epsilon* (list 1)) 
	; 	(make-simple-trans 0 '*epsilon* (list 2))) 
	; (make-simple-trans 0 \b (list 3))))

;(println (parseTree comT ()))
;(println (applyTrans comT 0 \a))

; 9.a
(defn driver? [e] (and (list? e) (= 'driver (first e))))

; 9.b
(defn make-driver [table eClosureF iState fStates] (list 'driver table eClosureF iState fStates))

; 9.c
(defn driver-table [driver] (arg1 driver))

(defn driver-epsilonClosure [driver] (arg2 driver))

(defn driver-q0 [driver] (arg3 driver))

(defn driver-final [driver] (arg4 driver))

;10



(defn getMaxState [tList maxState]
	(if (empty? tList)
		maxState
		(do
			(let [maxValue (apply max (conj (simple-ostates (first tList)) (simple-istate (first tList))))]
			(if (> maxValue maxState)
				(recur (rest tList) maxValue)
				(recur (rest tList) maxState))))))

(defn makeEmptyList [chrs aList] 
	(if (< chrs 0) 
		aList 
		(recur (dec chrs) (conj aList ()))))

(defn makeNestArr [states chrs arr]
	(if (< states 0)
		arr
		(recur (dec states) chrs (conj arr (makeEmptyList chrs ())))))

(defn make-matrix [treeList matrix]
	(if  (empty? treeList)
		matrix
		(do ; do allow multiple expressions
			(if (not (epsilon? (simple-isyme (first treeList))))
				(aset matrix (simple-istate (first treeList)) (int (simple-isyme (first treeList))) 
					(simple-ostates (first treeList))))
			(recur (rest treeList) matrix))))

(defn arrayFromTransTree [transTree]
	(let [treeList (parseTree transTree ())
		maxState (getMaxState treeList 0)
		arr (makeNestArr maxState 126 ())
		matrix (to-array-2d arr)]
		(make-matrix treeList matrix)
		))
;test makeNestArr
;(println (makeNestArr 10 10 ()))

; test 10
; (def comT (make-compound-trans 
; 	(make-compound-trans 
; 		(make-simple-trans 0 \a (list 1)) 
; 		(make-simple-trans 0 \c (list 2))) 
; 	(make-simple-trans 0 \b (list 3))))
; (def arr2d (arrayFromTransTree comT) )
; (println (aget arr2d 0 (int \a)))
; (println (aget arr2d 0 (int \b)))
; (println (aget arr2d 0 (int \c)))

; (println (aget (arrayFromTransTree tree) 4 97 ))
;11

(defn mergeList [addtion aList]
	(if (empty? addtion)
		aList
		(if (inList (first addtion) aList)
			(recur (rest addtion) aList)
			(recur (rest addtion) (conj aList (first addtion))))))

(defn findEC [func stack stateList]
	(if (empty? stack)
		stateList
		(let [ex (func (first stack) '*epsilon*)]
			(recur func (mergeList ex (rest stack)) (mergeList ex stateList)))))


; Issue: "epsilonClosureHelper"
; Should be taking "function" rather than "transTree" for 1-st argument
;                  "function" => "applyTrans"

(defn epsilonClosureHelper [func stateList]
 	(let [stack stateList]; make stack 
 		(findEC func stack stateList))) ; return the e-closure

;test 11
; (def func (fn [l x] (applyTrans (nfa-delta nfa) l x)))
; (println (epsilonClosureHelper func '(0 4)))


; 12

(defn driverFromNFA [nfa]
      (let [atf (memoize (fn [l x] (applyTrans (nfa-delta nfa) l x))) ; l is states, x is char
            ecf (memoize (fn [l] (epsilonClosureHelper atf l)))		  ; l is a list 
	    table (arrayFromTransTree (nfa-delta nfa))
	    q0 (nfa-q0 nfa)
	    final (nfa-final nfa)]
	    (make-driver table ecf q0 final)))


; 
; test 12
(def tree '(compoundTr (simpleTr 0 *epsilon* (4)) (compoundTr (compoundTr (simpleTr 5 *epsilon* (6)) (compoundTr (simpleTr 4 \a (5)) (simpleTr 6 \b (7)))) (compoundTr (simpleTr 0 *epsilon* (12)) (compoundTr (compoundTr (compoundTr (simpleTr 12 *epsilon* (8 10)) (compoundTr (simpleTr 9 *epsilon* (13)) (simpleTr 11 *epsilon* (13)))) (simpleTr 8 \e (9))) (simpleTr 10 *epsilon* (11)))))))
(def nfa (make-nfa tree 0 '(4 7)))
;(println (driverFromNFA nfa))

; 13
(defn transferStates [table fromList chr resList]
	(if (empty? fromList)
		resList
		(recur table (rest fromList) chr (mergeList (aget table (first fromList) (int chr)) resList))))

(defn driverActionHelper [table eClosureFunc string iIndex curIndex stateList tripleList]
	(if (or (empty? stateList) (>= curIndex (count string)))
		(if (> iIndex (dec (count string)))
			false
			tripleList)
		(do (let [third  (eClosureFunc (transferStates table stateList (get string curIndex) ()))
				triple (list iIndex curIndex third)]
			(if (empty? third)
				(recur table eClosureFunc string iIndex (inc curIndex) third tripleList)
				(recur table eClosureFunc string iIndex (inc curIndex) third (conj tripleList triple)))))))
	
;test 13
; (let [table (arrayFromTransTree tree)
; 	eClosureFunc (driver-epsilonClosure (driverFromNFA nfa))
; 	stateList (eClosureFunc '(0))]
; (println (aget table 4 97))
; (println (driverActionHelper table eClosureFunc "abc" 0 0 stateList ())))

; 14
(defn insert [x y] 
	(if (empty? y)
		(list x)
  		(if (> x (first y))
  			(cons (first y) (insert x (rest y)))
  			(cons x y))))

(defn insertion-sort [x]
	(if (empty? x)
		x
		(insert (first x) (insertion-sort (rest x)))))

(defn findMatchHelper [fList triple]
	(if (empty? fList)
		()
		(if (inList (first fList) (arg2 triple))
			(list (first fList) triple)
			(recur (rest fList) triple))))

(defn findMatch [fList tList]
	(if (empty? tList) 
		false
		(let [pair (findMatchHelper fList (first tList))]
			(if (not (empty? pair))
				pair
				(recur fList (rest tList))))))

(defn searchFromFBL [fState funcBindList]
    (if (empty? funcBindList)
    (throw (Exception. ( "searchFromFBL: state not found.")))
    (if (inList fState (first (first funcBindList)))
        (second (first funcBindList))
        (searchFromFBL fState (rest funcBindList)))))

(defn identifyLexeme [tripleList string fStateList mutableRefer funcBindList]
	(let [fStateListSorted (reverse fStateList)
		matchRes (findMatch fStateListSorted tripleList)]
		;(println "final sorted" fStateListSorted "triple list" tripleList matchRes)
		(if (= false matchRes) 
			false
			(do (reset! mutableRefer (inc (arg1 (arg1 matchRes))))
				(let [lexeme (subs string (first (arg1 matchRes)) (inc (arg1 (arg1 matchRes))))
					fState (first matchRes)
					func (searchFromFBL fState funcBindList)]
					(func lexeme))))))

; test 14

; (let [table (arrayFromTransTree tree)
; 	eClosureFunc (driver-epsilonClosure (driverFromNFA nfa))
; 	stateList (eClosureFunc '(0))
; 	tripleList (driverActionHelper table eClosureFunc "abc" 0 0 stateList ())
; 	fbl (list (list 7 (fn [lex] lex)))
; 	fStateList (list 7)]
; (println (identifyLexeme tripleList "abc" fStateList (atom 0) fbl)))

		

; 15
; [table eClosureFunc string iIndex curIndex stateList tripleList]
(defn driverAction [driver string eofToken funcBindList]
	(def mutableRefer (atom 0))
	(let [
		 table (driver-table driver)
		 ec (driver-epsilonClosure driver)
		 q0 (driver-q0 driver)
		 final (driver-final driver)]
	 (fn [] (let [tripleList (driverActionHelper table ec string @mutableRefer @mutableRefer (ec (list q0)) ())]
	 			;(println @mutableRefer)
	 			(if (= tripleList false)
	 				eofToken
	 				(let [tok (identifyLexeme tripleList string final mutableRefer funcBindList)]
	 					(if (not (= false tok))
	 						tok
	 						(recur))))))))
	             

; test 15

; (def table (arrayFromTransTree tree))
; (def driver (driverFromNFA nfa))
; (def eClosureFunc (nth driver 2))
; (def stateList (eClosureFunc '(0)))
; (def tripleList (driverActionHelper table eClosureFunc "abc" 0 0 stateList ()))
; (def fbl (list (list 7 (fn [lex] lex))))
; (def fStateList (list 7))
; (def token (identifyLexeme tripleList "abc" fStateList (atom 0) fbl))
; (println (driverAction driver "abc" token fbl))


; 16
(defn reListToNfaList [reList nfaList]
	(if (empty? reList)
		nfaList
		(recur (rest reList) (conj nfaList (nfaFromRegExp (first reList))))))

(defn nfaFromPattern [patterns regularBList reList]
	(if (empty? patterns)
		(reListToNfaList (reverse reList) ())
		(let [extendedRE (extendedFromNames (first patterns) regularBList)]
			(recur (rest patterns) regularBList (conj reList (regularFromExtended extendedRE))))))

(defn getFuncBindList [nfaList funcList bList]
	(if (empty? nfaList)
		bList
		(recur (rest nfaList) (rest funcList) (conj bList (list (arg3 (first nfaList)) (first funcList))))))

(defn make-lexer [defList funcList]
	(let [
		nameBList (arg1 (arg1 defList))
		regularBList (buildEnv nameBList)
		extendRBList (conj regularBList (list 'whitespace (make-range (char 0) (char 32))))
		patterns (arg1 (arg2 defList))
		nfaList (nfaFromPattern patterns extendRBList ())
		nfaAll (glueNFAs nfaList)
		funcBindList (getFuncBindList (reverse nfaList) funcList ())
		aDriver (driverFromNFA nfaAll)
		]
		(fn [string] (driverAction aDriver string (arg1 (first defList)) funcBindList))
		))
