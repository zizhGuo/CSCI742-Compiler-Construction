(ns assign3.checker
	;(:use [assign3.parser])
	;(:use [assign3.unification :only (unifyTerm4 
		;failure
		;extend-history
		;theta-identity
		;logic-variable?
		;applyUnifier)])
	)

;1(a)

(defn arg1[l] (nth l 1))
(defn arg2[l] (nth l 2))
(defn arg3[l] (nth l 3))
(defn arg4[l] (nth l 4))

(defn variable? [l] (symbol? l))
(defn boolean? [l] (= (first l) 'boolean))
(defn double? [l] (= (first l) 'double))
(defn unit? [l] (= (first l) 'Unit))
(defn empty-list? [l] (= (first l) 'Emptylist))
(defn constant? [l] (or (boolean? l) (double? l) (unit? l) (empty-list? l) (integer? l) (string? l)))
(defn tuple? [l] (= (first l) 'Tuple))
(defn decls? [l] (= (first l) 'decls))
(defn data-decls? [l] (= (first l) 'decl-data))
(defn data-construct? [l] (= (first l) 'constructor))
(defn type-decl? [l] (= (first l) 'decl-type))
(defn def? [l] (= (first l) 'def))
(defn fun-def? [l] (= (first l) 'defun))
(defn or? [l] (= (first l) 'or))
(defn and? [l] (= (first l) 'and))
(defn if? [l] (= (first l) 'if))
(defn case? [l] (= (first l) 'case))
(defn let? [l] (= (first l) 'let))
(defn lambda? [l] (= (first l) 'lambda))
(defn lambda1? [l] (= (first l) 'lambda1));?
(defn error? [l] (= (first l) 'error))
(defn call? [l] (= (first l) 'call))
(defn boolean-type? [l] (= (first l) 'BoolType))
(defn integer-type? [l] (= (first l) 'IntType))
(defn double-type? [l] (= (first l) 'DoubleType))
(defn string-type? [l] (= (first l) 'StringType))
(defn unit-type? [l] (= (first l) 'UnitType))
(defn constant-type? [l] (or (boolean-type? l) (integer-type? l) (double-type? l) (string-type? l) (unit-type? l)))
(defn tuple-type? [l] (= (first l) 'TupleType))
(defn arrow-type? [l] (= (first l) 'ArrowType))
(defn named-type? [l] (= (first l) 'NamedType))
(defn forall-type? [l] (= (first l) 'forall))
(defn unit-pat? [l] (= (first l) 'Unit))
(defn empty-list-pat? [l] (= (first l) 'EmptyList))
(defn constant-pat? [l] (or (boolean? l) (integer? l) (double? l) (string? l) (unit-pat? l) (empty-list-pat? l)))
(defn tuple-pat? [l] (= (first l) 'TuplePat))
(defn call-pat? [l] (= (first l) 'call-pat))


(defn make-forall-type [L type] (list 'forall L type))

(defn make-lambda1 [idL exp] (list 'lambda1 idL exp))

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
								(reset! res (distinct (concat @res r)))
								(reset! res (distinct (conj @res r))))
						(recur (rest t)))))
				@res)
		(arrow-type? type) 
			(let [
				pre (free-variables-from-type (arg1 type)) 
				pos (free-variables-from-type (arg2 type)) 
				]
				(cond
					(and (list? pre) (list? pos)) (distinct (concat pre pos))
					(and (list? pre) (not (list? pos))) (distinct (conj pre pos))
					(and (not (list? pre)) (list? pos)) (distinct (conj pos pre))
					:else (list pre pos)
					))
		(named-type? type) (free-variables-from-type (arg2 type))
		(variable? type) type
		:else (throw (Exception. "Unexpected Input --- not a type"))))


;2a ii
(defn universalize-type [type]
	(make-forall-type (free-variables-from-type type) type))

; (def tree '(TupleType ((ArrowType a b) (NamedType *List* a))))
; (print (universalize-type tree))

;2b
(defn types-from-data [triList]
	(let [
		split (apply concat (map #(map #(list (first %) (second %) %1) (arg3 %)) triList))
	]))







