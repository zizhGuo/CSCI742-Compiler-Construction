(ns assign2.parser
	(:use [assign2.genPar :only (make-parser)])
	;(:use [csci742.genPar :only (make-parser)])
)

; 1a 
(defn make-call [exp1 exp2] (list 'call exp1 exp2))

(defn make-call-pat [identifier patExpList] (list 'call-pat identifier patExpList))

(defn make-named-type [identifier typeExpList] (list 'NamedType identifier typeExpList))

; 1b
(defn make-unit [] (list 'Unit))

(defn make-tuple [l] (list 'Tuple l))

(defn make-empty-list [] (list 'EmptyList))

(defn make-from-parens [l]
  (cond 
    (empty? l) (make-unit)
    (= 1 (count l)) (first l);?
    :else (make-tuple l)))

; 1c 
(defn make-equali [exp1 exp2] ; 
	(make-call '*==i* (make-from-parens (list exp1 exp2))))

(defn make-notEquali [exp1 exp2] 
	(make-call '*!=i* (make-from-parens (list exp1 exp2))))

(defn make-lessEquali [exp1 exp2] 
	(make-call '*<=i* (make-from-parens (list exp1 exp2))))

(defn make-lessi [exp1 exp2] 
	(make-call '*<i* (make-from-parens (list exp1 exp2))))

(defn make-grEquali [exp1 exp2] 
	(make-call '*>=i* (make-from-parens (list exp1 exp2))))

(defn make-greati [exp1 exp2] 
	(make-call '*>i* (make-from-parens (list exp1 exp2))))

(defn make-equald [exp1 exp2] 
	(make-call '*==d* (make-from-parens (list exp1 exp2))))

(defn make-notEquald [exp1 exp2] 
	(make-call '*!=d* (make-from-parens (list exp1 exp2))))

(defn make-lessEquald [exp1 exp2] 
	(make-call '*<=d* (make-from-parens (list exp1 exp2))))

(defn make-lessd [exp1 exp2] 
	(make-call '*<d* (make-from-parens (list exp1 exp2))))

(defn make-grEquald [exp1 exp2] 
	(make-call '*>=d* (make-from-parens (list exp1 exp2))))

(defn make-greatd [exp1 exp2] 
	(make-call '*>d* (make-from-parens (list exp1 exp2))))

(defn make-sumi [exp1 exp2] 
	(make-call '*+i* (make-from-parens (list exp1 exp2))))

(defn make-diffi [exp1 exp2] 
	(make-call '*-i* (make-from-parens (list exp1 exp2))))

(defn make-prodi [exp1 exp2] 
	(make-call '**i* (make-from-parens (list exp1 exp2))))

(defn make-quoi [exp1 exp2] 
	(make-call '*divi* (make-from-parens (list exp1 exp2))))

(defn make-negi [exp1] 
	(make-call '*negi* (make-from-parens (list exp1))))

(defn make-sumd [exp1 exp2] 
	(make-call '*+d* (make-from-parens (list exp1 exp2))))

(defn make-diffd [exp1 exp2] 
	(make-call '*-d* (make-from-parens (list exp1 exp2))))

(defn make-prodd [exp1 exp2] 
	(make-call '**d* (make-from-parens (list exp1 exp2))))

(defn make-quod [exp1 exp2] 
	(make-call '*divd* (make-from-parens (list exp1 exp2))))

(defn make-negd [exp1] 
	(make-call '*negd* (make-from-parens (list exp1))))

(defn make-cons [exp1 exp2] 
	(make-call '*Cons* (make-from-parens (list exp1 exp2))))

(defn make-from-brackets [l]
  (if (empty? l) 
    (make-empty-list)
    (make-cons (first l) (make-from-brackets (rest l))))) ;This function needs "make-cons" to define.

; 1d

(defn make-or [exp1 exp2]
        (list 'or exp1 exp2))

(defn make-and [exp1 exp2]
        (list 'and exp1 exp2))

(defn make-if [exp1 exp2 exp3]
        (list 'if exp1 exp2 exp3))

(defn make-case [exp1 expList]
        (list 'case exp1 expList))

(defn make-let [exp1 exp2]
        (list 'let exp1 exp2))

(defn make-lambda [exp1]
        (list 'lambda exp1))

(defn make-error [exp1]
        (list 'error exp1))

; 1e

(defn make-unit-pat [] (make-unit))

(defn make-empty-list-pat [] (make-empty-list))

(defn make-tuple-pat [l] (list 'TuplePat l))

(defn make-pat-from-parens [l]
        (cond
	  (empty? l)
	  (make-unit-pat)
	  (= 1 (count l)) (first l);?
          :else (make-tuple-pat l)))

(defn make-cons-pat [pat1 pat2]
        (make-call-pat '*Cons* (make-pat-from-parens (list pat1 pat2))))

(defn make-pat-from-brackets [l]
  (if (empty? l) 
    (make-empty-list-pat)
    (make-cons-pat (make-pat-from-brackets (rest l)))))

; 1f

(defn make-unit-type [] (list 'UnitType))

(defn make-boolean-type [] (list 'BoolType))

(defn make-integer-type [] (list 'IntType))

(defn make-double-type [] (list 'DoubleType))

(defn make-string-type [] (list 'StringType))

(defn make-list-type [type] (make-named-type '*List* type))

(defn make-arrow-type [type1 type2] (list 'ArrowType type1 type2))

(defn make-tuple-type [lt] (list 'TupleType lt))

(defn make-type-parens [l] 
  (cond 
    (empty? l) (make-unit-type)
    (= 1 (count l)) (first l);?
    :else (make-tuple-type l)))

; 1g

(defn make-decls [l] (list 'decls l))

(defn make-data-construct [identifier type] (list 'constructor identifier type))

(defn make-data-decl [identifier idl conl] (list 'decl-data identifier idl conl))

(defn make-type-decl [identifier type] (list 'decl-type identifier type))

(defn make-def [identifier exp] (list 'def identifier exp))

(defn make-fun-def [identifier pat l]
        (list 'defun identifier (cons pat (drop-last l)) (last l)))

; 2

(def par-spec
	'((start (decls 'EOF))
		(decls ())
		(decls (decl decls))
		(decl ('DATA 'ID fmls 'EQ cstrs))
		(decl ('ID decl2))
		(decl2 ('COLON type))
		(decl2 ('EQ m))
		(decl2 ('OP ps 'CP decl3))
		(decl3 ('EQ m))
		(decl3 ('OP ps 'CP decl3))
		(fmls ())
		(fmls ('OP 'ID  fmls2 'CP))
		(fmls2 ())
		(fmls2 ('COMMA 'ID  fmls2))
		(cstr ('ID 'OP types 'CP))
		(cstrs (cstr cstrs2))
		(cstrs2 ())
		(cstrs2 ('OR2  cstr cstrs2))
		(type (type2 type3))
		(type2 ('boolean))
		(type2 ('integer))
		(type2 ('double))
		(type2 ('string))
		(type2 ('ID type4))
		(type2 ('OP types 'CP))
		(type2 ('OB types 'CB))
		(type3 ())
		(type3 ('ARROW type))
		(type4 ())
		(type4 ('OP types 'CP))
		(types ())
		(types (type types2))
		(types2 ())
		(types2 ('COMMA type types2))
		(seq ())
		(seq (m seq2))
		(seq2 ())
		(seq2 ('COMMA m seq2))
		(m ('IF m 'THEN m 'ELSE m))
		(m ('CASE m 'OF mbs))
		(m ('LET ds in m))
		(m ('LAM mbs))
		(m ('ERROR m))
		(m (bo))
		(d (p 'EQ m))
		(ds (d ds2))
		(ds2 ())
		(ds2 ('COMMA d ds2))
		(mb (p 'ARROW 'OP m 'CP ))
		(mbs (mb mbs2))
		(mbs2 ())
		(mbs2 ('SEMI mb mbs2))
		(p (ap p2))
		(p2 ())
		(p2 ('COLON2 p))
		(ap ('BOOL))
		(ap ('INT))
		(ap ('DBL))
		(ap ('MINUS int))
		(ap ('MINUSD 'DBL))
		(ap ('STR))
		(ap ('ID ap2))
		(ap ('OB ps 'CB))
		(ap ('OP ps 'CP))
		(ap2 ())
		(ap2 ('OP ps 'CP))
		(ps ())
		(ps (p ps2))
		(ps2 ())
		(ps2 ('COMMA p ps2))
		(bo (ba bo2))
		(bo2 ('OR2  ba bo2))
		(bo2 ())
		(ba (r ba2))
		(ba2 ('AND2 r ba2))
		(ba2 ())
		(r (e r2))
		(r2 ())
		(r2 ('EQ2 e))
		(r2 ('NEQ e))
		(r2 ('LEQ e))
		(r2 ('LESS e))
		(r2 ('GEQ e))
		(r2 ('GREAT e))
		(r2 ('EQ2D e))
		(r2 ('NEQD e))
		(r2 ('LEQD e))
		(r2 ('LESSD e))
		(r2 ('GEQD e))
		(r2 ('GREATD e))
		(e (t e2))
		(e2 ('PLUS t e2))
		(e2 ('MINUS t e2))
		(e2 ('PLUSD t e2))
		(e2 ('MINUSD t e2))
		(e2 ())
		(t (f t2))
		(t2 ('STAR f t2))
		(t2 ('SLASH f t2))
		(t2 ('TIMESD f t2))
		(t2 ('SLASHD f t2))
		(t2 ())
		(f ('MINUS f))
		(f ('MINUSD f))
		(f (c f2))
		(f2 ())
		(f2 ('COLON2 f))
		(c (v c2))
		(c2 ())
		(c2 ('OP seq 'CP c2))
		(v ('BOOL))
		(v ('INT))
		(v ('DBL))
		(v ('STR))
		(v ('ID))
		(v ('OB seq 'CB ))
		(v ('OP seq 'CP ))))


(def par-spec-actions
	(list (fn [$1 $2] (make-decls $1))
		(fn [] ())
		(fn [$1 $2] (conj $2 $1))
		(fn [$1 $2 $3 $4 $5] (make-data-decl $2 $3 $5))
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2] (fn [I] (make-type-decl I $2)))
		(fn [$1 $2] (fn [I] (make-def I $2)))
		(fn [$1 $2 $3 $4] (fn [I] (make-fun-def I (make-pat-from-parens $2) $4)))
		(fn [$1 $2] (list $2))
		(fn [$1 $2 $3 $4] (conj $4 (make-pat-from-parens $2)))
		(fn [] ())
		(fn [$1 $2 $3 $4] (conj $3 $2))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2 $3 $4] (make-data-construct $1 (make-type-parens $3)))
		(fn [$1 $2] (conj $2 $1))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2] ($2 $1))
		(fn [$1] (make-boolean-type));?
		(fn [$1] (make-integer-type));?
		(fn [$1] (make-double-type));?
		(fn [$1] (make-string-type));?
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (make-type-parens $2))
		(fn [$1 $2 $3] (make-list-type $2))
		(fn [] (fn [T] T))
		(fn [$1 $2] (fn [T] (make-arrow-type T $2)));?
		(fn [] (fn [I] I))
		(fn [$1 $2 $3] (fn [I] (make-named-type I (make-type-parens $2))))
		(fn [] ())
		(fn [$1 $2] (conj $2 $1))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [] ())
		(fn [$1 $2] (conj $2 $1))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2 $3 $4 $5 $6] (make-if $2 $4 $6))
		(fn [$1 $2 $3 $4] (make-case $2 $4))
		(fn [$1 $2 $3 $4] (make-let $2 $4))
		(fn [$1 $2] (make-lambda $2))
		(fn [$1 $2] (make-error $2))
		(fn [$1] $1)
		(fn [$1 $2 $3] (list $1 $3)) ;clojure-list?
		(fn [$1 $2] (conj $2 $1))  ;clojure-conj?
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2 $3 $4 $5] (list $1 $4));?
		(fn [$1 $2] (conj $2 $1))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2] ($2 $1))
		(fn [] (fn [A] A))
		(fn [$1 $2] (fn [A] (make-cons-pat A $2)))
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1 $2] (- $2))
		(fn [$1 $2] (- $2))
		(fn [$1] $1)
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (make-pat-from-brackets $2))
		(fn [$1 $2 $3] (make-pat-from-parens $2))
		(fn [] (fn [I] I))
		(fn [$1 $2 $3] (fn [I] (make-call-pat I $2)))
		(fn [] ())
		(fn [$1 $2] (conj $2 $1))
		(fn [] ())
		(fn [$1 $2 $3] (conj $3 $2))
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (fn [B] ($3 (make-or B $2))))
		(fn [] (fn [B] B))
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (fn [R] ($3 (make-and R $2))))
		(fn [] (fn [R] R))
		(fn [$1 $2] ($2 $1))
		(fn [] (fn [E] E))
		(fn [$1 $2] (fn [E] (make-equali E $2)))
		(fn [$1 $2] (fn [E] (make-notEquali E $2)))
		(fn [$1 $2] (fn [E] (make-lessEquali E $2)))
		(fn [$1 $2] (fn [E] (make-lessi E $2)))
		(fn [$1 $2] (fn [E] (make-grEquali E $2)))
		(fn [$1 $2] (fn [E] (make-greati E $2)))
		(fn [$1 $2] (fn [E] (make-equald E $2)))
		(fn [$1 $2] (fn [E] (make-notEquald E $2)))
		(fn [$1 $2] (fn [E] (make-lessEquald E $2)))
		(fn [$1 $2] (fn [E] (make-lessd E $2)))
		(fn [$1 $2] (fn [E] (make-grEquald E $2)))
		(fn [$1 $2] (fn [E] (make-greatd E $2)))
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (fn [T] ($3 (make-sumi T $2))))
		(fn [$1 $2 $3] (fn [T] ($3 (make-diffi T $2))))
		(fn [$1 $2 $3] (fn [T] ($3 (make-sumd T $2))))
		(fn [$1 $2 $3] (fn [T] ($3 (make-diffd T $2))))
		(fn [] (fn [T] T))
		(fn [$1 $2] ($2 $1))
		(fn [$1 $2 $3] (fn [F] ($3 (make-prodi F $2))))
		(fn [$1 $2 $3] (fn [F] ($3 (make-quoi F $2))))
		(fn [$1 $2 $3] (fn [F] ($3 (make-prodd F $2))))
		(fn [$1 $2 $3] (fn [F] ($3 (make-quod F $2))))
		(fn [] (fn [F] F ))
		(fn [$1 $2] (make-negi $2))
		(fn [$1 $2] (make-negd $2))
		(fn [$1 $2] ($2 $1))
		(fn [] (fn [C] C))
		(fn [$1 $2] (fn [C] (make-cons C $2))) ;AP?
		(fn [$1 $2] ($2 $1))
		(fn [] (fn [V] V))
		(fn [$1 $2 $3 $4] (fn [V] ($4 (make-call V (make-from-parens $2)))))
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1] $1)
		(fn [$1 $2 $3] (make-from-brackets $2))
		(fn [$1 $2 $3] (make-from-parens $2))))

;;;;;;
; Example from previous assignment

; (defn make-sumi [e1 e2] (list '+i e1 e2))

; (defn make-diffi [e1 e2] (list '-i e1 e2))

; (defn make-prodi [e1 e2] (list '*i e1 e2))

; (defn make-quoi [e1 e2] (list 'divi e1 e2))

; (defn make-negi [e] (list 'negi e))


; (def par-spec
;  '((S (E 'EOF))
;    (E (T E2))
;    (E2 ('PLUS T E2))
;    (E2 ('MINUS T E2))
;    (E2 ())
;    (T (F T2))
;    (T2 ('STAR F T2))
;    (T2 ('SLASH F T2))
;    (T2 ())
;    (F ('INT))
;    (F ('ID))
;    (F ('MINUS F))
;    (F ('OP E 'CP))))

; (def par-spec-actions
;  (list (fn [$1 $2] $1)
;        (fn [$1 $2] ($2 $1))
;        (fn [$1 $2 $3] (fn [T] ($3 (make-sumi T $2))))
;        (fn [$1 $2 $3] (fn [T] ($3 (make-diffi T $2))))
;        (fn [] (fn [T] T))
;        (fn [$1 $2] ($2 $1))
;        (fn [$1 $2 $3] (fn [F] ($3 (make-prodi F $2))))
;        (fn [$1 $2 $3] (fn [F] ($3 (make-quoi F $2))))
;        (fn [] (fn [F] F))
;        (fn [$1] $1)
;        (fn [$1] $1)
;        (fn [$1 $2] (make-negi $2))
;        (fn [$1 $2 $3] $2) ))

(def parser (make-parser par-spec par-spec-actions))
