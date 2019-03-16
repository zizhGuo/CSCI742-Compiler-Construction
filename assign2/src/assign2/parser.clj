(ns assign2.parser
	(:use [assign2.genPar :only (make-parser)])
)

(defn make-sumi [e1 e2] (list '+i e1 e2))

(defn make-diffi [e1 e2] (list '-i e1 e2))

(defn make-prodi [e1 e2] (list '*i e1 e2))

(defn make-quoi [e1 e2] (list 'divi e1 e2))

(defn make-negi [e] (list 'negi e))


(def par-spec
  '((S (E 'EOF))
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

(def par-spec-actions
  (list (fn [$1 $2] $1)
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
        (fn [$1 $2 $3] $2) ))

(def parser (make-parser par-spec par-spec-actions))