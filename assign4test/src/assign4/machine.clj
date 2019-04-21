(ns assign4.machine)

; machine oriented helper code

(defn emit [instr instructions] (conj instructions instr))

; constructors for machine instructions

(defn make-ldc [c] (list 'ldc c))

(defn make-load [id] (list 'load id))

(defn make-load-global [id] (list 'load-global id))

(defn make-extend [id] (list 'extend id))

(defn make-store [id] (list 'store id))

(defn make-prim [name] (list 'prim name))

(defn make-invoke [label] (list 'invoke label))

(defn make-invoke-closure [] (list 'invoke-closure))

(defn make-create-closure [label] (list 'create-closure label))

(defn make-return [] (list 'return))

(defn make-goto [label] (list 'goto label))

(defn make-if-not-goto [label] (list 'if-not-goto label))

(defn make-create-tuple [n] (list 'create-tuple n))

(defn make-tuple-name [name] (list 'tuple-name name))

(defn make-halt [] (list 'halt))

; predicates for machine instructions

(defn ldc? [a] (and (seq? a) (not (empty? a)) (= (first a) 'ldc)))

(defn load? [a] (and (seq? a) (not (empty? a)) (= (first a) 'load)))

(defn load-global? [a] (and (seq? a) (not (empty? a)) (= (first a) 'load-global)))

(defn extend? [a] (and (seq? a) (not (empty? a)) (= (first a) 'extend)))

(defn store? [a] (and (seq? a) (not (empty? a)) (= (first a) 'store)))

(defn prim? [a] (and (seq? a) (not (empty? a)) (= (first a) 'prim)))

(defn invoke? [a] (and (seq? a) (not (empty? a)) (= (first a) 'invoke)))

(defn invoke-closure? [a] (and (seq? a) (not (empty? a)) (= (first a) 'invoke-closure)))

(defn create-closure? [a] (and (seq? a) (not (empty? a)) (= (first a) 'create-closure)))

(defn return? [a] (and (seq? a) (not (empty? a)) (= (first a) 'return)))

(defn goto? [a] (and (seq? a) (not (empty? a)) (= (first a) 'goto)))

(defn if-not-goto? [a] (and (seq? a) (not (empty? a)) (= (first a) 'if-not-goto)))

(defn create-tuple? [a] (and (seq? a) (not (empty? a)) (= (first a) 'create-tuple)))

(defn tuple-name? [a] (and (seq? a) (not (empty? a)) (= (first a) 'tuple-name)))

(defn halt? [a] (and (seq? a) (not (empty? a)) (= (first a) 'halt)))

; selectors

(defn instr-arg1 [a] (nth a 1))

(defn instr-arg2 [a] (nth a 2))


; helper code

(defn lookup [x env]
   (if (empty? env) 
       (throw (Exception. (str "Name " (str x) " is undefined")))
       (let [pair (first env)
             key (first pair)
             val (second pair)]
          (if (= x key)
              val
              (lookup x (rest env))))))

; alto data

(defn alto-cons [car cdr] (vector 'tuple-like "*Cons*" car cdr))

(def alto-empty-list (vector 'tuple-like "*EmptyList*"))

(def alto-unit (vector 'tuple-like "*Unit*"))

(defn is-tuple-like [a] (and (vector? a) (= (get a 0) 'tuple-like)))

(defn get-tuple [tl n] (get tl (+ n 2)))

(defn get-tuple-name [tl] (get tl 1))

(def closure-var '*closure-var*)

(defn closure-object? [a] (and (vector? a) (= (get a 0) 'closure)))

(defn closure-label [c] (get c 1))

(defn closure-data [c] (get c 2))

(defn new-closure [label data] ['closure label data])

(defn update-name [tl name] (apply vector (conj (rest (rest (apply list tl))) name (first tl))))

(defn new-tuple [values] (apply vector (conj values "*anonymous*" 'tuple-like)))


  
(declare string-from-alto-data)


(defn string-from-alto-tuple [tl]
  (cond (= (count tl) 2) "" 
        (= (count tl) 3) (string-from-alto-data (get tl 2))
        :else
         (loop [i 2 result ""]
           (if (= i (- (count tl) 2))
               (str result 
                    (string-from-alto-data (get tl i)) ", " 
                    (string-from-alto-data (get tl (+ i 1))))
               (recur (+ i 1) (str result (string-from-alto-data (get tl i)) ", "))))))

(defn string-from-alto-list [al]
   (if (and (is-tuple-like al) (= (get-tuple-name al) "*Cons*"))
        (let [car-string (string-from-alto-data (get al 2))
              cdr (get al 3)]
          (if (= cdr alto-empty-list)
              car-string
              (str car-string ", " (string-from-alto-list cdr))))
       (throw (Exception. (str "In string-from-alto-list, encountered bad list " (str al))))))

(defn string-from-alto-data [data]
  (cond (= data alto-unit) "()"
        (= data alto-empty-list) "[]"
        (number? data) (str data)
        (= data true) "True"
        (= data false) "False"
        (string? data) (str "\"" data "\"")
        (is-tuple-like data) 
         (cond (= (get-tuple-name data) "*Cons*") (str "[" (string-from-alto-list data) "]")
               (= (get-tuple-name data) "*anonymous*") (str "(" (string-from-alto-tuple data) ")")
               :else (str (str (get-tuple-name data)) "(" (string-from-alto-tuple data) ")"))
        (closure-object? data) "<function>"
        :else (throw (Exception. (str "In string-from-alto-data, unexpected data " (str data))))))


; machine execution


(def empty-lenv '())


(defn alto-print [data] 
  (do (print (string-from-alto-data data))
      alto-unit))


(defn run-machine [code jump-table prim-table genv lenv vstack cstack]
  (let [instr (first code)]
    (cond (symbol? instr) 
           (recur (rest code) jump-table prim-table genv lenv vstack cstack)
          (ldc? instr)
            (let [v (instr-arg1 instr)]
              (recur (rest code) jump-table prim-table genv lenv (conj vstack v) cstack))
          (load? instr)
            (let [v (lookup (instr-arg1 instr) lenv)]
              (recur (rest code) jump-table prim-table genv lenv (conj vstack v) cstack))
          (load-global? instr) 
            (let [v (deref (genv (instr-arg1 instr)))]
              (recur (rest code) jump-table prim-table genv lenv (conj vstack v) cstack))
          (extend? instr)
           (let [x (instr-arg1 instr)
                 v (first vstack)
                 x+v (list x v)]
             (recur (rest code) jump-table prim-table genv (conj lenv x+v) (rest vstack) cstack))
          (store? instr)
           (let [ref (genv (instr-arg1 instr))]
             (reset! ref (first vstack))
             (recur (rest code) jump-table prim-table genv lenv (rest vstack) cstack))
          (prim? instr)
           (let [new-vstack ((prim-table (instr-arg1 instr)) vstack)]
             (recur (rest code) jump-table prim-table genv lenv new-vstack cstack))
          (invoke? instr)
           (let [new-cstack (conj cstack lenv (rest code))
                 new-code   (jump-table (instr-arg1 instr))]
             (recur new-code jump-table prim-table genv empty-lenv vstack new-cstack))
          (invoke-closure? instr)
           (let [f (first vstack)]
             (if (closure-object? f)
                 (let [data  (closure-data f)
                       label (closure-label f)
                       new-cstack (conj cstack lenv (rest code))
                       new-code (jump-table label)
                       new-lenv (conj empty-lenv (list closure-var data))]
                   (recur new-code jump-table prim-table genv new-lenv (rest vstack) new-cstack))
                 (throw (Exception. (str "In run-machine, expected closure, not " f)))))
          (create-closure? instr)
           (let [label (instr-arg1 instr)
                 data  (first vstack)
                 ob    (new-closure label data)]
             (recur (rest code) jump-table prim-table genv lenv (conj (rest vstack) ob) cstack))
          (return? instr)
           (let [new-code (first cstack)
                 cstack1  (rest cstack)
                 new-lenv (first cstack1)
                 cstack2  (rest cstack1)]
             (recur new-code jump-table prim-table genv new-lenv vstack cstack2))
          (goto? instr) 
           (let [new-code (jump-table (instr-arg1 instr))]
             (recur new-code jump-table prim-table genv lenv vstack cstack))
          (if-not-goto? instr)
           (let [v (first vstack)
                 new-code (jump-table (instr-arg1 instr))]
             (if v
                 (recur (rest code) jump-table prim-table genv lenv (rest vstack) cstack)
                 (recur new-code jump-table prim-table genv lenv (rest vstack) cstack)))
          (create-tuple? instr)
           (let [vs (take (instr-arg1 instr) vstack)
                 new-vstack (drop (instr-arg1 instr) vstack)
                 t (new-tuple vs)]
             (recur (rest code) jump-table prim-table genv lenv (conj new-vstack t) cstack))
          (tuple-name? instr)
           (let [v (first vstack)
                 ob (update-name v (instr-arg1 instr))]
             (recur (rest code) jump-table prim-table genv lenv (conj (rest vstack) ob) cstack))
          (halt? instr) (do (alto-print (first vstack)) (println))
          :else (throw (Exception. (str "In run-machine, unexpected instruction " (str instr)))))))

; jump table

(defn build-jump-table-list [code]
  (cond (empty? code) '()
        (symbol? (first code)) (conj (build-jump-table-list (rest code)) (rest code) (first code))
        :else (build-jump-table-list (rest code))))

(defn build-jump-table [code] (apply hash-map (build-jump-table-list code)))

; primitive functionality

(defn make-prim-behavior [n f]
  (fn [vstack]
    (let [top (take n vstack)
          new-vstack (conj (drop n vstack) (apply f top))]
      new-vstack)))


(def prim-table
  {'*Cons* (make-prim-behavior 2 alto-cons) ,
   '*==i* (make-prim-behavior 2 =) ,
   '*!=i* (make-prim-behavior 2 not=) ,
   '*<=i* (make-prim-behavior 2 <=) ,
   '*<i* (make-prim-behavior 2 <) ,
   '*>=i* (make-prim-behavior 2 >=) ,
   '*>i* (make-prim-behavior 2 >) ,
   '*==d* (make-prim-behavior 2 =) ,
   '*!=d* (make-prim-behavior 2 not=) ,
   '*<=d* (make-prim-behavior 2 <=) ,
   '*<d* (make-prim-behavior 2 <) ,
   '*>=d* (make-prim-behavior 2 >=) ,
   '*>d* (make-prim-behavior 2 >) ,
   '*+i* (make-prim-behavior 2 +) ,
   '*-i* (make-prim-behavior 2 -) ,
   '**i* (make-prim-behavior 2 *) ,
   '*divi* (make-prim-behavior 2 quot) ,
   '*negi* (make-prim-behavior 1 -) ,
   '*+d* (make-prim-behavior 2 +) ,
   '*-d* (make-prim-behavior 2 -) ,
   '**d* (make-prim-behavior 2 *) ,
   '*divd* (make-prim-behavior 2 /) ,
   '*negd* (make-prim-behavior 1 -) ,
   'print (make-prim-behavior 1 alto-print) ,
   '*gen-unit* (make-prim-behavior 0 (fn [] alto-unit)) ,
   '*gen-empty-list* (make-prim-behavior 0 (fn [] alto-empty-list)) ,
   '*getTuple* (make-prim-behavior 2 get-tuple) , 
   '*isEqual* (make-prim-behavior 2 =) , 
   '*isUnit* (make-prim-behavior 1 (fn [a] (= a alto-unit))) , 
   '*isEmptyList* (make-prim-behavior 1 (fn [a] (= a alto-empty-list))) , 
   '*isTupleLike* (make-prim-behavior 1 is-tuple-like) , 
   '*getTupleName* (make-prim-behavior 1 get-tuple-name)})

; global environment

(defn build-genv-table [names]
  (loop [names names lst '()]
    (if (empty? names)
        (apply hash-map lst)
        (recur (rest names) (conj lst (atom nil) (first names))))))

; start machine

(defn load-and-go [code global-names]
  (run-machine code (build-jump-table code) prim-table (build-genv-table global-names) empty-lenv '() '()))