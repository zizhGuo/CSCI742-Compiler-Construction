(ns assign3.checker 
    (:use [assign3.parser]) 
    (:use [assign3.unification :only (unifyTerm4 
                                      failure 
                                      extend-history 
                                      theta-identity 
                                      logic-variable? 
                                      applyUnifier)]))

(defn free-variables-from-type [type] 
   (cond (variable? type) (list type)
         (constant-type? type) '()
         (tuple-type? type) (loop [result '() list_types (arg1 type)]
                                 (if (empty? list_types) result
                                (recur
                                   (distinct (concat result
                                      (free-variables-from-type (first list_types))))
                                      (rest list_types))))
         (arrow-type? type) (distinct (concat (free-variables-from-type (arg1 type))
                                  (free-variables-from-type (arg2 type))))
         (forall-type? type) (arg1 type)
         (named-type? type) (free-variables-from-type (arg2 type)) 
         :else (do (println "Type is: " type) (throw (Exception. "Input is not a type")))))

(defn universalize-type [type]
    (make-forall-type (free-variables-from-type type) type))

(defn types-from-data [list_triples_i_xs_cs] 
   (loop [result '() triple_list list_triples_i_xs_cs]
       (if (empty? (first triple_list)) result
           (let [name_data_type (first (first triple_list))
                 list_variable  (arg1 (first triple_list))
                 list_cstors    (arg2 (first triple_list))
                 ;dbg1 (println name_data_type)
                 ;dbg2 (println list_variable)
                 ;dbg3 (println list_cstors)
                 pred_x (fn [x] (some #(= x %) list_variable))
                 check_var
                    ; Check if declared variables match the variables used in constructor
                    (loop [cstr list_cstors]
                        (if (empty? cstr) true
                          (let [cstr_var (free-variables-from-type (arg2 (first cstr)))]
                             (if (every? pred_x cstr_var) (recur (rest cstr))
                               (throw 
                                  (Exception. 
                                        "Variable used in constructor does not 
                                         match variable used in declaration"))))))
                  result
                    (loop [sub_result result cstr list_cstors]
                        (if (empty? cstr) sub_result
                         (let [data_type_var (make-type-parens list_variable)
                              data_type (make-named-type name_data_type data_type_var)
                              cstr_name (arg1 (first cstr))
                              cstr_type (make-arrow-type (arg2 (first cstr)) data_type)
                              ;db3 (println "cstr" cstr)
                              ;dg4 (println "data_var" data_type_var)
                              ;db5 (println "data_type" data_type)
                              ;dg6 (println "cstr_name" cstr_name)
                              ;dg7 (println "cstr_type" cstr_type)
                              univ_data_type (universalize-type cstr_type)]
                             (recur (concat sub_result (list (list cstr_name univ_data_type)))
                                     (rest cstr)))))]
            (recur result (rest triple_list))))))


(defn make-bin-optype [tag type1 type2 return_type]  
                      (list tag (universalize-type 
                                (make-arrow-type 
                                      (make-tuple-type (list type1 type2)) return_type))))

(defn make-unary-optype [tag type return_type] 
                      (list tag (universalize-type 
                                (make-arrow-type 
                                      type return_type))))


(def init-type-env (list (make-unary-optype '*Cons* (make-tuple-type 
                                                       (list 'a (make-list-type 'a))) 
                                                     (make-list-type 'a))
                          (make-bin-optype '*==i* (make-integer-type) (make-integer-type) 
                                          (make-boolean-type))
                          (make-bin-optype '*!=i* (make-integer-type) (make-integer-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*<=i* (make-integer-type) (make-integer-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*<i* (make-integer-type) (make-integer-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*>=i* (make-integer-type) (make-integer-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*>i* (make-integer-type) (make-integer-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*==d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*!=d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*<=d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*<d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*>=d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*>d* (make-double-type) (make-double-type) 
                                           (make-boolean-type))
                          (make-bin-optype '*+i* (make-integer-type) (make-integer-type) 
                                           (make-integer-type))
                          (make-bin-optype '*-i* (make-integer-type) (make-integer-type) 
                                           (make-integer-type))
                          (make-bin-optype '**i* (make-integer-type) (make-integer-type) 
                                           (make-integer-type))
                          (make-bin-optype '*divi* (make-integer-type) (make-integer-type) 
                                           (make-integer-type))
                          (make-unary-optype '*negi* (make-integer-type) (make-integer-type))
                          (make-bin-optype '*+d* (make-double-type) (make-double-type) 
                                           (make-double-type))
                          (make-bin-optype '*-d* (make-double-type) (make-double-type) 
                                           (make-double-type))
                          (make-bin-optype '**d* (make-double-type) (make-double-type) 
                                           (make-double-type))
                          (make-bin-optype '*divd* (make-double-type) (make-double-type) 
                                           (make-double-type))
                          (make-unary-optype '*negd* (make-double-type) (make-double-type))
                          (make-unary-optype 'print (make-string-type) (make-unit-type))))
                          
(defn gen-formal [] (gensym "x*"))

(declare reduce-forms)

(defn create_process_funs_table [list_func_def]
    (loop [func_list list_func_def result '{} key_order '{}]
       (if (empty? func_list) (list result key_order)
           (let [func_def (first func_list)
                 func_name (arg1 func_def)
                 func_pat  (arg2 func_def)
                 func_pat_count (count func_pat)
                 actual_pat (if (> func_pat_count 1) 
                                   (list (make-tuple-pat func_pat)) func_pat)
                 func_exp  (reduce-forms (arg3 func_def))
                 value (get result func_name)
                 ;db1 (println "func_def" func_def)
                 ;db2 (println "func_name" func_name)
                 ;db3 (println "func_pat" func_pat
                 ;db4 (println "func_pat_count" func_pat_count)
                 ;db5 (println "func_exp" func_exp)
                 ;db8 (println "value" value)
                 new_value 
               (if (= value nil)
                       (list func_pat_count 
                                (concat actual_pat (list func_exp)))
                   (if (= (first value) func_pat_count) 
                                  (concat value (list (concat actual_pat (list func_exp))))
                       (throw 
                         (Exception.
                         "function with same name has different param number."))))
                  func_order (let [key_count (get key_order func_name)]
                               (if key_count key_count 
                                 (count key_order)))
                  update_key (assoc key_order func_name func_order)
                  update_map (assoc result func_name new_value)
                  ;db9 (println "size of map"(count update_map))
                 ;db6 (println "new_value" new_value)
                 ;db7 (println "new_map" update_map)
              ]
             (recur (rest func_list) update_map update_key)))))
                   

(defn build_func_form [num_parm formal_list value]
    (let [formal_parm (gen-formal)
          new_formal_list (concat formal_list (list formal_parm))]
      (if (= 1 num_parm)
          (if (= (count formal_list) 0) 
            (make-lambda1 formal_parm (make-case formal_parm value))
          (make-lambda1 formal_parm (make-case (make-tuple new_formal_list) value)))
    (make-lambda1 formal_parm 
        (build_func_form (- num_parm 1) new_formal_list value)))))
   

(defn transform [func_map] 
    (fn [func_name] 
      (let [value (get func_map func_name)
            ;dbg (println value)
           ]
         (list func_name (build_func_form (first value) '() (rest value))))))
    
(defn process-funs [list_func_def] 
    (let [fun_maps (create_process_funs_table list_func_def)
          ;db_t  (def f_map fun_map)
          db_a (println	 "two maps" fun_maps)
          order_map (sort-by #(val %) (second fun_maps))
          db_b (println "ordered" order_map)
          db_c (println "actual_map" (first fun_maps))
          fun_map_key (keys order_map)
          db_d (println fun_map_key)
          fun_map  (first fun_maps)
          func_formatter (transform fun_map)]
          (map func_formatter fun_map_key)))  


(defn build_let_format [let_list final_exp] 
   (let [pair (first let_list)
         ;db1 (println "pair" pair)
         ;db2 (println "let_list" let_list)
        ] 
      (if (= 1 (count let_list)) (make-case (reduce-forms (second pair)) 
                                            (list (list (first pair) final_exp)))  
      (make-case (reduce-forms (second pair)) 
                 (list (list (first pair) (build_let_format (rest let_list) final_exp)))))))           
(defn reduce-forms [exp] 
   (cond (if? exp) (make-case (reduce-forms (arg1 exp)) 
                     (list (list true (reduce-forms (arg2 exp)))
                           (list false (reduce-forms (arg3 exp)))))
         (or? exp) (reduce-forms (make-if (arg1 exp) true (arg2 exp)))
         (and? exp) (reduce-forms (make-if (arg1 exp) (arg2 exp) false))
         (let? exp) (build_let_format (arg1 exp) (arg2 exp))
         (lambda? exp) (let [fresh_var (gen-formal)
                             ;dbg   (println "list pairs" (arg1 exp))
                             pair_list (arg1 exp)
                             ;d1 (println "1st thing" (first pair_list))
                             ;d2 (println "p.1" (first (first pair_list)))
                             ;d3 (println "p.2" (second (first pair_list)))
                            ]
                          (make-lambda1 fresh_var (make-case fresh_var 
                              (map #(list (first %) (reduce-forms (second %))) pair_list))))
         :else exp)) 

(defn get_list_func [syntax_tree]
   (loop [st syntax_tree result '()]
     (if (empty? st) result
       (if (fun-def? (first st))  (recur (rest st) (concat result (list (first st))))
         (recur (rest st) result)))))


(defn split-decl [tree] 
  (let [list_func (get_list_func (arg1 tree))
        ;db1 (def fun_act list_func)
        form_list_func (process-funs list_func)
        ;db3 (def g_g form_list_func)
        ;db7 (println form_list_func)
         ]
   (loop [syntax_tree (arg1 tree) l1 '() l4 '() l5 '()]
     (if (empty? syntax_tree) (list l1 (types-from-data l1) init-type-env l4 
                                (concat form_list_func l5))
      (let [curr_node (first syntax_tree)]
        (cond (data-decls? curr_node) (recur (rest syntax_tree)
                                            (concat l1 (list (list (arg1 curr_node)
                                                          (arg2 curr_node)
                                                          (arg3 curr_node)))) l4 l5)
              (type-decl? curr_node) (recur (rest syntax_tree) l1
                                            (concat l4 (list (list (arg1 curr_node) 
                                                  (universalize-type (arg2 curr_node))))) l5)
              (def? curr_node) (recur (rest syntax_tree) l1 l4
                                      (concat l5 (list (list (arg1 curr_node)
                                        (reduce-forms (arg2 curr_node))))))
            :else (recur (rest syntax_tree) l1 l4 l5)))))))
              
                
(declare check-program-5tuple)

(defn gen-type-var [] (gensym "?t"))

(defn extendGamma [typotheses_var_type variable type] 
   (conj typotheses_var_type (list variable type)))

(defn copy-type [type_env_map type]
   (if (forall-type? type) (let [var_list (arg1 type)
                                 new_var_list (map #(get type_env_map %) var_list)
                                 ](list (first type) new_var_list (rest (rest type))))
       (throw (Exception. "The type has not be univeralised"))))

(defn lookupGamma [typotheses_var_type variable]
   (let [var_found_list (filter #(= variable (first %)) typotheses_var_type)]
     (if (empty? var_found_list) (do (print variable) 
                                     (throw (Exception. "Variable is undeclared")))
                                 (second (first var_found_list)))))

(defn generate_type_map [variable var_type] 
  (let [list_var_in_type (arg1 var_type)
        fresh_logic_vars (loop [ var_in_type list_var_in_type fresh_types '()]
                          (if (empty? var_in_type) fresh_types 
                              (recur (rest var_in_type) (concat fresh_types 
                                                                 (gen-type-var)))))
       ](hash-map variable fresh_logic_vars)))

(defn applyGamma [typotheses_var_type variable]
   (let [var_type (lookupGamma typotheses_var_type variable)]
      (if (forall-type? var_type) (copy-type (generate_type_map variable var_type) var_type)
                                   var_type)))

(declare judge-type)
(declare judge-pat-type)

(defn base-types [base_type_def] 
     (fn [typotheses exp type theta_unifer history] 
       (base_type_def)))

(def prove-int (base-types (make-integer-type)))
(def prove-string (base-types (make-string-type)))
(def prove-boolean (base-types (make-boolean-type)))
(def prove-double (base-types (make-double-type)))
(def prove-unit (base-types (make-unit-type)))
(def prove-empty (base-types (make-empty-list)))


(defn prove-variable [typotheses exp logic_var theta_unifier history]
   (let [var_type (applyGamma typotheses exp)] var_type))

(defn prove-error [typotheses exp logic_var theta_unifier history]
   (let [ fresh_var (gen-type-var)
          history_term (extend-history history (arg1 exp) fresh_var)
          new_unifier (judge-type typotheses (arg1 exp) fresh_var theta_unifier
                          history_term)
          generated_type (make-error (applyUnifier new_unifier fresh_var))
        ]
      generated_type))

(defn prove-lambda [typotheses exp logic_var theta_unifier history]
   (let [fresh_var_rhs (gen-type-var)
         param_type (applyGamma typotheses (arg1 exp)) 
         history_term (extend-history history (arg2 exp) fresh_var_rhs)
         new_unifier (judge-type typotheses (arg2 exp) fresh_var_rhs 
                               theta_unifier history_term)
         generated_type (make-arrow-type param_type 
                                         (applyUnifier new_unifier fresh_var_rhs))]
        (generated_type)))
 
(defn prove-call [typotheses exp logic_var theta_unifier history]
    (let [fresh_var_call (gen-type-var)
          fresh_var_body (gen-type-var)
          history_term (extend-history history (arg1 exp) fresh_var_call)       
          sub_unifier_1  (judge-type typotheses (arg1 exp) fresh_var_call theta_unifier 
                            history_term)
          history_term_2 (extend-history history_term (arg2 exp) fresh_var_body)
          sub_unifier_2  (judge-type typotheses (arg2 exp) fresh_var_body sub_unifier_1
                            history_term_2)
          generated_arrow_type  (applyUnifier sub_unifier_1 fresh_var_call)
          generated_param_value_type (applyUnifier sub_unifier_2 fresh_var_body)
          check_var (unifyTerm4 (arg1 generated_arrow_type) generated_param_value_type
                                  sub_unifier_2 history)
         ]
        (arg2 generated_arrow_type)))

(defn prove-tuple [typotheses exp logic_var theta_unifier history]
   (let [list_expression_type (arg1 exp)
         list_fresh_var (map #(list (gen-type-var) %) list_expression_type)
         total_unifier  (loop [judge_types list_fresh_var 
               		       threaded_unifier theta_unifier 
		               history_thread history]
            	         (if (empty? judge_types) (list threaded_unifier history_thread)
           		   (recur (rest judge_types) (judge-type typotheses (second judge_types) (first judge_types) threaded_unifier history_thread)
		              (extend-history history (second judge_types) (first judge_types)))))
         list_generated_expression_types (map #(applyUnifier total_unifier %) list_fresh_var)
         generated_tuple_type (make-tuple list_generated_expression_types)]
      generated_tuple_type))

(defn prove-case [typotheses exp logic_var theta_unifier history]
  (let [fresh_var_exp (gen-type-var)
        history_exp (extend-history history (arg1 exp) fresh_var_exp)
        unifier_exp (judge-type typotheses (arg1 exp) fresh_var_exp theta_unifier
                            history_exp)
        list_patt_exp (arg2 exp)]
        (loop [pat_exp list_patt_exp 
               next_typotheses typotheses
               next_unifier unifier_exp 
               next_history history_exp
               list_pat_exp_pairs '()]
            (if (empty? pat_exp) (make-case exp list_pat_exp_pairs)
            (let [pat (first pat_exp)
                  eqva_exp (second pat_exp)
                  type_hist (extend-history 
                                next_history pat fresh_var_exp)
                  unifier_typotheses (judge-pat-type next_typotheses 
                                        pat fresh_var_exp next_unifier type_hist)
                  pat_type (applyUnifier 
                                 (first unifier_typotheses) fresh_var_exp)
                  new_fresh_inner_var      (gen-type-var)
                  updated_history_inner_exp (extend-history type_hist
                                                eqva_exp new_fresh_inner_var) 
                  unifier_inner_exp  (judge-type next_typotheses eqva_exp
                                      new_fresh_inner_var (first unifier_typotheses) 
                                                   next_history)
                  inner_exp_type (applyUnifier unifier_inner_exp new_fresh_inner_var)
                  update_pat_exp_pairs (concat list_pat_exp_pairs 
                                             (list (list pat_type inner_exp_type)))]
               (recur (rest pat_exp) (second unifier_typotheses) 
                               unifier_inner_exp updated_history_inner_exp update_pat_exp_pairs))))))


(declare judge-pat-type)



(defn judge-const-pat [typotheses pat type-patt theta_unifier history]
   (cond (boolean? pat) (prove-boolean typotheses pat type-patt theta_unifier history)
         (integer? pat) (prove-int typotheses pat type-patt theta_unifier history) 
         (double? pat)  (prove-double typotheses pat type-patt theta_unifier history)
         (string? pat)  (prove-string typotheses pat type-patt theta_unifier history)
         (unit-pat? pat) (prove-unit typotheses pat type-patt theta_unifier history)
         (empty-list-pat? pat) (prove-empty typotheses pat type-patt theta_unifier history)
         :else (throw (Exception. "Not a constant type"))))

(defn judge-var-pat [typotheses pat type-patt theta_unifier history]
   (prove-variable typotheses pat type-patt theta_unifier history))

(defn judge-call-pat [typotheses pat type-patt theta_unifier history]
   (prove-call typotheses pat type-patt theta_unifier history))

(defn judge-tuple-pat [typotheses pat type-patt theta_unifier history]
   (prove-tuple typotheses pat type-patt theta_unifier history))


(defn judge-pat-helper [typotheses pat type-patt theta_unifier history]
  (cond (constant-pat? pat) (judge-const-pat typotheses pat type-patt theta_unifier history)
        (tuple-pat? pat)   (judge-tuple-pat  typotheses pat type-patt theta_unifier history) 
        (call-pat? pat)  (judge-call-pat  typotheses pat type-patt theta_unifier history)
        (variable? pat)   (judge-call-pat  typotheses pat type-patt theta_unifier history)
        :else (do (println pat) (throw (Exception. "This type is unknow")))))

(defn get-vars-from-pat [pattern]
   (cond (boolean? pattern) (make-boolean-type)
         (integer? pattern) (make-integer-type)
         (double? pattern) (make-double-type)
         (string? pattern) (make-double-type)
         (unit-pat? pattern) (make-unit-pat)
         (empty-list-pat? pattern) (make-empty-list-pat)
         (tuple-pat? pattern) (arg1 pattern)
         (call-pat? pattern) (get-vars-from-pat (arg2 pattern))))



(defn judge-pat-type [typotheses pat type-patt theta_unifier history]
   (let [vars_in_pat (arg1 pat)
         is_pat_linear (distinct? vars_in_pat)];no two param have the same name
         (if (not is_pat_linear) (failure history "Pattern is not linear")
           (let [type_var_bindings (map #((gen-type-var) %) vars_in_pat)
                 next_typotheses (loop [bind type_var_bindings
                                        updated_typotheses typotheses]
                                      (if (empty? bind) updated_typotheses
                                        (recur (rest bind) (extendGamma 
                                                             (second bind)
                                                             (first bind)))))
                 total_unifier (loop [bind type_var_bindings
                                      sub_unifier theta_unifier
                                      next_history history 
                                     ]
                                      (if (empty? bind) theta_unifier
                                        (recur (rest bind)
                                          (unifyTerm4 type-patt 
                                             (judge-pat-helper
                                               next_typotheses
                                               (second bind)
                                               (first bind)
                                               sub_unifier
                                               next_history)) 
                                             (extend-history next_history (second bind) 
                                                (first bind)))))]
       (list total_unifier next_typotheses)))))
             
;
;(defn judge-pat-type [typotheses pat type-patt theta_unifier history]
;   (let [free_var_in_pat (arg1 pat)
;         is_pat_linear (distinct? free_variables)
;        ]     (if (not is_pat_linear) (failure history "Pattern is not linear")
;        (let [fresh_variable_types (map #(% genvar) free_var_in_pat)
;              extend_typotheses (loop [list_bindings fresh_variable_types 
;                                       result typotheses 
;                                       next_unifier theta_unifier
;                                       next_history history] 
;                                 (if (empty? init_typo) result
;                                     (let [ new_unifier (judge-pat-type2
;                                                           result
;                                                           (first list_bindings)
;                                                           (second list_bindings)
;                                                           next_unifier
;                                                           next_history)
;                                             new_typo (extendGamma result
;                                                       (first list_bindings) 
;                                                       (second list_bindings))
;                                            
;                                           ]
;                                         (recur (rest list_bindings) result))))
;              
;        (cond (constant-pat? pat)  (judge-type typotheses 
;
;
          
(defn judge-type [typotheses exp type-from-gamma theta_unifier history]
   (cond (integer? exp) (unifyTerm4 type-from-gamma 
                              (prove-int typotheses exp 
                              (gen-type-var) theta_unifier history) theta_unifier history)
         (string? exp)  (unifyTerm4 type-from-gamma 
                              (prove-string typotheses exp (gen-type-var) 
                               theta_unifier history) theta_unifier history)
         (boolean? exp) (unifyTerm4 type-from-gamma 
                               (prove-boolean typotheses exp (gen-type-var) 
                                theta_unifier history) theta_unifier history)
         (double? exp)  (unifyTerm4 type-from-gamma 
                               (prove-double typotheses exp (gen-type-var) 
                                 theta_unifier history) theta_unifier history)
         (unit? exp)    (unifyTerm4 type-from-gamma 
                               (prove-unit typotheses exp (gen-type-var) 
                                  theta_unifier history) theta_unifier history)
         (empty-list? exp) (unifyTerm4 type-from-gamma 
                               (prove-empty typotheses exp (gen-type-var)
                                  theta_unifier history) theta_unifier history)
         (variable? exp) (unifyTerm4 type-from-gamma
                               (prove-variable typotheses exp (gen-type-var) 
                                  theta_unifier history) theta_unifier history)
         (tuple? exp)  (unifyTerm4 type-from-gamma 
                               (prove-tuple typotheses exp (gen-type-var) 
                                  theta_unifier history) theta_unifier history)
         (case? exp)   (unifyTerm4 type-from-gamma
                               (prove-case typotheses exp (gen-type-var)
                                  theta_unifier history) theta_unifier history)
         (lambda1? exp) (unifyTerm4 type-from-gamma
                                (prove-lambda typotheses exp (gen-type-var)
                                   theta_unifier history) theta_unifier history)
         (call? exp)    (unifyTerm4 type-from-gamma
                                (prove-call typotheses exp (gen-type-var)
                                   theta_unifier history) theta_unifier history)
         (error? exp)   (unifyTerm4 type-from-gamma 
                                (prove-error typotheses exp (gen-type-var) theta_unifier history) theta_unifier history)
         :else (throw (Exception. "Such an expression should not exist"))))


; TODO: Verify that any variables bound by the universal quantifier remain varibales
(defn check-program-5tuple [listFive rfive]
  (let [
        gamma (reverse (into '() (concat (arg1 listFive) (arg2 listFive) (arg3 listFive))))
        ]
    (do
      ;(println gamma)
      ;(loop [L (arg4 listFive)]
      (loop [L rfive]
        (when (not (empty? L))
        	(do
	          (println "-testing->" (first L))
	          (judge-type gamma (second (first L)) (applyGamma gamma (first (first L))) () ())
	          )
          
          (recur (rest L))))
      (println "Type check successful. "))))
(def five '(((Float () ((constructor Box (DoubleType)))) (MyList (a) ((constructor MT (UnitType)) (constructor Cons (TupleType (a (NamedType MyList a)))))) (Either (a b) ((constructor Left a) (constructor Right b)))) ((Box (forall () (ArrowType (DoubleType) (NamedType Float (UnitType))))) (MT (forall (a) (ArrowType (UnitType) (NamedType MyList a)))) (Cons (forall (a) (ArrowType (TupleType (a (NamedType MyList a))) (NamedType MyList a)))) (Left (forall (a b) (ArrowType a (NamedType Either (TupleType (a b)))))) (Right (forall (a b) (ArrowType b (NamedType Either (TupleType (a b))))))) ((*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a)))) (*==i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*!=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*<i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>=i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*>i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType)))) (*==d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*!=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*<d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>=d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*>d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (*+i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*-i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (**i* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*divi* (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (*negi* (forall () (ArrowType (IntType) (IntType)))) (*+d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*-d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (**d* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*divd* (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (*negd* (forall () (ArrowType (DoubleType) (DoubleType)))) (print (forall () (ArrowType (StringType) (UnitType))))) ((pi (forall () (DoubleType))) (arith (forall () (IntType))) (lst1 (forall () (NamedType *List* (IntType)))) (triple (forall (a) (TupleType ((UnitType) (NamedType *List* a) (NamedType *List* (IntType)))))) (fact (forall () (ArrowType (IntType) (IntType)))) (mkAdd (forall () (ArrowType (IntType) (ArrowType (IntType) (IntType))))) (seven (forall () (IntType))) (facta (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType)))) (map (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType *List* a))) (NamedType *List* b)))) (reva (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (rev2 (forall (t) (ArrowType (TupleType ((NamedType *List* t) (NamedType *List* t))) (NamedType *List* t)))) (foo (forall () (ArrowType (DoubleType) (DoubleType)))) (bar (forall (a b) (ArrowType (TupleType ((ArrowType a b) (NamedType MyList a))) (NamedType MyList b)))) (baz (forall (a b) (NamedType Either (TupleType (a b))))) (test (forall () (BoolType))) (test2 (forall () (IntType))) (test3 (forall () (BoolType))) (append (forall (a) (ArrowType (TupleType ((NamedType *List* a) (NamedType *List* a))) (NamedType *List* a)))) (curAppend (forall (a) (ArrowType (NamedType *List* a) (ArrowType (NamedType *List* a) (NamedType *List* a))))) (myAppend (forall (a) (ArrowType (TupleType ((NamedType MyList a) (NamedType MyList a))) (NamedType MyList a)))) (insert (forall () (ArrowType (TupleType ((IntType) (NamedType *List* (IntType)))) (NamedType *List* (IntType))))) (iSort (forall () (ArrowType (NamedType *List* (IntType)) (NamedType *List* (IntType))))) (abs (forall () (ArrowType (DoubleType) (DoubleType)))) (average (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (improveSqrtGuess (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (epsilon (forall () (DoubleType))) (isCloseEnough (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType)))) (sqrtIter (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType)))) (sqrt (forall () (ArrowType (DoubleType) (DoubleType)))) (quad (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (TupleType ((DoubleType) (DoubleType)))))) (maxRoot (forall () (ArrowType (TupleType ((DoubleType) (DoubleType) (DoubleType))) (DoubleType)))) (head (forall (a) (ArrowType (NamedType *List* a) a))) (head2 (forall (a) (ArrowType (NamedType *List* a) (NamedType Either (TupleType ((StringType) a))))))) ((fact (lambda1 x*1434 (case x*1434 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1)))))))))))) (mkAdd (lambda1 x*1435 (lambda1 x*1436 (case (Tuple (x*1435 x*1436)) (((TuplePat (n m)) (call *+i* (Tuple (n m))))))))) (facta (lambda1 x*1437 (case x*1437 (((TuplePat (n a)) (case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))))))))) (map (lambda1 x*1438 (case x*1438 (((TuplePat (f (EmptyList))) (EmptyList)) ((TuplePat (f (call-pat *Cons* (TuplePat (x xs))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs))))))))))) (reva (lambda1 x*1439 (case x*1439 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call reva (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (foo (lambda1 x*1440 (case x*1440 ((-2.718 1.0))))) (append (lambda1 x*1441 (case x*1441 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2)))))))))))))) (curAppend (lambda1 x*1442 (lambda1 x*1443 (case (Tuple (x*1442 x*1443)) (((TuplePat ((EmptyList) L)) L) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) L)) (call *Cons* (Tuple (x (call (call curAppend xs) L)))))))))) (myAppend (lambda1 x*1444 (case x*1444 (((TuplePat (L1 L2)) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2)))))))))))))) (insert (lambda1 x*1445 (case x*1445 (((TuplePat (x (EmptyList))) (call *Cons* (Tuple (x (EmptyList))))) ((TuplePat (x (call-pat *Cons* (TuplePat (y ys))))) (case (call *<=i* (Tuple (x y))) ((true (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys))))))) (false (call *Cons* (Tuple (y (call insert (Tuple (x ys)))))))))))))) (iSort (lambda1 x*1446 (case x*1446 (((EmptyList) (EmptyList)) ((call-pat *Cons* (TuplePat (x xs))) (call insert (Tuple (x (call iSort xs))))))))) (abs (lambda1 x*1447 (case x*1447 ((x (case (call *<d* (Tuple (x 0.0))) ((true (call *negd* x)) (false x)))))))) (average (lambda1 x*1448 (case x*1448 (((TuplePat (x y)) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0)))))))) (improveSqrtGuess (lambda1 x*1449 (case x*1449 (((TuplePat (a g)) (call average (Tuple (g (call *divd* (Tuple (a g))))))))))) (isCloseEnough (lambda1 x*1450 (case x*1450 (((TuplePat (x y)) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon)))))))) (sqrtIter (lambda1 x*1451 (case x*1451 (((TuplePat (a g)) (case (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) ((true g) (false (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g)))))))))))))) (sqrt (lambda1 x*1452 (case x*1452 ((x (call sqrtIter (Tuple (x 1.0)))))))) (quad (lambda1 x*1453 (case x*1453 (((TuplePat (a b c)) (case (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c)))))) ((e (case (call sqrt e) ((d (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a))))))))))))))))))) (maxRoot (lambda1 x*1454 (case x*1454 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1)))))))) (head (lambda1 x*1455 (case x*1455 ((L (case L (((EmptyList) (error Can't)) ((call-pat *Cons* (TuplePat (x xs))) x)))))))) (head2 (lambda1 x*1456 (case x*1456 (((EmptyList) (call Left Oops)) ((call-pat *Cons* (TuplePat (x xs))) (call Right x)))))) (pi 3.14) (arith (call *+i* (Tuple (2 (call **i* (Tuple (3 4))))))) (lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList))))))))))) (triple (Tuple ((Unit) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList))))))) (seven (call (call mkAdd 2) 5)) (rev2 (lambda1 x*1457 (case x*1457 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a))))))))))) (test (case (case (case true ((true true) (false false))) ((true true) (false true))) ((true true) (false false)))) (test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4)))) (test3 (case (case true ((true true) (false (case false ((true true) (false false)))))) ((true true) (false false)))) (epsilon 1.0E-10))))


(def rfive '(
; (fact (lambda1 x*1434 (case x*1434 ((0 1) (n (call **i* (Tuple (n (call fact (call *-i* (Tuple (n 1))))))))))))
; (mkAdd (lambda1 x*1435 (lambda1 x*1436 (case (Tuple (x*1435 x*1436)) (((TuplePat (n m)) (call *+i* (Tuple (n m)))))))))
; (facta (lambda1 x*1437 (case x*1437 (((TuplePat (n a)) (case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a))))))))))))))
; (map (lambda1 x*1438 (case x*1438 (((TuplePat (f (EmptyList))) (EmptyList)) ((TuplePat (f (call-pat *Cons* (TuplePat (x xs))))) (call *Cons* (Tuple ((call f x) (call map (Tuple (f xs)))))))))))
; (reva (lambda1 x*1439 (case x*1439 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call reva (Tuple (xs (call *Cons* (Tuple (x a)))))))))))
; (foo (lambda1 x*1440 (case x*1440 ((-2.718 1.0)))))
; (append (lambda1 x*1441 (case x*1441 (((TuplePat (L1 L2)) (case L1 (((EmptyList) L2) ((call-pat *Cons* (TuplePat (x xs))) (call *Cons* (Tuple (x (call append (Tuple (xs L2))))))))))))))
; (curAppend (lambda1 x*1442 (lambda1 x*1443 (case (Tuple (x*1442 x*1443)) (((TuplePat ((EmptyList) L)) L) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) L)) (call *Cons* (Tuple (x (call (call curAppend xs) L))))))))))
; (myAppend (lambda1 x*1444 (case x*1444 (((TuplePat (L1 L2)) (case L1 (((call-pat MT (Unit)) L2) ((call-pat Cons (TuplePat (x xs))) (call Cons (Tuple (x (call myAppend (Tuple (xs L2))))))))))))))
; (insert (lambda1 x*1445 (case x*1445 (((TuplePat (x (EmptyList))) (call *Cons* (Tuple (x (EmptyList))))) ((TuplePat (x (call-pat *Cons* (TuplePat (y ys))))) (case (call *<=i* (Tuple (x y))) ((true (call *Cons* (Tuple (x (call *Cons* (Tuple (y ys))))))) (false (call *Cons* (Tuple (y (call insert (Tuple (x ys))))))))))))))
; (iSort (lambda1 x*1446 (case x*1446 (((EmptyList) (EmptyList)) ((call-pat *Cons* (TuplePat (x xs))) (call insert (Tuple (x (call iSort xs)))))))))
; (abs (lambda1 x*1447 (case x*1447 ((x (case (call *<d* (Tuple (x 0.0))) ((true (call *negd* x)) (false x))))))))
; (average (lambda1 x*1448 (case x*1448 (((TuplePat (x y)) (call *divd* (Tuple ((call *+d* (Tuple (x y))) 2.0))))))))
; (improveSqrtGuess (lambda1 x*1449 (case x*1449 (((TuplePat (a g)) (call average (Tuple (g (call *divd* (Tuple (a g)))))))))))
; (isCloseEnough (lambda1 x*1450 (case x*1450 (((TuplePat (x y)) (call *<d* (Tuple ((call abs (call *-d* (Tuple (1.0 (call *divd* (Tuple (x y))))))) epsilon))))))))
; (sqrtIter (lambda1 x*1451 (case x*1451 (((TuplePat (a g)) (case (call isCloseEnough (Tuple ((call **d* (Tuple (g g))) a))) ((true g) (false (call sqrtIter (Tuple (a (call improveSqrtGuess (Tuple (a g))))))))))))))
; (sqrt (lambda1 x*1452 (case x*1452 ((x (call sqrtIter (Tuple (x 1.0))))))))
; (quad (lambda1 x*1453 (case x*1453 (((TuplePat (a b c)) (case (call *-d* (Tuple ((call **d* (Tuple (b b))) (call **d* (Tuple ((call **d* (Tuple (4.0 a))) c)))))) ((e (case (call sqrt e) ((d (Tuple ((call *divd* (Tuple ((call *+d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))) (call *divd* (Tuple ((call *-d* (Tuple ((call *negd* b) d))) (call **d* (Tuple (2.0 a)))))))))))))))))))
; (maxRoot (lambda1 x*1454 (case x*1454 (((TuplePat (a b c)) (case (call quad (Tuple (a b c))) (((TuplePat (r1 r2)) r1))))))))
; (head (lambda1 x*1455 (case x*1455 ((L (case L (((EmptyList) (error Can't)) ((call-pat *Cons* (TuplePat (x xs))) x))))))))
; (head2 (lambda1 x*1456 (case x*1456 (((EmptyList) (call Left Oops)) ((call-pat *Cons* (TuplePat (x xs))) (call Right x))))))
(pi 3.14)
; (error "Can't")
;(call Left Oops)
; (foo (lambda1 x*1440 (case x*1440 (2.11))))
; (Tuple (3 4))

; (arith (call *+i* (Tuple (2 (call **i* (Tuple (3 4)))))))
; (lst1 (call *Cons* (Tuple (1 (call *Cons* (Tuple (2 (call *Cons* (Tuple (3 (EmptyList)))))))))))
; (triple (Tuple ((Unit) (EmptyList) (call *Cons* (Tuple ((call *+i* (Tuple (99 1))) (EmptyList)))))))
; (seven (call (call mkAdd 2) 5))
; (rev2 (lambda1 x*1457 (case x*1457 (((TuplePat ((EmptyList) a)) a) ((TuplePat ((call-pat *Cons* (TuplePat (x xs))) a)) (call rev2 (Tuple (xs (call *Cons* (Tuple (x a)))))))))))
; (test (case (case (case true ((true true) (false false))) ((true true) (false true))) ((true true) (false false))))
; (test2 (call *+i* (Tuple ((call *+i* (Tuple ((call *+i* (Tuple (1 2))) 3))) 4))))
; (test3 (case (case true ((true true) (false (case false ((true true) (false false)))))) ((true true) (false false))))
; (epsilon 1.0E-10))
))

(check-program-5tuple five rfive)
