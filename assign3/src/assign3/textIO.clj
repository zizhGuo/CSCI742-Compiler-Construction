(ns assign3.textIO
   (:import java.io.FileNotFoundException)
   (:use [assign3.lexer :only (make-next)])
   (:use [assign3.parser :only (parser)])
   ;(:use [assign3.checker :only (split-decl check-program-5tuple free-variables-from-type)])
   (:use clojure.pprint))

; (def tree '(decl-type pi (DoubleType)))

; (print (free-variables-from-type tree))

; (defn process-filename [file-name]
;    (try
;       (let [content-str (slurp file-name)
;             next-token (make-next content-str)
;             tree (parser next-token)
;             transformed (split-decl tree)]
;          (println "File:")
;          (println content-str)
;          (println "Transformed Tree:")
;          (pprint transformed)
;          (check-program-5tuple transformed))
;       (catch FileNotFoundException e (println "File not found."))
;       (catch Exception e (println (str "A problem was found. " 
;                                        (str e) 
;                                        " Execution ending.")))))

; (defn display-usage [args]
;    (println "Usage: lein run "))

; to print the tree.
(defn process-filename [file-name]
   (try 
      (let [content-str (slurp file-name)
            next-token (make-next content-str)
            tree (parser next-token)]
         (println "File:")
         (println content-str)
         (println "Parse Tree:")
         (println tree))
      (catch FileNotFoundException e (println "File not found."))
      ; (catch Exception e (println (str "A problem was found. " 
      ;                                  (str e) 
      ;                                  " Execution ending.")))
      ))

(defn display-usage [args]
   (println "Usage: lein run "))