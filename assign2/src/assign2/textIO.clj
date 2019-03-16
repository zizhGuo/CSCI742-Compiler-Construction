(ns assign2.textIO
   (:import java.io.FileNotFoundException)
   (:use [assign2.lexer :only (make-next)])
   (:use [assign2.parser :only (parser)])
   (:use clojure.pprint))

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
      (catch Exception e (println (str "A problem was found. " 
                                       (str e) 
                                       " Execution ending.")))
      ))

(defn display-usage [args]
   (println "Usage: lein run "))