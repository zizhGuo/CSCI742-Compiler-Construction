(ns assign4.textIO
   (:import java.io.FileNotFoundException)
   (:use [assign4.lexer :only (make-next)])
   (:use [assign4.parser :only (parser)])
   (:use [assign4.checker :only (split-decl check-program-5tuple)])
   (:use [assign4.genIR :only (transform-5tuple gen-from-6tuple)])
   (:use [assign4.machine :only (load-and-go)])
   (:use clojure.java.io)
   (:use clojure.pprint))

(defn process-filename [file-name]
   (try 
      (let [content-str (slurp file-name)
            next-token (make-next content-str)
            tree (parser next-token)
            transformed (split-decl tree)
            transformed2 (transform-5tuple transformed)
            ipair        (gen-from-6tuple transformed2)]
	 (println "File:")
         (println content-str)
         (check-program-5tuple transformed)
         (println "Transformed2 Tree:")
         (pprint transformed2)
         (pprint ipair (writer "out.txt"))
         (print "Result: ")
         (load-and-go (second ipair) (first ipair)))
      (catch FileNotFoundException e (println "File not found."))
      (catch Exception e (println (str "A problem was found. " (str e) " Execution ending.")))))

(defn display-usage [args]
   (println "Usage: lein run <filename>"))