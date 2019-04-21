(ns assign5.textIO
   (:import java.io.FileNotFoundException)
   (:use [assign5.genClass :only (gen-jvm-code)]))

(defn process-filename [file-name class-name]
   (try 
      (let [content-str (slurp file-name)
            code-exp    (read-string content-str)]
         (println "File:")
         (println content-str)
         (gen-jvm-code class-name (second code-exp)))
      (catch FileNotFoundException e (println "File not found."))
      (catch Exception e (println (str "A problem was found. " 
                                       (str e) 
                                       " Execution ending.")))))

(defn display-usage [args]
   (println "Usage: lein run <file> <name>"))
