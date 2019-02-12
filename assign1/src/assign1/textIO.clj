(ns assign1.textIO
	(:import java.io.FileNotFoundException)
	(:use [assign1.lexer :only (make-next token-name token-value)]))
	
(defn process-filename [file-name]
	(try
		(let [content-str (slurp file-name)
			next-token (make-next content-str)]
		(println "File:")
		(println content-str)
		(println "Lexical Analysis:")
		(loop [current (next-token)]
			(if (= (token-name current) ’EOF)
				(println "Done!")
				(do (println current)
					(recur (next-token))))))
		(catch FileNotFoundException e (println "File not found."))
		(catch Exception e (println (str "A problem was found. "(str e)
			" Execution ending.")))))

(defn display-usage [args]
	(println "Usage: lein run <filename>"))