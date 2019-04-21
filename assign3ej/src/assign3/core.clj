(ns assign3.core
  (:gen-class)
  (:use [assign3.textIO :only (process-filename display-usage)]))

(defn -main
  "Compiler entry point."
  [& args]
   (if (= (count args) 1)
       (process-filename (first args))
       (display-usage args)))

