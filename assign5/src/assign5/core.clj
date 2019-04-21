(ns assign5.core
   (:gen-class)
   (:use [assign5.textIO :only (process-filename display-usage)]))

(defn -main
   "JVM gen entry point."
   [& args]
    (if (= (count args) 2)
        (process-filename (first args) (second args))
        (display-usage args)))