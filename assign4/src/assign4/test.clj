(ns assign4.test
   ;; (:import java.io.FileNotFoundException)
   (:use [assign4.genIR])
   (:use clojure.pprint))


(def cc1 '(case (call *==i* (Tuple (n 0))) ((true a) (false (call facta (Tuple ((call *-i* (Tuple (n 1))) (call **i* (Tuple (n a)))))))))
)

(println (transform-case caseexp1))
