(ns assign4.checker
	(:use [assign4.parser])
	(:use [assign4.unification :only (unifyTerm4 
		failure
		extend-history
		theta-identity
		logic-variable?
		applyUnifier
		unifyTerm2)])
	(:use [assign4.unification :only (extendUnifier)])
	)
(defn checkMain [gramma l5] 
	(let [
			mains (filter #(= (first %) 'main) l5)
		]
		(if (empty? mains)
			(throw (Exception. "No Main method!")))
			(unifyTerm4 
				(applyGamma gramma (first (first mains)))
				(make-arrow-type (make-unit-type) (gen-type-var))
				theta-identity 
				()
			)
		)
)
;3e

(defn check-program-5tuple [listFive]
  (let [
        gamma (reverse (into '() (concat (arg1 listFive) (arg2 listFive) (arg3 listFive))))
        ]
    (do
      ;(println gamma)
      ;(loop [L (arg4 listFive)]
      (checkMain (arg4 listFive)
      (loop [L]
        (when (not (empty? L))
        	(do
	          ;(println "-testing->" (first L))
	          (judge-type gamma (second (first L)) (applyGamma gamma (first (first L))) theta-identity ())
	          ;(judge-type gamma (second (first L)) (applyGamma gamma (first (first L))) () ())
	          )
          
          (recur (rest L))))
      (println "Type check successful. "))))



