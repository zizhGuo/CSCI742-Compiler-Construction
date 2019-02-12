; Part B of HW1
(ns assign1.lexer
	(:use [assign1.genLex :only (make-lexer)]))

(def lex-spec
	'((def-eof (token "" EOF))
	(def-names
; 1
		((lower (range \a \z)); lower → a · · · z
		(upper (range \A \Z)); upper → A · · · Z
		(letter (or upper lower)); letter → lower | upper
		(digit (range \0 \9));digit → 0 · · · 9
		(idfirst (or (or letter \_) \$)); idfirst → letter | | $
		(idrest (or idfirst digit)); idrest → idfirst | digit
		(ident (seqn idfirst (* idrest))); ident → idfirst idrest*
		(digits (+ digit)); digits → digit+
		(double (seqn digits (seqn (? (seqn \. digits)) 
								   (? (seqn (or \E \e) 
								   	  		(seqn (? (seqn + -)) 
								   	  			  digits))))));double → digits(. digits)?((E | e)(+ | −) ?digits)?
		(string (seqn \" 
					  (seqn ('all-except \")
					  		\"))); string → "(Σ − {"})*"
	(def-patterns
		((+ whitespace)
		"let"
		"in"
		"="
		","
		"+"
		"-"
		"*"
		"/"
		letters
		digits))))))
(def lex-spec-actions
	'(make-ignore
	make-let-tok
	make-in-tok
	make-equal-tok
	make-comma-tok
	make-plus-tok
	make-minus-tok
	make-star-tok
	make-slash-tok
	make-id-tok
	make-intConst-tok))

(def make-next (list make-lexer lex-spec lex-spec-actions))

; existing definitions
(defn make-ignore [lexeme] false)
(defn make-let-tok [lexeme] (list 'token lexeme 'LET)); let
(defn make-in-tok [lexeme] (list 'token lexeme 'IN)); in
(defn make-equal-tok [lexeme] (list 'token lexeme 'EQ))
(defn make-comma-tok [lexeme] (list 'token lexeme 'COMMA))
(defn make-plus-tok [lexeme] (list 'token lexeme 'PLUS))
(defn make-minus-tok [lexeme] (list 'token lexeme 'MINUS))
(defn make-star-tok [lexeme] (list 'token lexeme 'STAR))
(defn make-slash-tok [lexeme] (list 'token lexeme 'SLASH))


;2
(defn make-case-tok [lexeme] (list 'token lexeme 'CASE)); case
(defn make-of-tok [lexeme] (list 'token lexeme 'OF)); of
(defn make-if-tok [lexeme] (list 'token lexeme 'IF)); if
(defn make-then-tok [lexeme] (list 'token lexeme 'THEN)); then 
(defn make-else-tok [lexeme] (list 'token lexeme 'ELSE)); else
(defn make-true-tok [lexeme] (list 'token lexeme 'BOOL true));True
(defn make-false-tok [lexeme] (list 'token lexeme 'BOOL false));false  
(defn make-error-tok [lexeme] (list 'token lexeme 'ERROR));error
(defn make-data-tok [lexeme] (list 'token lexeme 'DATA)); data
(defn make-boolean-tok [lexeme] (list 'token lexeme 'boolean)); boolean
(defn make-integer-tok [lexeme] (list 'token lexeme 'integer));integer
(defn make-double-tok [lexeme] (list 'token lexeme 'double));double
(defn make-string-tok [lexeme] (list 'token lexeme 'string));string

;3
(defn make-equal-tok [lexeme] (list 'token lexeme 'EQ))
(defn make-comma-tok [lexeme] (list 'token lexeme 'COMMA))
(defn make-plus-tok [lexeme] (list 'token lexeme 'PLUS))
(defn make-minus-tok [lexeme] (list 'token lexeme 'MINUS))
(defn make-star-tok [lexeme] (list 'token lexeme 'STAR))
(defn make-slash-tok [lexeme] (list 'token lexeme 'SLASH))
(defn make-equal2-tok [lexeme] (list 'token lexeme 'EQ2))
(defn make-notEqual-tok [lexeme] (list 'token lexeme 'NEQ))
(defn make-leq-tok [lexeme] (list 'token lexeme 'LEQ))
(defn make-less-tok [lexeme] (list 'token lexeme 'LESS))
(defn make-geq-tok [lexeme] (list 'token lexeme 'GEQ))
(defn make-greater-tok [lexeme] (list 'token lexeme 'GREAT))
(defn make-plusDouble-tok [lexeme] (list 'token lexeme 'PLUSD))
(defn make-minusDouble-tok [lexeme] (list 'token lexeme 'MINUSD))
(defn make-timeDouble-tok [lexeme] (list 'token lexeme 'TIMESD))
(defn make-divideDouble-tok [lexeme] (list 'token lexeme 'DIVIDED))
(defn make-equal2Double-tok [lexeme] (list 'token lexeme 'EQ2D))
(defn make-notEqualDouble-tok [lexeme] (list 'token lexeme 'NEQD))
(defn make-leqDouble-tok [lexeme] (list 'token lexeme 'LEQD))
(defn make-lessDouble-tok [lexeme] (list 'token lexeme 'LESSD))
(defn make-geqDouble-tok [lexeme] (list 'token lexeme 'GEQD))
(defn make-greaterDouble-tok [lexeme] (list 'token lexeme 'GREATD))
(defn make-lambda-tok [lexeme] (list 'token lexeme 'LAM))
(defn make-arrow-tok [lexeme] (list 'token lexeme 'ARROW))
(defn make-and2-tok [lexeme] (list 'token lexeme 'AND2))
(defn make-or2-tok [lexeme] (list 'token lexeme 'OR2))
(defn make-op-tok [lexeme] (list 'token lexeme 'OP))
(defn make-cp-tok [lexeme] (list 'token lexeme 'CP))
(defn make-semi-tok [lexeme] (list 'token lexeme 'SEMI))
(defn make-ob-tok [lexeme] (list 'token lexeme 'OB))
(defn make-cb-tok [lexeme] (list 'token lexeme 'CB))
(defn make-colon2-tok [lexeme] (list 'token lexeme 'COLON2))
(defn make-colon-tok [lexeme] (list 'token lexeme 'COLON))
(defn make-under-tok [lexeme] (list 'token lexeme 'UNDER))
(defn make-at-tok [lexeme] (list 'token lexeme 'AT))

;4
(defn make-id-tok [lexeme] (list 'token lexeme 'ID (symbol lexeme)))
(defn make-intConst-tok [lexeme] (list 'token lexeme 'INT (read-string lexeme)));???
(defn make-stringConst-tok [lexeme] (list 'token lexeme 'STR (read-string lexeme)));
(defn make-doubleConst-tok [lexeme] (list 'token lexeme 'DBL (read-string lexeme)));

