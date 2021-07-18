(ns proyecto-01.core-test
  (:require [clojure.test :refer :all]
            [proyecto-01.core :refer :all]))

(deftest test-pop
  (testing "Funcionalidad de POP en interpretar"
    (is (= [[['POP 1] ['TEST]] [1 6 3 4] 1 [5] []] 
           (interpretar [['POP 1] ['TEST]] [1 2 3 4] 0 [5 6] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-pfm
  (testing "Funcionalidad de PFM en interpretar"
    (is (= [[['PFM 3] ['TEST]] [1 2 3 4 5] 1 [4] []]
           (interpretar [['PFM 3] ['TEST]] [1 2 3 4 5] 0 [] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-pfi
  (testing "Funcionalidad de PFI en interpretar"
    (is (= [[['PFI 6] ['TEST]] [1 2 3 4 5] 1 [6] []]
           (interpretar [['PFI 6] ['TEST]] [1 2 3 4 5] 0 [] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-add
  (testing "Funcionalidad de ADD en interpretar"
    (is (= [[['ADD] ['TEST]] [1 2 3] 1 [9] []]
           (interpretar [['ADD] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
    (is (= [[['ADD] ['TEST]] [1 2 3] 1 [9] []]
           (interpretar [['ADD] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-sub
  (testing "Funcionalidad de SUB en interpretar"
    (is (= [[['SUB] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['SUB] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
    (is (= [[['SUB] ['TEST]] [1 2 3] 1 [-1] []]
           (interpretar [['SUB] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-mul
  (testing "Funcionalidad de MUL en interpretar"
    (is (= [[['MUL] ['TEST]] [1 2 3] 1 [20] []]
           (interpretar [['MUL] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
    (is (= [[['MUL] ['TEST]] [1 2 3] 1 [20] []]
           (interpretar [['MUL] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-div
  (testing "Funcionalidad de DIV en interpretar"
    (is (= [[['DIV] ['TEST]] [1 2 3] 1 [4] []]
           (interpretar [['DIV] ['TEST]] [1 2 3] 0 [20 5] [])
        )
    )
    (is (= [[['DIV] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['DIV] ['TEST]] [1 2 3] 0 [5 20] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-eq
  (testing "Funcionalidad de EQ en interpretar"
    (is (= [[['EQ] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['EQ] ['TEST]] [1 2 3] 0 [4 4] [])
        )
    )
    (is (= [[['EQ] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['EQ] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-neq
  (testing "Funcionalidad de NEQ en interpretar"
    (is (= [[['NEQ] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['NEQ] ['TEST]] [1 2 3] 0 [4 4] [])
        )
    )
    (is (= [[['NEQ] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['NEQ] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-gt
  (testing "Funcionalidad de GT en interpretar"
    (is (= [[['GT] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['GT] ['TEST]] [1 2 3] 0 [4 4] [])
        )
    )
    (is (= [[['GT] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['GT] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
    (is (= [[['GT] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['GT] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-gte
  (testing "Funcionalidad de GTE en interpretar"
    (is (= [[['GTE] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['GTE] ['TEST]] [1 2 3] 0 [4 4] [])
        )
    )
    (is (= [[['GTE] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['GTE] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
    (is (= [[['GTE] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['GTE] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-lt
  (testing "Funcionalidad de LT en interpretar"
    (is (= [[['LT] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['LT] ['TEST]] [1 2 3] 0 [4 4] [])
        )
    )
    (is (= [[['LT] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['LT] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
    (is (= [[['LT] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['LT] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-lte
  (testing "Funcionalidad de LTE en interpretar"
    (is (= [[['LTE] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['LTE] ['TEST]] [1 2 3] 0 [4 4] []) ;[[[LTE] [TEST]] [1 2 3] 1 [4 4] []]
        )
    )
    (is (= [[['LTE] ['TEST]] [1 2 3] 1 [1] []]
           (interpretar [['LTE] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
    (is (= [[['LTE] ['TEST]] [1 2 3] 1 [0] []]
           (interpretar [['LTE] ['TEST]] [1 2 3] 0 [5 4] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-neg
 (testing "Funcionalidad de NEG en interpretar"
   (is (= [[['NEG] ['TEST]] [1 2 3] 1 [5 -4] []]
          (interpretar [['NEG] ['TEST]] [1 2 3] 0 [5 4] [])
       )
   )
   (is (= [[['NEG] ['TEST]] [1 2 3] 1 [5 4] []]
          (interpretar [['NEG] ['TEST]] [1 2 3] 0 [5 -4] [])
       )
   )
 )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-odd
  (testing "Funcionalidad de ODD en interpretar"
    (is (= [[['ODD] ['TEST]] [1 2 3] 1 [5 1] []]
           (interpretar [['ODD] ['TEST]] [1 2 3] 0 [5 3] [])
        )
    )
    (is (= [[['ODD] ['TEST]] [1 2 3] 1 [5 0] []]
           (interpretar [['ODD] ['TEST]] [1 2 3] 0 [5 2] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-jmp
  (testing "Funcionalidad de JMP en interpretar"
    (is (= [[['JMP 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 3 [5 3] []]
           (interpretar [['JMP 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 0 [5 3] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-jc
  (testing "Funcionalidad de JC en interpretar"
    (is (= [[['JC 8] ['TEST]] [1 2 3] 1 [5] []]
           (interpretar [['JC 8] ['TEST]] [1 2 3] 0 [5 0] [])
        )
    )
    (is (= [[['JC 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 3 [4] []]
           (interpretar [['JC 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-cal
  (testing "Funcionalidad de CAL en interpretar"
    (is (= [[['CAL 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 3 [4 5] [1]]
           (interpretar [['CAL 3] ['ADD] ['ADD] ['TEST]] [1 2 3] 0 [4 5] [])
        )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-ret
  (testing "Funcionalidad de RET en interpretar"
    (is (= [[['RET] ['ADD] ['ADD] ['TEST]] [1 2 3] 3 [4 5] []]
           (interpretar [['RET] ['ADD] ['ADD] ['TEST]] [1 2 3] 0 [4 5] [3])
        )
    )
  )
)


(deftest test-a-mayusculas-salvo-strings 
	(testing "A mayusculas salvo strings"
		(is (= "  CONST Y = 2;" (a-mayusculas-salvo-strings "  const Y = 2;")))
    (is (= "  WRITELN ('Se ingresa un valor, se muestra su doble.');" (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');")))
    (is (= "X = 'x'" (a-mayusculas-salvo-strings "x = 'x'")))
    (is (= "" (a-mayusculas-salvo-strings "")))
    (is (= "WRITELN ('Una cadena') WRITELN ('OTRA cadena')" (a-mayusculas-salvo-strings "writeln ('Una cadena') WRITELN ('OTRA cadena')")))
	)
)

(deftest test-palabra-reservada? 
	(testing "Palabra reservada con diferentes llamadas"
		(is (= true (palabra-reservada? 'PROCEDURE)))
		(is (= true (palabra-reservada? "BEGIN")))
		(is (= true (palabra-reservada? 'CALL)))
	  (is (= true (palabra-reservada? 'WHILE)))
	  (is (= true (palabra-reservada? 'WRITELN)))
	  (is (= true (palabra-reservada? 'IF)))
    (is (= true (palabra-reservada? "CALL")))
    (is (= false (palabra-reservada? 'ASIGNAR)))
    (is (= false (palabra-reservada? "ASIGNAR")))
    (is (= false (palabra-reservada? "BEGINN")))
    (is (= false (palabra-reservada? 'procedure)))
    (is (= false (palabra-reservada? "")))
	)
)

(deftest test-identificador?
  (testing "Identificador con diferentes llamadas"
    (is (= false (identificador? 2)))
    (is (= true (identificador? 'V2)))
    (is (= true (identificador? "V2")))
    (is (= true (identificador? "A")))
    (is (= true (identificador? 'A)))
    (is (= true (identificador? (symbol "A3"))))
    (is (= false (identificador? "V;")))
    (is (= false (identificador? "V4#")))
    (is (= false (identificador? "2V")))
    (is (= false (identificador? (symbol "3A"))))
    (is (= false (identificador? "")))
    (is (= false (identificador? ";Z")))
    (is (= false (identificador? 'CALL)))
    (is (= false (identificador? "ODD")))
  )
)

(deftest test-cadena? 
	(testing "Cadena con diferentes llamadas"
	  (is (= true (cadena? "'Hola'")))
	  (is (= true (cadena? "''")))
	  (is (= true (cadena? "'123#ABC'")))
	  (is (= true (cadena? "'********'")))
    (is (= false (cadena? "Hola")))
    (is (= false (cadena? "Una 'cadena'")))
    (is (= false (cadena? "'Hola")))
    (is (= false (cadena? 'Hola)))
	)
)

(deftest test-ya-declarado-localmente?
	(testing "ya-declarado-localmente? con diferentes llamadas"
	   (is (= true (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= true (ya-declarado-localmente? 'Y '[[0 3] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
    (is (= true (ya-declarado-localmente? 'Z '[[0] [[Z VAR 0]]])))
    (is (= true (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))
    (is (= false (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Z '[[1] [[Z VAR 0]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[0 3 6] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[3] []])))
	)
)

(deftest test-cargar-var-en-tabla
	(testing "cargar-var-en-tabla con diferentes llamadas"
		(let [

			amb_1 '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
			amb_2 '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]
			amb_3 '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
			amb_4 '[nil () [VAR X Z] :sin-errores [[0] [[X VAR 0] [Z VAR 1]]] 2 [[JMP ?]]]
			amb_5 '[nil () [VAR Y] :error [[0] []] 0 [[JMP ?]]]
		]
		  (is (= amb_1 (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]])))
	    (is (= amb_2 (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]])))
	    (is (= amb_3 (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])))
	    (is (= amb_4 (cargar-var-en-tabla '[nil () [VAR X , Z] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])))
	    (is (= amb_5 (cargar-var-en-tabla '[nil () [VAR Y] :error [[0] []] 0 [[JMP ?]]])))
		)
	)
)

(deftest test-inicializar-contexto-local
	(testing "inicializar-contexto-local con diferentes llamadas"
		(let [
			amb_1 '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
			amb_2 '[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
			amb_3 '[nil () [] :sin-errores [[0 2] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
			amb_4 '[nil () [] :sin-errores [[0 0] []] 2 [[JMP ?]]]
		]
		  (is (= amb_1 (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
	    (is (= amb_2 (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
	    (is (= amb_3 (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]])))
	    (is (= amb_4 (inicializar-contexto-local '[nil () [] :sin-errores [[0] []] 2 [[JMP ?]]])))
		)
	)
)

(deftest test-declaracion-var
	(testing "declaracion-var con diferentes llamadas"
		(let [
			amb_1 "[VAR (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :error [[0] []] 0 [[JMP ?]]]"
			amb_2 "[BEGIN (X := 7 ; Y := 12 ; END .) [VAR X , Y ;] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]"
			amb_3 "[CONST (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :sin-errores [[0] []] 0 [[JMP ?]]]"
			amb_4 "[BEGIN (X := 7 ; Y := 12 ; Z := 20 ; END .) [VAR X , Y , Z ;] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [Z VAR 2]]] 3 [[JMP ?]]]"
		]
		  (is (= amb_1 (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]]))))
	    (is (= amb_2 (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]))))
			(is (= amb_3 (str (declaracion-var ['CONST (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]))))
			(is (= amb_4 (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ",") 'Z (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'Z (symbol ":=") 20 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]))))
		)
	)
)

(deftest test-procesar-signo-unario
	(testing "procesar-signo-unario con diferentes llamadas"
		(let [
			amb_1 "[+ (7 ; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :error [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
			amb_2 "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
			amb_3 "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := +] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
			amb_4 "[7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := -] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
			amb_5 "[* (7 ; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]"
		]
		  (is (= amb_1 (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
	    (is (= amb_2 (str (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
			(is (= amb_3 (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
	    (is (= amb_4 (str (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
	    (is (= amb_5 (str (procesar-signo-unario ['* (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
		)
	)
)

(deftest test-termino
	(testing "termino con diferentes llamadas"
		(let [
			amb_1 "[X (* 2 END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
			amb_2 "[END (.) [VAR X ; BEGIN X := X * 2] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL]]"
			amb_3 "[END (.) [VAR X ; BEGIN X := X * 2 * 4] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 4] MUL]]"
			amb_4 "[END (.) [VAR Y ; BEGIN Y := Y * 4 / 6] :sin-errores [[0] [[Y VAR 0]]] 1 [[PFM 0] [PFI 4] MUL [PFI 6] DIV]]"
		]
			(is (= amb_1 (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_2 (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_3 (str (termino ['X (list '* 2 '* 4 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_4 (str (termino ['Y (list '* 4 '/ 6 'END (symbol ".")) ['VAR 'Y (symbol ";") 'BEGIN 'Y (symbol ":=")] :sin-errores '[[0] [[Y VAR 0]]] 1 []]))))
	  )
	)
)

(deftest test-expresion
	(testing "expresion con diferentes llamadas"
		(let [
			amb_1 "[- (( X * 2 + 1 ) END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
			amb_2 "[END (.) [VAR X ; BEGIN X := + ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD]]"
			amb_3 "[END (.) [VAR X ; BEGIN X := - ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]"
			amb_4 "[END (.) [VAR X ; BEGIN X := - ( X * 2 + 4 * 3 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 4] [PFI 3] MUL ADD NEG]]"
			amb_5 "[END (.) [VAR Y ; BEGIN Y := + ( Y / 2 - 5 + 6 )] :sin-errores [[0] [[Y VAR 0]]] 1 [[PFM 0] [PFI 2] DIV [PFI 5] SUB [PFI 6] ADD]]"
		]
			(is (= amb_1 (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_2 (str (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_3 (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_4 (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 4 '* 3 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_5 (str (expresion ['+ (list (symbol "(") 'Y '/ 2 '- 5 '+ 6 (symbol ")") 'END (symbol ".")) ['VAR 'Y (symbol ";") 'BEGIN 'Y (symbol ":=")] :sin-errores '[[0] [[Y VAR 0]]] 1 []]))))
	  )
	)
)


(deftest test-aplicar-aritmetico
	(testing "aplicar-aritmetico con diferentes llamadas"
		(is (= (aplicar-aritmetico + [1 2]) [3] ))
		(is (= (aplicar-aritmetico - [1 4 1]) [1 3]))
		(is (= (aplicar-aritmetico * [1 2 4]) [1 8]))
		(is (= (aplicar-aritmetico / [1 2 4]) [1 0]))
		(is (= (aplicar-aritmetico + nil) nil))
		(is (= (aplicar-aritmetico + []) []))
		(is (= (aplicar-aritmetico + [1]) [1]))
		(is (= (aplicar-aritmetico 'hola [1 2 4]) [1 2 4]))
		(is (= (aplicar-aritmetico count [1 2 4]) [1 2 4]))
		(is (= (aplicar-aritmetico + '[a b c]) '[a b c]))
	)
)

(deftest test-aplicar-relacional
	(testing "aplicar-relacional con diferentes llamadas"
		(is (= (aplicar-relacional > [7 5]) [1] ))
		(is (= (aplicar-relacional > [4 7 5]) [4 1]))
		(is (= (aplicar-relacional = [4 7 5]) [4 0]))
		(is (= (aplicar-relacional not= [4 7 5]) [4 1]))
		(is (= (aplicar-relacional < [4 7 5]) [4 0]))
		(is (= (aplicar-relacional <= [4 6 6]) [4 1]))
		(is (= (aplicar-relacional <= '[a b c]) '[a b c]))
	)
)

(deftest test-dump
	(testing "dump con diferentes llamadas"
		(let [
			string_1 "0 [PFM 0]\r\n1 [PFI 2]\r\n2 MUL\r\n3 [PFI 1]\r\n4 ADD\r\n5 NEG\r\n"
			string_2 "0 HLT\r\n"
			string_2 "0 nil\r\n"
		]
			(is (= string_1 (with-out-str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]))))
		)
	)
)

(deftest test-generar
	(testing "generar con diferentes llamadas"
		(let [
			amb_1 "[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]]"
			amb_2 "[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]]"
			amb_3 "[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]"
			amb_4 "[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]"
		]
			(is (= amb_1 (str (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT))))
		  (is (= amb_2 (str (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0))))
		  (is (= amb_3 (str (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT))))
		  (is (= amb_4 (str (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0))))
	  )
	)
)

(deftest test-buscar-coincidencias
	(testing "buscar-coincidencias con diferentes llamadas"
		(is (= '([X VAR 0] [X VAR 2]) (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])))
		(is (= '([Y VAR 1] [Y VAR 3]) (buscar-coincidencias '[nil () [CALL Y] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])))
		(is (= '() (buscar-coincidencias '[nil () [CALL Z] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])))
	)
)

(deftest test-fixup
	(testing "fixup con diferentes llamadas"
		(let [
			amb_1 "[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]"
			amb_2 "[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]"
			amb_3 "[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]"
		]
			(is (= amb_1 (str (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1))))
		  (is (= amb_2 (str (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1))))
		  (is (= amb_3 (str (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0))))
	  )
	)
)

(deftest  test-generar-operador-relacional
	(testing "fixup con diferentes llamadas"
		(let [
			amb_1 "[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]"
			amb_2 "[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]"
			amb_3 "[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]"
			amb_4 "[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]"
		]
			(is (= amb_1 (str (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=))))
		  (is (= amb_2 (str (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+))))
		  (is (= amb_3 (str (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=))))
		  (is (= amb_4 (str (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=))))
	  )
	)
)

(deftest  test-generar-signo
	(testing "fixup con diferentes llamadas"
		(let [
			amb_1 "[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]"
			amb_2 "[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]"
			amb_3 "[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]"
			amb_4 "[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]"
			amb_5 "[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]"
		]
			(is (= amb_1 (str (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-))))
		  (is (= amb_2 (str (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+))))
		  (is (= amb_3 (str (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+))))
		  (is (= amb_4 (str (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*))))
		  (is (= amb_5 (str (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-))))
	  )
	)
)