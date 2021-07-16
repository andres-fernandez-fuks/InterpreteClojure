(ns proyecto-01.core-test
  (:require [clojure.test :refer :all]
            [proyecto-01.core :refer :all]))

(deftest test-a-mayusculas-salvo-strings 
	(testing "A mayusculas salvo strings"
		(is (= "  CONST Y = 2;" (a-mayusculas-salvo-strings "  const Y = 2;")))
    (is (= "  WRITELN ('Se ingresa un valor, se muestra su doble.');" (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');")))
	)
)

(deftest test-palabra-reservada? 
	(testing "Palabra reservada con diferentes llamadas"
	  (is (= true (palabra-reservada? 'CALL)))
    (is (= true (palabra-reservada? "CALL")))
    (is (= false (palabra-reservada? 'ASIGNAR)))
    (is (= false (palabra-reservada? "ASIGNAR")))
	)
)

(deftest test-identificador?
  (testing "Identificador con diferentes llamadas"
    (is (= false (identificador? 2)))
    (is (= true (identificador? 'V2)))
    (is (= true (identificador? "V2")))
    (is (= false (identificador? 'CALL)))
  )
)

(deftest test-cadena? 
	(testing "Cadena con diferentes llamadas"
	  (is (= true (cadena? "'Hola'")))
    (is (= false (cadena? "Hola")))
    (is (= false (cadena? "'Hola")))
    (is (= false (cadena? 'Hola)))
	)
)

(deftest test-ya-declarado-localmente?
	(testing "ya-declarado-localmente? con diferentes llamadas"
	  (is (= true (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
    (is (= true (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))
	)
)

(deftest test-cargar-var-en-tabla
	(testing "cargar-var-en-tabla con diferentes llamadas"
		(let [
			amb_1 '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
			amb_2 '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]]
			amb_3 '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
		]
		  (is (= amb_1 (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]])))
	    (is (= amb_2 (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]])))
	    (is (= amb_3 (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])))
		)
	)
)

(deftest test-inicializar-contexto-local
	(testing "inicializar-contexto-local con diferentes llamadas"
		(let [
			amb_1 '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
			amb_2 '[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
		]
		  (is (= amb_1 (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
	    (is (= amb_2 (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
		)
	)
)

(deftest test-declaracion-var
	(testing "declaracion-var con diferentes llamadas"
		(let [
			amb_1 "[VAR (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :error [[0] []] 0 [[JMP ?]]]"
			amb_2 "[BEGIN (X := 7 ; Y := 12 ; END .) [VAR X , Y ;] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]"
		]
		  (is (= amb_1 (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]]))))
	    (is (= amb_2 (str (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]))))
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
		]
		  (is (= amb_1 (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
	    (is (= amb_2 (str (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
			(is (= amb_3 (str (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
	    (is (= amb_4 (str (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))))
		)
	)
)

(deftest test-termino
	(testing "termino con diferentes llamadas"
		(let [
			amb_1 "[X (* 2 END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
			amb_2 "[END (.) [VAR X ; BEGIN X := X * 2] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL]]"
		]
			(is (= amb_1 (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_2 (str (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
	  )
	)
)

(deftest test-termino
	(testing "termino con diferentes llamadas"
		(let [
			amb_1 "[- (( X * 2 + 1 ) END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]"
			amb_2 "[END (.) [VAR X ; BEGIN X := + ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD]]"
			amb_3 "[END (.) [VAR X ; BEGIN X := - ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]"
		]
			(is (= amb_1 (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_2 (str (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
		  (is (= amb_3 (str (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []]))))
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
		(is (= (str (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])) "0 [PFM 0]\n1 [PFI 2]\n2 MUL\n3 [PFI 1]\n4 ADD\n5 NEG\nnil"))
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

(deftest buscar-coincidencias
	(testing "buscar-coincidencias con diferentes llamadas"
		(is (= (buscar-coincidencias ([X VAR 0] [X VAR 2]) '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])))
	)
)
