(declare driver-loop)
(declare escanear-arch)
(declare a-mayusculas-salvo-strings)
(declare listar)
(declare dar-error)
(declare buscar-mensaje)
(declare parsear)
(declare simb-actual)
(declare simb-no-parseados-aun)
(declare simb-ya-parseados)
(declare estado)
(declare contexto)
(declare prox-var)
(declare bytecode)
(declare escanear)
(declare palabra-reservada?)
(declare identificador?)
(declare cadena?)
(declare procesar-terminal)
(declare programa)
(declare bloque)
(declare declarar-mas-idents-igual-numero)
(declare declaracion-const)
(declare declarar-mas-idents)
(declare declaracion-var)
(declare declaraciones-procedures)
(declare procesar-mas-propos)
(declare leer-mas-idents)
(declare escribir-cadena-o-expresion)
(declare escribir-mas-cadenas-o-expresiones)
(declare procesar-writeln)
(declare proposicion)
(declare procesar-operador-relacional)
(declare condicion)
(declare procesar-signo-unario)
(declare expresion)
(declare procesar-mas-terminos)
(declare termino)
(declare procesar-mas-factores)
(declare factor)
(declare inicializar-contexto-global)
(declare inicializar-contexto-local)
(declare restaurar-contexto-anterior)
(declare ya-declarado-localmente?)
(declare controlar-duplicado)
(declare cargar-const-en-tabla)
(declare cargar-var-en-tabla)
(declare cargar-procedure-en-tabla)
(declare verificar-tipo-var)
(declare verificar-tipo-procedure)
(declare verificar-tipo-const-o-var)
(declare buscar-coincidencias)
(declare generar)
(declare generar-factor-const-o-var)
(declare generar-signo)
(declare generar-operador-relacional)
(declare generar-con-valor)
(declare fixup)
(declare fixup-bloque)
(declare interpretar)
(declare aplicar-aritmetico)
(declare aplicar-relacional)
(declare dump)

; (defn spy
;   ([x] (do (prn x) x))
;   ([msg x] (do (print msg) (print ": ") (prn x) x))
; )

(defn driver-loop
   ([]
      (prn)
      (println "Interprete de PL/0 en Clojure")
      (println "Trabajo Practico de 75.14/95.48 Lenguajes Formales - 2021")
      (prn)
      (println "Lista de comandos posibles:")
      (println "AYUDA: volver a este menu")
      (println "SALIR: volver al REPL de Clojure")
      (println "ESCAN <archivo>: mostrar los tokens de un programa escrito en PL/0")
      (println "VIRTU <archivo>: mostrar la RI de un programa escrito en PL/0")
      (println "INTER <archivo>: interpretar la RI de un programa escrito en PL/0")
      (prn)
      (driver-loop :iniciado))
   ([status]
      (print "PL/0> ") (flush)
      (try (let [linea (clojure.string/split (clojure.string/upper-case (read-line)) #" "), cabeza (first linea)]
                (cond (= cabeza "SALIR") 'CHAU
                      (= cabeza "AYUDA") (driver-loop)
                      (= cabeza "ESCAN") (let [nom (second linea)]
                                              (if (not (.exists (clojure.java.io/file nom)))
                                                  (do (print "ERROR: ") (println (buscar-mensaje 22)) (flush) (driver-loop status))
                                                  (do (listar (escanear-arch nom)) (driver-loop status))))
                      (= cabeza "VIRTU") (let [nom (second linea)]
                                              (if (not (.exists (clojure.java.io/file nom)))
                                                  (do (print "ERROR: ") (println (buscar-mensaje 22)) (flush) (driver-loop status))
                                                  (let [res (parsear (escanear-arch nom))]
                                                       (do (if (= (estado res) :sin-errores)
                                                               (dump (bytecode res)))
                                                           (driver-loop status)))))
                      (= cabeza "INTER") (let [nom (second linea)]
                                              (if (not (.exists (clojure.java.io/file nom)))
                                                  (do (print "ERROR: ") (println (buscar-mensaje 22)) (flush) (driver-loop status))
                                                  (let [res (parsear (escanear-arch nom))]
                                                       (do (if (= (estado res) :sin-errores)
                                                               (interpretar (bytecode res) (vec (repeat (prox-var res) 0)) 0 [] []))
                                                                            ; cod           mem                            cont-prg pila-dat pila-llam
                                                           (driver-loop status)))))
                      (= cabeza "") (driver-loop status)
                      :else (do (print "ERROR: ") (println (buscar-mensaje 23)) (flush) (driver-loop status))))
           (catch Exception e (println "ERROR ->" (clojure.string/trim (clojure.string/upper-case (get (Throwable->map e) :cause)))) (driver-loop status))))
)

(defn escanear-arch [nom]
      (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux)) aux (symbol %)))
            (remove empty? (with-open [rdr (clojure.java.io/reader nom)]
                                      (flatten (doall (map #(re-seq #"CONST|VAR|PROCEDURE|CALL|BEGIN|END|IF|THEN|WHILE|DO|ODD|READLN|WRITELN|WRITE|\<\=|\>\=|\<\>|\<|\>|\=|\:\=|\(|\)|\.|\,|\;|\+|\-|\*|\/|\'[^\']*\'|\d+|[A-Z][A-Z0-9]*|\!|\"|\#|\$|\%|\&|\'|\@|\?|\^|\:|\[|\\|\]|\_|\{|\||\}|\~" (a-mayusculas-salvo-strings %)) (line-seq rdr)))))))
)

(defn listar
  ([prog] (listar prog 0))
  ([prog tab]
    (if (empty? prog)
        (prn)
        (let [s1 (first prog),
              s2 (second prog)]
           (do (cond (= s1 'BEGIN) (do (prn) (print (apply str (repeat tab " "))) (println s1) (print (apply str (repeat (+ tab 2) " "))))
                     (= s2 'END) (do (println s1) (print (apply str (repeat (- tab 2) " "))))
                     (and (= s1 (symbol ";")) (not= s2 'BEGIN)) (do (println s1) (print (apply str (repeat tab " "))))
                     :else (do (print s1) (print " ")))
               (recur (rest prog) (cond (= s1 'BEGIN) (+ tab 2)
                                         (= s2 'END) (- tab 2)
                                         :else tab))))))
)

(defn dar-error [amb cod]
  (if (= (estado amb) :sin-errores)
      (do (prn)
          (println "ERROR AL INTERPRETAR EL PROGRAMA!")
          (println "*********************************")
          (prn)
          (listar (simb-ya-parseados amb))
          (prn) (println ">") (println ">>" (buscar-mensaje cod)) (println ">") (prn)
          (print (simb-actual amb)) (print " ")
          (listar (simb-no-parseados-aun amb)) (prn)
          (flush)
          [(simb-actual amb) '() (simb-ya-parseados amb) cod])
      amb)
)

(defn buscar-mensaje [cod]
  (case cod
    1 "SE ENCONTRO PREMATURAMENTE EL FIN DEL ARCHIVO:  EOF"
    2 "SE ESPERABA UN PUNTO:  ."
    3 "SE ESPERABA UNA COMA O UN PUNTO Y COMA:  , O ;"
    4 "SE ESPERABA UN PUNTO Y COMA:  ;"
    5 "SE ESPERABA UN IDENTIFICADOR"
    6 "SE ESPERABA UN IGUAL:  ="
    7 "SE ESPERABA UN NUMERO"
    8 "SE ESPERABA UNA ASIGNACION:  :="
    9 "SE ESPERABA UN PUNTO Y COMA O END:  ; O END"
   10 "SE ESPERABA UN THEN"
   11 "SE ESPERABA UN DO"
   12 "SE ESPERABA ABRIR UN PARENTESIS:  ("
   13 "SE ESPERABA CERRAR UN PARENTESIS:  )"
   14 "SE ESPERABA UN OPERADOR RELACIONAL:  =, <>, >, >=, < O <=)"
   15 "SE ESPERABA UN IDENTIFICADOR, UN NUMERO, ABRIR UN PARENTESIS O UNA CADENA"
   16 "IDENTIFICADOR DUPLICADO"
   17 "SE ESPERABA UN IDENTIFICADOR DE TIPO VARIABLE"
   18 "SE ESPERABA UN IDENTIFICADOR DE TIPO PROCEDURE"
   19 "SE ESPERABA UN IDENTIFICADOR DE TIPO CONSTANTE O VARIABLE"
   20 "IDENTIFICADOR NO DECLARADO"
   21 "ENTRADA INVALIDA. INTENTE DE NUEVO!"
   22 "ARCHIVO NO ENCONTRADO"
   23 "COMANDO DESCONOCIDO"
   cod)
)

(defn parsear [tokens]
  (let [simbolo-inicial (first tokens)]
       (if (nil? simbolo-inicial)
           (dar-error ['EOF '() [] :sin-errores] 1)
           (programa [simbolo-inicial (rest tokens) [] :sin-errores [] 0 []])))
                   ; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode]
)

(defn simb-actual [amb]
  (amb 0)
)

(defn simb-no-parseados-aun [amb]
  (amb 1)
)

(defn simb-ya-parseados [amb]
  (amb 2)
)

(defn estado [amb]
  (amb 3)
)

(defn contexto [amb]
  (amb 4)
)

(defn prox-var [amb]
  (amb 5)
)

(defn bytecode [amb]
  (amb 6)
)

(defn escanear [amb] 
  (if (= (estado amb) :sin-errores)
      [(let [simb (first (simb-no-parseados-aun amb))]
            (if (nil? simb) 'EOF simb)) (rest (simb-no-parseados-aun amb)) (conj (simb-ya-parseados amb) (simb-actual amb)) (estado amb) (contexto amb) (prox-var amb) (bytecode amb)]
      amb)
)

(defn procesar-terminal [amb x cod-err]
  (if (= (estado amb) :sin-errores)
      (if (or (and (symbol? x) (= (simb-actual amb) x)) (x (simb-actual amb)))
          (escanear amb)
          (dar-error amb cod-err))
      amb)
)

(defn controlar-duplicado [amb]
  (if (= (estado amb) :sin-errores)
      (if (ya-declarado-localmente? (last (simb-ya-parseados amb)) (contexto amb))
          (dar-error amb 16)
          amb)
      amb)
)

(defn verificar-tipo [amb control]
  (if (= (estado amb) :sin-errores)
      (let [coincidencias (buscar-coincidencias amb)]
           (if (empty? coincidencias)
               (dar-error amb 20)
               (control amb coincidencias)))
      amb)
)

(defn verificar-tipo-var [amb]
  (verificar-tipo amb #(if (not= 'VAR (second (last %2)))
                           (dar-error %1 17)
                           %1))
)

(defn verificar-tipo-procedure [amb]
  (verificar-tipo amb #(if (not= 'PROCEDURE (second (last %2)))
                           (dar-error %1 18)
                           %1))
)

(defn verificar-tipo-const-o-var [amb]
  (verificar-tipo amb #(if (not (contains? (hash-set 'CONST 'VAR) (second (last %2))))
                           (dar-error %1 19)
                           %1))
)

(defn cargar-const-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [((contexto amb) 0) (conj ((contexto amb) 1) [(last (drop-last 2 (simb-ya-parseados amb))) 'CONST (last (simb-ya-parseados amb))])])
      amb)
)

(defn cargar-procedure-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [((contexto amb) 0) (conj ((contexto amb) 1) [(last (simb-ya-parseados amb)) 'PROCEDURE (count (bytecode amb))])])
      amb)
)

(defn inicializar-contexto-global [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [[0] []])           ; [fronteras  tabla]
      amb)
)

(defn restaurar-contexto-anterior [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [(vec (butlast ((contexto amb) 0))) (subvec ((contexto amb) 1) 0 (last ((contexto amb) 0)))])
      amb)
)

(defn programa [amb]
  (-> amb
      (inicializar-contexto-global)
      (bloque)  
      (procesar-terminal ,,, (symbol ".") 2)
      (generar ,,, 'HLT))
)

(defn fixup-bloque [amb ini-bloque]
  (if (= (estado amb) :sin-errores)
      (if (not= (inc (count (bytecode ini-bloque))) (count (bytecode amb)))
          (fixup amb (count (bytecode ini-bloque)))
          (assoc amb 6 (vec (butlast (bytecode amb)))))
      amb)
)

(defn bloque [amb]
  (if (= (estado amb) :sin-errores)
      (let [ini-bloque amb]
            (-> amb
                (generar ,,, 'JMP '?)
                (declaracion-const)
                (declaracion-var)
                (declaraciones-procedures)
                (fixup-bloque ,,, ini-bloque)
                (proposicion)))
      amb)
)

(defn declarar-mas-idents-igual-numero [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 5)
              (controlar-duplicado)
              (procesar-terminal ,,, (symbol "=") 6)
              (procesar-terminal ,,, integer? 7)
              (cargar-const-en-tabla)
              (recur))
          amb)
      amb)
)

(defn declaracion-const [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'CONST)
          (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 5)
              (controlar-duplicado)
              (procesar-terminal ,,, (symbol "=") 6)
              (procesar-terminal ,,, integer? 7)
              (cargar-const-en-tabla)
              (declarar-mas-idents-igual-numero)
              (procesar-terminal ,,, (symbol ";") 3))
          amb)
      amb)
)

(defn declarar-mas-idents [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 5)
              (controlar-duplicado)
              (cargar-var-en-tabla)
              (recur))
          amb)
      amb)
)

(defn declaraciones-procedures [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'PROCEDURE)
          (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 5)
              (controlar-duplicado)
              (cargar-procedure-en-tabla)
              (procesar-terminal ,,, (symbol ";") 4)
              (inicializar-contexto-local)
              (bloque)
              (generar ,,, 'RET)
              (restaurar-contexto-anterior)
              (procesar-terminal ,,, (symbol ";") 4)
              (recur))
          amb)
      amb)
)

(defn procesar-mas-propos [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ";"))
          (-> amb
              (escanear)
              (proposicion)
              (recur))
          amb)
      amb)
)

(defn leer-mas-idents [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 5)
              (verificar-tipo-var)
              (generar-con-valor ,,, 'IN)
              (recur))
          amb)
      amb)
)

(defn escribir-cadena-o-expresion [amb]
  (if (= (estado amb) :sin-errores)
      (if (cadena? (simb-actual amb))
          (-> amb
              (generar ,,, 'OUT (simb-actual amb))
              (escanear))
          (-> amb
              (expresion)
              (generar ,,, 'OUT)))
      amb)
)

(defn escribir-mas-cadenas-o-expresiones [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (escribir-cadena-o-expresion)
              (recur))
          amb)
      amb)
)

(defn procesar-writeln [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "("))
          (-> amb
              (escanear)
              (escribir-cadena-o-expresion)
              (escribir-mas-cadenas-o-expresiones)
              (procesar-terminal ,,, (symbol ")") 13))
          amb)
      amb)
)

(defn proposicion [amb]
  (if (= (estado amb) :sin-errores)
      (if (identificador? (simb-actual amb))
          (let [primera-fase (-> amb
                                 (escanear)
                                 (verificar-tipo-var))]
              (if (= (estado primera-fase) :sin-errores)
                  (let [coincidencias (buscar-coincidencias primera-fase),
                        valor (nth (last coincidencias) 2)]
                      (-> primera-fase
                          (procesar-terminal ,,, (symbol ":=") 8)
                          (expresion)
                          (generar ,,, 'POP valor)))
                  primera-fase))
          (case (simb-actual amb) 
                CALL (-> amb
                         (escanear)
                         (procesar-terminal ,,, identificador? 5)
                         (verificar-tipo-procedure)
                         (generar-con-valor ,,, 'CAL))
               BEGIN (-> amb
                         (escanear)
                         (proposicion)
                         (procesar-mas-propos)
                         (procesar-terminal ,,, 'END 9))
                  IF (let [primera-fase (-> amb
                                            (escanear)
                                            (condicion))]
                           (if (= (estado primera-fase) :sin-errores)
                               (-> primera-fase
                                   (generar ,,, 'JC (+ 2 (count (bytecode primera-fase))))
                                   (generar ,,, 'JMP '?)
                                   (procesar-terminal ,,, 'THEN 10)
                                   (proposicion)
                                   (fixup ,,, (inc (count (bytecode primera-fase)))))
                               primera-fase))
               WHILE (let [primera-fase (-> amb
                                            (escanear))]
                           (let [segunda-fase (-> primera-fase
                                                  (condicion))]
                                 (if (= (estado segunda-fase) :sin-errores)
                                     (-> segunda-fase
                                         (generar ,,, 'JC (+ 2 (count (bytecode segunda-fase))))
                                         (generar ,,, 'JMP '?)
                                         (procesar-terminal ,,, 'DO 11)
                                         (proposicion)
                                         (generar ,,, 'JMP (count (bytecode primera-fase)))
                                         (fixup ,,, (inc (count (bytecode segunda-fase)))))
                                     segunda-fase)))
              READLN (-> amb
                         (escanear)
                         (procesar-terminal ,,, (symbol "(") 12)
                         (procesar-terminal ,,, identificador? 5)
                         (verificar-tipo-var)
                         (generar-con-valor ,,, 'IN)
                         (leer-mas-idents)
                         (procesar-terminal ,,, (symbol ")") 13))
               WRITE (-> amb
                         (escanear)
                         (procesar-terminal ,,, (symbol "(") 12)
                         (escribir-cadena-o-expresion)
                         (escribir-mas-cadenas-o-expresiones)
                         (procesar-terminal ,,, (symbol ")") 13))
             WRITELN (-> amb
                         (escanear)
                         (procesar-writeln)
                         (generar ,,, 'NL))
            amb))
      amb)
)

(defn procesar-operador-relacional [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         = (-> amb
               (escanear))
        <> (-> amb
               (escanear))
         > (-> amb
               (escanear))
        >= (-> amb
               (escanear))
         < (-> amb
               (escanear))
        <= (-> amb
               (escanear))
       (dar-error amb 14))
      amb)
)

(defn condicion [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'ODD)
          (-> amb
              (escanear)
              (expresion)
              (generar ,,, 'ODD))
          (let [primera-fase (-> amb
                                 (expresion)
                                 (procesar-operador-relacional)),
                operador-relacional (last (simb-ya-parseados primera-fase))]
               (if (= (estado primera-fase) :sin-errores)
                   (-> primera-fase
                       (expresion)
                       (generar-operador-relacional ,,, operador-relacional))
                   primera-fase)))
      amb)
)

(defn procesar-mas-terminos [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         + (-> amb
               (escanear)
               (termino)
               (generar ,,, 'ADD)
               (recur))
         - (-> amb
               (escanear)
               (termino)
               (generar ,,, 'SUB)
               (recur))
         amb)
      amb)
)

(defn procesar-mas-factores [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         * (-> amb
               (escanear)
               (factor)
               (generar ,,, 'MUL)
               (recur))
         / (-> amb
               (escanear)
               (factor)
               (generar ,,, 'DIV)
               (recur))
         amb)
      amb)
)

(defn factor [amb]
  (if (= (estado amb) :sin-errores)
      (cond
        (identificador? (simb-actual amb)) (-> amb
                                       (escanear)
                                       (verificar-tipo-const-o-var)
                                       (generar-factor-const-o-var))
        (integer? (simb-actual amb)) (let [num (simb-actual amb)]
                                          (-> amb
                                              (escanear)
                                              (generar ,,, 'PFI num)))  
        (= (simb-actual amb) (symbol "(")) (-> amb
                                               (escanear)
                                               (expresion)
                                               (procesar-terminal ,,, (symbol ")") 13))
        :else (dar-error amb 15))
      amb)
)

(defn generar-con-valor [amb instr]
  (if (= (estado amb) :sin-errores)
      (let [coincidencias (buscar-coincidencias amb),
            valor (nth (last coincidencias) 2)]
               (generar amb instr valor))
      amb)
)

(defn generar-factor-const-o-var [amb]
  (if (= (estado amb) :sin-errores)
      (let [coincidencias (buscar-coincidencias amb),
            tipo (second (last coincidencias)),
            valor (nth (last coincidencias) 2)]
           (if (= tipo 'CONST)
               (generar amb 'PFI valor)        
               (generar amb 'PFM valor)))      
      amb)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LA SIGUIENTE FUNCION DEBERA SER COMPLETADA PARA QUE ANDE EL INTERPRETE DE PL/0 
; FALTAN IMPLEMENTAR (todas como llamados recursivos a la funcion interpretar, con recur y argumentos actualizados):
;
; POP: Saca un valor de la pila de datos, lo coloca en una direccion de memoria que forma parte de la instruccion (direccionamiento directo) e incrementa el contador de programa
; PFM: Coloca en la pila de datos un valor proveniente de una direccion de memoria que forma parte de la instruccion (PUSH FROM MEMORY: direccionamiento directo) e incrementa el contador de programa 
; PFI: Coloca en la pila de datos un valor que forma parte de la instruccion (PUSH FROM INSTRUCTION: direccionamiento inmediato) e incrementa el contador de programa 
;
; ADD: Reemplaza los dos valores ubicados en el tope de la pila de datos por su suma e incrementa el contador de programa  
; SUB: Reemplaza los dos valores ubicados en el tope de la pila de datos por su resta e incrementa el contador de programa  
; MUL: Reemplaza los dos valores ubicados en el tope de la pila de datos por su producto e incrementa el contador de programa  
; DIV: Reemplaza los dos valores ubicados en el tope de la pila de datos por su cociente entero e incrementa el contador de programa  
;
; EQ : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si son iguales (si no, por 0) e incrementa el contador de programa
; NEQ: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si son distintos (si no, por 0) e incrementa el contador de programa
; GT : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es mayor que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
; GTE: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es mayor o igual que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
; LT : Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es menor que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
; LTE: Reemplaza los dos valores ubicados en el tope de la pila de datos por 1 si el valor ubicado debajo del tope es menor o igual que el ubicado en el tope (si no, por 0) e incrementa el contador de programa
;
; NEG: Le cambia el signo al valor ubicado en el tope de la pila de datos e incrementa el contador de programa
; ODD: Reemplaza el valor ubicado en el tope de la pila de datos por 1 si este es impar (si no, por 0) e incrementa el contador de programa
;
; JMP: Reemplaza el contador de programa por la direccion que forma parte de la instruccion
; JC : Saca un valor de la pila de datos y si es 0 incrementa el contador de programa (si no, reemplaza el contador de programa por la direccion que forma parte de la instruccion)
; CAL: Coloca en la pila de llamadas el valor del contador de programa incrementado en 1 y reemplaza el contador de programa por la direccion que forma parte de la instruccion
; RET: Saca una direccion de la pila de llamadas y la coloca en el contador de programa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interpretar [cod mem cont-prg pila-dat pila-llam]
  (let [fetched (cod cont-prg),
        opcode (if (symbol? fetched) fetched (first fetched))]
       (case opcode
          HLT nil
          IN (let [entr (try (Integer/parseInt (read-line)) (catch Exception e ""))]
                  (if (integer? entr)
                      (recur cod (assoc mem (second fetched) entr) (inc cont-prg) pila-dat pila-llam)
                      (do (print "ERROR: ") (println (buscar-mensaje 21)) (print "? ") (flush) (recur cod mem cont-prg pila-dat pila-llam))))
          OUT (if (symbol? fetched)
                  (do (print (last pila-dat)) (flush)
                      (recur cod mem (inc cont-prg) (vec (butlast pila-dat)) pila-llam))
                  (do (print (apply str (butlast (rest (str (second fetched)))))) (flush)
                      (recur cod mem (inc cont-prg) pila-dat pila-llam)))
          NL (do (prn) (recur cod mem (inc cont-prg) pila-dat pila-llam))
          POP (let [val (last (pila_dat)) pos (second fetched)]
                   (recur cod (assoc mem pos val) (inc cont_prg) (vec (butlast pila-dat)) pila-llam))
          PFM (let [val (nth mem (second fetched))]
                   (recur cod (conj mem val) (inc cont_prg) pila-dat pila-lam))
          PFI (let [val (second fetched)]
                   (recur cod (conj mem val) (inc cont_prg) pila-dat pila-lam))
          ADD (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (+ val1 val2) pila-dat)) pila-lam))
          SUB (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (- val1 val2) pila-dat)) pila-lam))
          MUL (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (* val1 val2) pila-dat)) pila-lam))
          DIV (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (/ val1 val2) pila-dat)) pila-lam))
          EQ (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (= val1 val2) 1 0) pila-dat)) pila-lam))
          NEQ (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod (mem (inc cont_prg) (vec (cons (if (= val1 val2) 0 1) pila-dat)) pila-lam))
          GT (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (> val1 val2) 0 1) pila-dat)) pila-lam))
          GTE (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (>= val1 val2) 0 1) pila-dat)) pila-lam))
          LT (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (< val1 val2) 0 1) pila-dat)) pila-lam))
          LTE (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (<= val1 val2) 0 1) pila-dat)) pila-lam))
          NEG (let [val (last pila_dat)]
                   (recur cod mem (inc cont_prg) (vec (cons (- val) pila-dat)) pila-lam))
          ODD (let [val1 (last pila_dat) val2 (last (butlast pila_dat))]
                   (recur cod mem (inc cont_prg) (vec (cons (if (<= val1 val2) 0 1) pila-dat)) pila-lam))
          JMP PFM (let [val (second fetched)]
                   (recur cod mem val pila-dat pila-lam))
          JC (let [val (if (= (last pila_dat) 0) (inc cont_prg) (second fetched))]
                   (recur cod mem val (butlast pila-dat) pila-lam))
          CAL (let [val (second fetched)]
                   (recur cod mem val pila-dat (conj pila-lam (inc cont_prg))))
          RET (let [val (last pila-lam)]
                   (recur cod mem val pila-dat (butlast pila-lam))
       )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LAS FUNCIONES QUE SIGUEN DEBERAN SER IMPLEMENTADAS PARA QUE ANDE EL INTERPRETE DE PL/0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe una cadena y la devuelve con sus letras convertidas a mayusculas, salvo las subcadenas contenidas entre
; apostrofos. Por ejemplo:
; user=> (a-mayusculas-salvo-strings "  const Y = 2;")
; "  CONST Y = 2;"
; user=> (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');")
; "  WRITELN ('Se ingresa un valor, se muestra su doble.');"
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn a-mayusculas-salvo-strings 
  ([s]
    (let [sec_letras (seq s)]
      (apply str (reverse (a-mayusculas-salvo-strings sec_letras true)))
    )
  )
  ([secuencia es_mayuscula]
    (if (empty? secuencia) '()
      (let [es_mayuscula_act (if (= (last secuencia) \') (not es_mayuscula) es_mayuscula)]
         (cons (if es_mayuscula (clojure.string/upper-case (last secuencia)) (last secuencia)) 
                              (a-mayusculas-salvo-strings (butlast secuencia) es_mayuscula_act))
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un dato y devuelve true si es una palabra reservada de PL/0; si no, devuelve false. Por ejemplo:
; user=> (palabra-reservada? 'CALL)
; true
; user=> (palabra-reservada? "CALL")
; true
; user=> (palabra-reservada? 'ASIGNAR)
; false
; user=> (palabra-reservada? "ASIGNAR")
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn palabra-reservada? [x]
   (let [palabras_reservadas ["CONST" "VAR" "PROCEDURE" "CALL" "BEGIN" "IF" "WHILE" "ODD" "THEN" "DO"]]
      (if (some #(= (str x) %) palabras_reservadas) true false)
   )   
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un dato y devuelve true si es un identificador valido de PL/0; si no, devuelve false. Por ejemplo:
; user=> (identificador? 2)
; false
; user=> (identificador? 'V2)
; true
; user=> (identificador? "V2")
; true
; user=> (identificador? 'CALL)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn identificador? [x]
   (cond 
   (palabra-reservada? x) false
   (symbol? x) true 
   (string? x) true
   true false
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un dato y devuelve true si es una cadena conteniendo una cadena de PL/0; si no, devuelve false. Por ejemplo:
; user=> (cadena? "'Hola'")
; true
; user=> (cadena? "Hola")
; false
; user=> (cadena? "'Hola")
; false
; user=> (cadena? 'Hola)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cadena? [x]
   (let [sec_letras (seq x)]
   (if (and (= (first sec_letras) '\') (= (last sec_letras) '\')) true false))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un identificador y un contexto (un vector formado por dos subvectores: el primero con las sucesivas
; posiciones de inicio de los distintos ambitos/scopes y el segundo con ternas [identificador, tipo, valor]
; resultantes de las declaraciones efectuadas, y devuelve true si el identificador está declarado en el segundo
; subvector a partir de la ultima posicion guardada en el primer subvector (o sea, en el ambito/scope local); si no,
; devuelve false. Por ejemplo:
; user=> (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]])
; true  
; user=> (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]])
; false  
; user=> (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])
; false
; user=> (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])
; true
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-contiene-variable [x v]
   (if (and (some #(= x %) v) (not-any? #(= 'CONST %) v)) true false)
)

(defn aux-reducir-bool [x1 x2]
   (cond
     (and (not x1) (not x2)) false
     (and x1 (not x2)) false
     true true
   )
)

(defn ya-declarado-localmente? [ident context]
  (let [v (second context)]
    (let [v_bool (map (partial aux-contiene-variable ident) v)]
       (reduce aux-reducir-bool v_bool)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, lo devuelve modificado
; con la variable declarada como terna [identificador, tipo, valor] en el segundo subvector del vector contexto y el
; contador de variables incrementado en 1. Por ejemplo:
; user=> (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]])
; [nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]

; user=> (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]])
; [nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]] ]

; user=> (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])
; [nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-convertir-var 
  ([v]
    (vec (aux-convertir-var v 0))
  )
  ([v i]
    (if (= i (count v)) []
      (let [r (aux-convertir-var v (inc i))]
        (cons [(nth v i) 'VAR i] r)
      )
    )
  )
)

(defn aux-variables [variables]
  (map aux-convertir-var variables)
)

(defn aux-nuevo-ambiente-1 [amb]
   (let [
     v1 (first amb)
     v2 (second amb)
     v3 (nth amb 2)
     v4 (nth amb 3)
     v5 (first (nth amb 4))
     v6 (aux-convertir-var (rest (nth amb 2)))
     v7 (inc (nth amb 5))
     v8 (nth amb 6)
   ]
      [v1 v2 v3 v4 [v5 v6] v7 v8])
)

(defn cargar-var-en-tabla [amb]
  (if (not (= (nth amb 3) :sin-errores)) amb (aux-nuevo-ambiente-1 amb))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, lo devuelve modificado
; con el tamano del segundo subvector del vector contexto agregado al final del primer subvector del vector contexto.
; Por ejemplo:
; user=> (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
; [nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
; user=> (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])
; [nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-nuevo-ambiente-2 [amb]
   (let [
     len_subv_2 (count (nth (nth amb 4) 1))
     v1 (first amb)
     v2 (second amb)
     v3 (nth amb 2)
     v4 (nth amb 3)
     v5 (conj (first (nth amb 4)) len_subv_2)
     v6 (nth (nth amb 4) 1)
     v7 (nth amb 5)
     v8 (nth amb 6)
   ]
      [v1 v2 v3 v4 [v5 v6] v7 v8])
)

(defn inicializar-contexto-local [amb]
   (if (not (= (nth amb 3) :sin-errores)) amb (aux-nuevo-ambiente-2 amb))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, verifica si se debe
; parsear una declaracion de variables de PL/0. Si no es asi, se devuelve el ambiente intacto. De lo contrario, se
; devuelve un nuevo ambiente con la declaracion de variables parseada (ver EBNF), las variables declaradas en el
; contexto y el contador de variables actualizado. Por ejemplo:

; user=> (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]])
; [VAR (X , Y ; BEGIN X := 7 ; Y := 12 ; END .) [] :error [[0] []] 0 [[JMP ?]]]

; user=> (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]])
; [BEGIN (X := 7 ; Y := 12 ; END .) [VAR X , Y ;] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-es-declaracion-de-var [amb]
  (= (first amb) 'VAR)
)

(defn aux-extraer-vars [amb]
  (let [
    vect_total (second amb)
    vect_red (take (.indexOf vect_total (symbol ";")) vect_total)
  ]
    (remove #(= (symbol ",") %) vect_red)
  )
)

(defn aux-extraer-vector-de-vars [amb]
  (let [
    vect_total (second amb)
    vect_red (take (inc (.indexOf vect_total (symbol ";"))) vect_total)
  ]
   (vec (cons (first amb) vect_red))
  )
)

(defn aux-extraer-valores [amb]
  (let [vect_total (second amb)]
    (rest (drop (inc (.indexOf vect_total (symbol ";"))) vect_total))
  )
)

(defn aux-nuevo-ambiente-3 [amb]
   (let [
     v1 (aux-extraer-valores amb)
     v2 (aux-extraer-vector-de-vars amb)
     v3 (nth amb 3)
     v4 (first (nth amb 4))
     vect_vars (aux-extraer-vars amb)
     v5 (aux-convertir-var vect_vars)
     v6 (+ (nth amb 5) (count vect_vars)) ; sacar_cant_variables
     v7 (nth amb (- (count amb) 1))
   ]
    ['BEGIN v1 v2 v3 [v4 v5] v6 v7])
)

(defn declaracion-var [amb]
  (cond 
    (not (= (nth amb 3) :sin-errores)) amb
    (not (aux-es-declaracion-de-var amb)) amb
    true (aux-nuevo-ambiente-3 amb)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, verifica si se debe
; parsear un signo unario (+ o -). Si no es asi, se devuelve el ambiente intacto. De lo contrario, se devuelve un
; nuevo ambiente con el signo unario parseado (ver EBNF). Por ejemplo:
; user=> (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [+ (7 ; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :error [[0] [[X VAR 0] [Y VAR 1]]] 2 []]

; user=> (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X :=] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]

; user=> (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := +] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]

; user=> (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])
; [7 (; Y := - 12 ; END .) [VAR X , Y ; BEGIN X := -] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 []]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-es-signo-unario [sym]
  (let [
    es_signo_menos (= sym (symbol "-"))
    es_signo_mas (= sym (symbol "+"))
  ]
    (or es_signo_menos es_signo_mas)
  )
)

(defn aux-nuevo-ambiente-4 [amb]
   (let [
     v1 (first (second amb))
     v2 (rest (second amb))
     v3 (conj (nth amb 2) (first amb))
     v4 (nth amb 3)
     v5 (nth amb 4)
     v6 (nth amb 5)
     v7 (nth amb 6)
   ]
    [v1 v2 v3 v4 v5 v6 v7])
)

(defn procesar-signo-unario [amb]
  (cond 
    (not (= (nth amb 3) :sin-errores)) amb
    (not (aux-es-signo-unario (first amb))) amb
    true (aux-nuevo-ambiente-4 amb)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, se devuelve un
; nuevo ambiente con el termino parseado (ver EBNF). Esta funcion no genera ninguna instruccion de la RI por si
; misma. Por ejemplo:

; user=> (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])
; [X (* 2 END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]

; user=> (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := X * 2] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-armar-v1 [amb]
  (let [vect_total (second amb)]
    (rest (drop (.indexOf vect_total 'END) vect_total))
  )
)

(defn aux-armar-vector-de-instr [v_ini fact tipo_op]
  [['PFM v_ini] ['PFI fact] tipo_op]
)

(defn aux-reductora [v x]
  (conj v x)
)


(defn aux-nuevo-ambiente-5 [amb]
   (let [
     v1 (aux-armar-v1 amb)
     x (first amb)
     op (first (second amb))
     fact (second (second amb))
     v2 (reduce aux-reductora[(nth amb 2) x op fact])
     v3 (nth amb 3)
     v4 (nth amb 4)
     v5 (nth amb 5)
     v_ini (last (last (second (nth amb 4))))
     tipo_op (if (= op (symbol "*")) 'MUL 'DIV)
     v6 (aux-armar-vector-de-instr v_ini fact tipo_op)
   ]
    ['END v1 v2 v3 v4 v5 v6])
)

(defn termino [amb]
  (if (not (= (nth amb 3) :sin-errores)) amb (aux-nuevo-ambiente-5 amb))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, se devuelve un
; nuevo ambiente con la expresion parseada (ver EBNF). A esta funcion le cabe la responsabilidad de solicitar
; la generacion de la instruccion NEG de la RI (llamando a generar-signo), si corresponde. Por ejemplo:

; user=> (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])
; [- (( X * 2 + 1 ) END .) [VAR X ; BEGIN X :=] :error [[0] [[X VAR 0]]] 1 []]

; user=> (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := + ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD]]

; user=> (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])
; [END (.) [VAR X ; BEGIN X := - ( X * 2 + 1 )] :sin-errores [[0] [[X VAR 0]]] 1 [[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-es-signo-unario [sym]
  (let [
    es_signo_menos (= sym (symbol "-"))
    es_signo_mas (= sym (symbol "+"))
  ]
    (or es_signo_menos es_signo_mas)
  )
)

(defn generar-salida-unario [unario]
  (if (= unario (symbol "+")) 'ADD 'SUB)
)

(defn aux-flatten [v]
  ;(println v)
  (if (empty? v) []
    (let [
      v_act (aux-flatten (rest v))
      elem (first v)
      ]
      ;(println v_act)
      (if (and (vector? elem) (vector? (first elem))) 
        (into elem v_act)
        (conj v_act elem) ; esto puede estar funcionando de suerte
      )
    )
  )
)

(defn generar-inst [x]
  (if (float? x)
    ['PFM (int x)]
    ['PFI x]
  )
)

(defn generar-salida-op-multi [v]
  (let [op (if (= (second v) (symbol "*")) 'MUL 'DIV)]
        [(generar-inst (first v)) (generar-inst (last v)) op]
  )
)

(defn generar-salida-operacion [v]
  (cond
  (= (count v) 1) ['PFI (first v)]
  true (generar-salida-op-multi v)
  )
)

(defn generar-salida-de-termino [termino]
  (if (aux-es-signo-unario termino) (generar-salida-unario termino)
    (generar-salida-operacion termino)
  )
)

(defn aux-sacar-variables [v]
  (let [vect_total (take (.indexOf v (symbol ";")) v)]
    (remove #(= (symbol "VAR") %) vect_total)
  )
)

(defn reemplazar-vars-por-valores [operaciones valores]
  (if (empty? operaciones) []
    (let [
      ops_actualizado (reemplazar-vars-por-valores (rest operaciones) valores)
      simb (first operaciones)
    ]
      (if (not (find valores simb)) (cons simb ops_actualizado)
      (cons (float (get valores simb)) ops_actualizado)
      )
    )
  )
)

(defn aux-vector-sin-unarios? [v]
  (and (< (.indexOf v '-) 0) (< (.indexOf v '+) 0))
)

(defn aux-indice-prox-unario [v]
  (let [
    indice_mas (.indexOf v '+)
    indice_menos (.indexOf v '-) 
    ]
    (cond
      (< indice_mas 0) indice_menos
      (< indice_menos 0) indice_mas
      (< indice_mas indice_menos) indice_mas
      true indice_menos
    )
  )
)

(defn aux-get-cant-elem [v]
  (cond
    (aux-vector-sin-unarios? v) (count v)
    true (aux-indice-prox-unario v)
  )
)

(defn dividir-en-terminos [operaciones]
  (if (empty? operaciones) []
    (let [
      simb (first operaciones)
    ]
      (if (aux-es-signo-unario simb) (cons simb (dividir-en-terminos (rest operaciones))) 
        (let [
          cant_elem (aux-get-cant-elem operaciones)
          nuevo_vector (take cant_elem operaciones)
        ]
          (cons nuevo_vector (dividir-en-terminos (drop cant_elem operaciones)))
        )
      )
    )
  )

)

(defn aux-negar-si-necesario [v negar]
  (if negar
    (conj v 'NEG)
    v
  )
)

(defn aux-quitar-parentesis [v]
  (let [v_mod (remove #(= (symbol "(") %) v)]
    (remove #(= (symbol ")") %) v_mod)
  )
)

(defn aux-get-operaciones [v]
  (take (.indexOf v 'END) v)
)

(defn aux-establecer-valores [vars valores]
  (if (empty? vars) {}
    (let [vars_actualizado (aux-establecer-valores (rest vars) (rest valores))]
      (assoc vars_actualizado (first vars) (last (first valores)))
    )
  )
)

(defn aux-armar-v1 [amb]
  (let [vect_total (second amb)]
    (rest (drop (.indexOf vect_total 'END) vect_total))
  )
)

(defn aux-armar-v2 [sym v1 v2]
  (vec (flatten [v1 (cons sym v2)]))
)

(defn aux-nuevo-ambiente-6 [amb]
   (let [
     v1 (aux-armar-v1 amb)
     operaciones (aux-get-operaciones (second amb))
     v2 (aux-armar-v2 (first amb) (nth amb 2) operaciones)
     operaciones (aux-quitar-parentesis operaciones)
     variables (aux-sacar-variables (nth amb 2))
     v3 (nth amb 3)
     v4 (nth amb 4)
     v5 (nth amb 5)
     valores (aux-establecer-valores variables (second (nth amb 4)))
     op_modif (reemplazar-vars-por-valores operaciones valores)
     vector_op (dividir-en-terminos op_modif)
     v6 (aux-flatten (vec (map generar-salida-de-termino vector_op)))
     v6 (aux-negar-si-necesario v6 (= (first amb) (symbol "-")))
     ;v_ini (last (last (second (nth amb 4))))
     ;oper_mod (assoc (vec operaciones) 1 v_ini)
     ;v6 (aux-construir-vect-final (assoc (vec operaciones) 1 v_ini))
   ]
    ['END v1 v2 v3 v4 v5 v6])
)

(defn expresion [amb]
  (if (not (= (nth amb 3) :sin-errores)) amb (aux-nuevo-ambiente-6 amb))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un operador aritmetico diadico de Clojure y un vector. Si el vector tiene mas de un elemento y si los dos
; ultimos elementos son numericos, devuelve el vector con los dos ultimos elementos reemplazados por el resultado de
; aplicar el operador con esos dos elementos. En cualquier otro caso, devuelve el vector intacto.
; user=> (aplicar-aritmetico + [1 2])
; [3]
; user=> (aplicar-aritmetico - [1 4 1])
; [1 3]
; user=> (aplicar-aritmetico * [1 2 4])
; [1 8]
; user=> (aplicar-aritmetico / [1 2 4])
; [1 0]
; user=> (aplicar-aritmetico + nil)
; nil
; user=> (aplicar-aritmetico + [])
; []
; user=> (aplicar-aritmetico + [1])
; [1]
; user=> (aplicar-aritmetico 'hola [1 2 4])
; [1 2 4]
; user=> (aplicar-aritmetico count [1 2 4])
; [1 2 4]
; user=> (aplicar-aritmetico + '[a b c])
; [a b c]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-operador-no-valido [op]
  (let [lista_ops [(str +) (str -) (str *) (str /)]]
    (< (.indexOf lista_ops (str op)) 0)
  )
)

(defn aux-tipo-dato-no-valido [pila]
  (let [v1 (last pila) v2 (last (butlast pila))]
    (not (and (integer? v1) (integer? v2)))
  )
)

(defn aplicar-operacion [op v1 v2]
  (if (= (str op) (str /))
    (quot v1 v2)
    (op v1 v2)
  )
)

(defn aux-agregar-resultado [op pila]
  (let [
    v1 (last (butlast pila))
    v2 (last pila)
    resultado (aplicar-operacion op v1 v2)
  ]
    (conj (vec (drop-last 2 pila)) resultado)
  )
)

(defn aplicar-aritmetico [op pila]
  (cond
    (aux-operador-no-valido op) pila
    (<= (count pila) 1) pila
    (aux-tipo-dato-no-valido pila) pila
    true (aux-agregar-resultado op pila)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un operador relacional de Clojure y un vector. Si el vector tiene mas de un elemento y si los dos
; ultimos elementos son numericos, devuelve el vector con los dos ultimos elementos reemplazados por el resultado de
; aplicar el operador con esos dos elementos, expresado como 0 (false) o 1 (true). En cualquier otro caso, devuelve
; el vector intacto.
; user=> (aplicar-relacional > [7 5])
; [1]
; user=> (aplicar-relacional > [4 7 5])
; [4 1]
; user=> (aplicar-relacional = [4 7 5])
; [4 0]
; user=> (aplicar-relacional not= [4 7 5])
; [4 1]
; user=> (aplicar-relacional < [4 7 5])
; [4 0]
; user=> (aplicar-relacional <= [4 6 6])
; [4 1]
; user=> (aplicar-relacional <= '[a b c])
; [a b c]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-operador-rel-no-valido [op]
  (let [lista_ops [(str =) (str <) (str >) (str <=) (str >=) (str not=)]]
    (< (.indexOf lista_ops (str op)) 0)
  )
)

(defn aux-agregar-resultado-rel [op pila]
  (let [
    v1 (last (butlast pila))
    v2 (last pila)
    resultado (if (op v1 v2) 1 0)
  ]
    (conj (vec (drop-last 2 pila)) resultado)
  )
)

(defn aplicar-relacional [op pila]
  (cond
    (aux-operador-rel-no-valido op) pila
    (<= (count pila) 1) pila
    (aux-tipo-dato-no-valido pila) pila
    true (aux-agregar-resultado-rel op pila)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un vector con instrucciones de la RI y las imprime numeradas a partir de 0. Siempre retorna nil.
; user=> (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])
; 0 [PFM 0]
; 1 [PFI 2]
; 2 MUL
; 3 [PFI 1]
; 4 ADD
; 5 NEG
; nil
; user=> (dump '[HLT])
; 0 HLT
; nil
; user=> (dump nil)
; 0 nil
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dump-rec 
  ([cod]
    (dump-rec cod 0)
  )
  ([cod i]
    (if (empty? cod)
      nil
      (do
        (println i (first cod))
        (dump-rec (rest cod) (inc i))
      )
    )  
  )
)

(defn dump [cod]
  (dump-rec cod)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Si recibe un ambiente y una instruccion de la RI, y si el estado es :sin-errores, devuelve el ambiente con la
; instruccion agregada al final del vector de bytecode. Si recibe un ambiente, una instruccion de la RI y un valor,
; y si el estado es :sin-errores, devuelve el ambiente con un vector conteniendo la instruccion y el valor, agregado
; al final del vector de bytecode. De lo contrario, devuelve el ambiente intacto. Por ejemplo:

; user=> (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT)
; [nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]]

; user=> (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0)
; [nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]]

; user=> (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT)
; [nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]

; user=> (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0)
; [nil () [VAR X] :error [[0] []] 0 [[JMP ?]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn generar 
  ([amb instr]
    (if (not (= (nth amb 3) :sin-errores)) amb
      (assoc amb 6 (conj (nth amb 6) instr))  
    )
  )
  ([amb instr val]
    (if (not (= (nth amb 3) :sin-errores))
      amb
      (assoc amb 6 (conj (nth amb 6) [instr val])) 
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y devuelve una lista con las ternas [identificador, tipo, valor] provenientes del segundo
; subvector del vector contexto que tengan como identificador al ultimo simbolo ubicado en el vector de simbolos ya
; escaneados. Por ejemplo: 
; user=> (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])
; ([X VAR 0] [X VAR 2])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-buscar-coincidencias-rec [simb v]
  (if (empty? v)
    []
    (let [
      v_act (aux-buscar-coincidencias-rec simb (rest v))
      elem (first v)
      ]
      (if (= simb (first elem))
        (cons elem v_act)
        v_act
      )
    )
  )
)

(defn buscar-coincidencias [amb]
  (let [
    simb (last (nth amb 2))
    vect_llamadas (second (nth amb 4)) 
    ]
    (aux-buscar-coincidencias-rec simb vect_llamadas )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y la ubicacion de un JMP a corregir en el vector de bytecode. Si el estado no es :sin-errores,
; devuelve el ambiente intacto. De lo contrario, lo devuelve con el JMP corregido con el tamano del vector de
; bytecode. Por ejemplo:

; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]

; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]]

; user=> (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-fixup-jump [amb ubi]
  (let [
    btc_vec (nth amb 6)
    btc_size (count btc_vec)
    btc_fixed (assoc btc_vec ubi ['JMP btc_size])
    ]
      (assoc amb 6 btc_fixed)
    )
)

(defn fixup [amb ubi]
   (if (not (= (nth amb 3) :sin-errores)) 
    amb
    (aux-fixup-jump amb ubi)
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y un operador relacional de PL/0. Si el estado no es :sin-errores o si el operador no es
; valido, devuelve el ambiente intacto. De lo contrario, devuelve el ambiente con la instruccion de la RI
; correspondiente al operador relacional agregada en el vector de bytecode. Por ejemplo: 

; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)
; [WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]

; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]]

; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]]

; user=> (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=)
; [WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-get-instr-de-operador [operador]
  (case (str operador)
    ("=") 'EQ
    ("<") 'LT
    (">") 'GT
    ("<=") 'LTE
    (">=") 'GTE
    ("<>") 'NEQ
  )
)

(defn modificar-amb [amb operador]
  (let [op_inst (aux-get-instr-de-operador operador)]
    (assoc amb 6 (conj (nth amb 6) op_inst))
  )
)

(defn aux-operador-invalido [operador]
  (let [lista_ops ['= '< '> '<= '>= (symbol "<>")]]
    (< (.indexOf lista_ops operador) 0)
  )
)

(defn aux-amb-con-errores [amb]
  (not (= (nth amb 3) :sin-errores))
)

(defn generar-operador-relacional [amb operador]
  (cond
    (aux-amb-con-errores amb) amb
    (aux-operador-invalido operador) amb
    true (modificar-amb amb operador)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y un operador monadico de signo de PL/0. Si el estado no es :sin-errores o si el operador no es
; valido, devuelve el ambiente intacto. De lo contrario, devuelve el ambiente con la instruccion de la RI
; correspondiente al operador monadico de signo agregada en el vector de bytecode. Por ejemplo:

; user=> (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)
; [nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]

; user=> (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)
; [nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]]

; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]

; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]]

; user=> (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)
; [nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aux-signo-invalido [operador]
  (not (= '- operador))
)

(defn aux-amb-con-errores [amb]
  (not (= (nth amb 3) :sin-errores))
)

(defn generar-signo [amb signo]
  (cond
    (aux-amb-con-errores amb) amb
    (aux-signo-invalido signo) amb
    true (assoc amb 6 (conj (nth amb 6) 'NEG))
  )
)

true