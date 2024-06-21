#lang eopl

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa (expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (body)
                 (evaluar-expresion body (ambiente-vacio))))))



;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    eval-program(sllgen:make-stream-parser 
      lexica
      gramatica)))


;;DEFINICION DE LOS AMBIENTES

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (identificadores (list-of symbol?))
   (valores (list-of valor?))
   (ambiente-ex ambiente?)))

(define valor?
  (lambda (x)
    #true))

(define apply-env
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio ()
                        (eopl:error "No se encuentra la variable " sym))
      (ambiente-extendido (lsyms lvals old-env)
                          (letrec
                              (
                               (buscar-sim
                                (lambda (lsyms lvals symb)
                                  (cond
                                    [(null? lsyms) (apply-env old-env sym)]
                                    [(equal? (car lsyms) sym) (car lvals)]
                                    [else (buscar-sim (cdr lsyms) (cdr lvals) sym)]))
                                )
                               )
                            (buscar-sim lsyms lvals sym)
                            )))))

;Prueba de ambiente inicial
(define ambiente-inicial
  (ambiente-extendido '(x y z) '(1 2 3)
                      (ambiente-extendido '(a b c) '(4 5 6)
                                          (ambiente-vacio))))


;Evaluar cada una de las expresiones
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      
      (bool-exp (bool)
        (cases bool-expresion bool
          ; Expresiones booleanas
          (true-exp ()#t)  ; Valor verdadero
          (false-exp ()#f)  ; Valor falso
        ))
      
      (var-exp (id) (apply-env env id))
      (num-exp (num) (evaluar-numero-exp num))

      ;expresion primitiva numerica
      (prim-num-exp (exp1 prim exp2)
        (let ((eexp1 (evaluar-expresion exp1 env))
              (eexp2 (evaluar-expresion exp2 env)))
          (evaluar-primitiva prim eexp1 eexp2))
        )

      ; Expresión primitiva booleana
      (prim-bool-exp (prim args)
        (evaluar-booleano prim (eval-rands args env)))  ; Evaluar expresión primitiva booleana
      
     ; Expresión primitiva de cadena
      (prim-cad-exp (prim exp)
        (let ((args (eval-rands exp env)))
          (aplicar_primitiva_string prim args)))  ; Aplicar operación primitiva de cadena

      ; Expresión de cadena
      (cadena-exp (identificador Lidentifica)
        (letrec
            ((crear_string
               (lambda (lids)
                 (cond
                   [(null? lids) ""]
                   [else (string-append " " (symbol->string (car lids)) (crear_string (cdr lids)))])))
            )
          (string-append (symbol->string identificador) (crear_string Lidentifica))
          ))
      

      
      (if-exp (test-exp true-exp false-exp)
              (if (evaluar-expresion test-exp env) ;;Condicion
                  (evaluar-expresion true-exp env) ;;Si es verdad
                  (evaluar-expresion false-exp env))) ;;Si es falso

      ; Expresión de lista
      (lista-exp (Lexp)
        (eval-rands Lexp env))  ; Evaluar expresiones en la lista

      ; Expresión cons (construcción de lista)
      (cons-exp (exp1 exp2)
        (cons (eval-rand exp1 env) (eval-rand exp2 env)))

      ; Expresión primitiva de lista
      (prim-list-exp (prim exp)
        (let ((valor (eval-rand exp env)))
          (aplicar_primitiva_lista prim valor)))  ; Aplicar operación primitiva de lista

      ; Expresión while
      (while-exp (boolean_exp body_exp)
        (cond 
          [(evaluar-expresion boolean_exp env)
           (evaluar-expresion body_exp env)
           (evaluar-expresion exp env)
          ]
          [else 'void]))

      (decl-exp (declarcion_ambiente) 
        (cases var-decl declarcion_ambiente
          (let-exp (ids rands body)
                  (let ((args (eval-rands rands env)))
                    (evaluar-expresion body
                                      (ambiente-extendido ids args env))))
          
          (lvar-exp (ids rands body) "") ))

      ; Expresión de arreglo
      (array-exp (lista)
        (list->vector (eval-rands lista env)))  ; Convertir lista en vector

      ; Expresión primitiva de arreglo
      (prim-array-exp (primitiva lista_argumentos)
        (aplicar_primitiva_array primitiva (eval-rands lista_argumentos env)))  ; Aplicar operación primitiva de arreglo

      ; Expresión de lista vacía
      (empty-list-exp ()
        '())  ; Lista vacía
      
      (else "ok")
      )))
      
;)))


; funciones auxiliares para aplicar eval-expresion a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

;;Cases numeros
(define evaluar-numero-exp
  (lambda (num)
    (cases numero-exp num
      (decimal-num (num) num)
      (octal-num (num) num)
      (bin-num (num) num)
      (hex-num (num) num)
      (float-num (num) num))))


; Funciones para operaciones lógicas
(define xor-logico
  (lambda (a b)
    (or (and a (not b)) (and (not a) b))))

(define and-logico 
  (lambda (a b)
    (and a b)))

(define or-logico 
  (lambda (a b)
    (or a b)))

(define evaluar-booleano
  (lambda (op args)
    ; Función auxiliar para evaluar una lista con un operador dado
    (letrec ((aplicar-operador 
               (lambda (operador lista)
                 (if (null? (cdr lista))
                     (car lista)
                     (operador (car lista) (aplicar-operador operador (cdr lista)))))))
      ; Evaluar según el tipo de operación boolean
      (cases primitivaBooleana op 
        (and-prim () (aplicar-operador and-logico args))
        (or-prim () (aplicar-operador or-logico args))
        (not-prim () (not (car args)))
        (xor-prim () (aplicar-operador xor-logico args))
      ))))
      

;;MANEJO DE PRIMITIVAS NUMERICAS
(define evaluar-primitiva
  (lambda (prim arg1 arg2)
    (cases primitiva prim
      (sum-prim ()(+ arg1 arg2))
      (minus-prim ()(- arg1 arg2))
      (mult-prim ()(* arg1 arg2))
      (mod-prim ()(remainder arg1 arg2))
      (elevar-prim ()(expt arg1 arg2))
      (menor-prim ()(< arg1 arg2))
      (mayor-prim ()(> arg1 arg2))
      (menorigual-prim ()(or(< arg1 arg2) (equal? arg1 arg2)))
      (mayorigual-prim ()(or(> arg1 arg2) (equal? arg1 arg2)))
      (diferente-prim () (not (equal? arg1 arg2)))
      (igual-prim () (equal? arg1 arg2))
)))



; Función principal para aplicar primitivas de cadenas
(define aplicar_primitiva_string
  (lambda (prim args)
    (letrec
      (
        ; Función auxiliar para concatenar una lista de cadenas
        (concatenar
          (lambda (lista-cadenas)
            (cond
              [(null? lista-cadenas) ""]  ; Si la lista está vacía, devolver una cadena vacía
              [else (string-append (car lista-cadenas) (concatenar (cdr lista-cadenas)))]  ; Concatenar la cabeza con el resultado recursivo de la cola
            ))))

      ; Aplicar la primitiva de cadena adecuada
      (cases primitivaCadena prim
        (concat-primCad () (concatenar args))  ; Concatenar todas las cadenas en args
        (length-primCad () (string-length (car args)))  ; Obtener la longitud de la primera cadena en args
        (index-primCad () (string (string-ref (car args) (cadr args))))  ; Obtener el carácter en el índice especificado
      ))))

; Función principal para aplicar primitivas de listas
(define aplicar_primitiva_lista
  (lambda (prim arg)
    (cases primitivaListas prim
      ; Devuelve el primer elemento de la lista
      (first-primList () (car arg))
      ; Devuelve el resto de la lista excluyendo el primer elemento
      (rest-primList () (cdr arg))
      ; Verifica si la lista está vacía
      (empty-primList () (null? arg))
    )
  )
)

; Función principal para aplicar primitivas de arreglos
(define aplicar_primitiva_array
  (lambda (prim arg)
    (letrec
      (
        ; Función auxiliar para obtener un subvector de un vector dado un rango
        (subvector
          (lambda (vect inicio final)
            (cond
              [(> inicio final) '()]
              [else (cons (vector-ref vect inicio) (subvector vect (+ inicio 1) final))]
            )
          )
        )
      ) 
      ; Aplicar la primitiva de arreglo adecuada
      (cases primitivaArray prim
        ; Obtener el elemento en un índice específico del vector
        (index-primArr () 
          (vector-ref (car arg) (cadr arg)))
        ; Obtener la longitud del vector
        (length-primArr () 
          (vector-length (car arg)))
        ; Obtener un subvector dado un rango
        (slice-primArr () 
          (list->vector (subvector (car arg) (cadr arg) (caddr arg))))
        ; Establecer un valor en un índice específico del vector y devolver el vector
        (setlist-primArr () 
          (vector-set! (car arg) (cadr arg) (caddr arg))
          (car arg))
      )
    )
  )
)


(interpretador)

      
