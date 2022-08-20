(ns tp.core
  (:gen-class))

(require '[clojure.string :refer [blank? ends-with? lower-case]] '[clojure.java.io :refer [reader]])

(defn spy
  ([x] (do (prn x) (prn) x))
  ([msg x] (do (print msg) (print ": ") (prn x) (prn) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-de)
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-setq)
(declare evaluar-quote)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-ge)
(declare fnc-gt)
(declare fnc-lt)
(declare fnc-add)
(declare fnc-env)
(declare fnc-not)
(declare fnc-sub)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-null)
(declare fnc-read)
(declare fnc-rest)
(declare fnc-equal)
(declare fnc-first)
(declare fnc-listp)
(declare fnc-prin3)
(declare fnc-append)
(declare fnc-length)
(declare fnc-terpri)
(declare fnc-reverse)

; Funciones auxiliares
(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare actualizar-amb)
(declare controlar-aridad)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-en-cond)
(declare evaluar-secuencia-en-cond)

(defn -main []
  (repl))

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de TLC-LISP."
  ([]
   (println "Interprete de TLC-LISP en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2022")
   (println "Inspirado en:")
   (println "  TLC-LISP Version 1.51 for the IBM Personal Computer")
   (println "  Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
   (repl '(add add append append cond cond cons cons de de env env equal equal
               eval eval exit exit first first ge ge gt gt if if lambda lambda
               length length list list listp listp load load lt lt nil nil
               not not null null or or prin3 prin3 quote quote read read
               rest rest reverse reverse setq setq sub sub t t terpri terpri
               + add - sub)))
  ([amb]
   (print ">>> ") (flush)
   (try
     (let [res (evaluar (read) amb nil)]  ; READ, EVAL
       (if (nil? (second res))
           true
           (do (imprimir (first res))     ; PRINT
               (repl (second res)))))     ; LOOP
   (catch Exception e
     (println) (print "*error* ")
     (println (get (Throwable->map e) :cause))
     (repl amb)))))

(defn evaluar
  "Evalua una expresion 'expre' en los ambientes global y local. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb-global amb-local]
  (if (or (igual? expre nil)
          (and (seq? expre)
                (or (empty? expre) (error? expre)))) ; si 'expre' es nil, () o error, devolverla intacta
      (list expre amb-global)                       ; de lo contrario, evaluarla
      (cond
        (not (seq? expre))             (evaluar-escalar expre amb-global amb-local)

        (igual? (first expre) 'cond)   (evaluar-cond expre amb-global amb-local)
        (igual? (first expre) 'de)     (evaluar-de expre amb-global)
          ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada aqui
          ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
        (igual? (first expre) 'eval)     (evaluar-eval expre amb-global amb-local)
        (igual? (first expre) 'exit)     (evaluar-exit expre amb-global amb-local)
        (igual? (first expre) 'if)     (evaluar-if expre amb-global amb-local)
        (igual? (first expre) 'lambda)     (evaluar-lambda expre amb-global amb-local)
        (igual? (first expre) 'load)     (evaluar-load expre amb-global amb-local)
        (igual? (first expre) 'or)     (evaluar-or expre amb-global amb-local)
        (igual? (first expre) 'quote)     (evaluar-quote expre amb-global amb-local)
        (igual? (first expre) 'setq)     (evaluar-setq expre amb-global amb-local)
          ;
        :else (let [res-eval-1 (evaluar (first expre) amb-global amb-local),
                        res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x) amb-local)] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
                        (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2) amb-local)))))


; Evalua una macro COND. Siempre devuelve una lista con un resultado y un ambiente.
(defn evaluar-cond [expre amb-global amb-local]
  "Evalua una forma 'cond' en TLC-LISP."
    (evaluar-clausulas-en-cond (next expre) amb-global amb-local))


(defn evaluar-clausulas-en-cond [expre amb-global amb-local]
  "Une 'evaluar-cond' con 'evaluar-secuencia-en-cond'."
  (if (nil? expre)
      (list nil amb-global)
        (let [res-eval (evaluar (ffirst expre) amb-global amb-local)]
            (cond
              (error? (first res-eval)) res-eval
              (not (igual? (first res-eval) nil)) (evaluar-secuencia-en-cond (nfirst expre) (second res-eval) amb-local)
              :else (recur (next expre) (second res-eval) amb-local)))))


; Evalua (con evaluar) secuencialmente las sublistas de una lista y devuelve el valor de la ultima evaluacion.
; Si alguna evaluacion devuelve un error, sera la ultima que se evalue. 
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
  (if (nil? (next lis))
      (evaluar (first lis) amb-global amb-local)
      (let [res-eval (evaluar (first lis) amb-global amb-local)]
            (if (error? (first res-eval))
                res-eval
                (recur (next lis) (second res-eval) amb-local)))))


(defn evaluar-eval
  "Evalua una forma 'eval' en TLC-LISP."
  [expre amb-global amb-local]
  (let [ari (controlar-aridad (next expre) 1)]
          (cond
            (seq? ari) ari
          (and (seq? (second expre)) (igual? (first (second expre)) 'quote)) (evaluar (second (second expre)) amb-global amb-local)
          :else (evaluar (second expre) amb-global amb-local))))


(defn evaluar-exit
  "Sale del interprete de TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list nil nil)
    :else (list (list '*error* 'too-many-args) amb-global)))


(defn evaluar-lambda
  "Evalua una forma 'lambda' en TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
    (and (not (igual? (second expre) nil)) (not (seq? (second expre)))) 
      (list (list '*error* 'list 'expected (second expre)) amb-global)
    :else (list expre amb-global)))

(defn evaluar-load
  "Evalua una forma 'load' en TLC-LISP. Carga en el ambiente un archivo 'expre' con código en TLC-LISP."
  [expre amb-global amb-local]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
        (> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
        :else (list \space (cargar-arch amb-global amb-local (second expre)))))


(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (error? nomb)
            (do (imprimir nomb) amb-global) 
          (let [nm (clojure.string/lower-case (str nomb)),
                nom (if (and (> (count nm) 4) (clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
                ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                            (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                                    (cargar-arch (second res) nil in res))
                                                          (catch Exception e (imprimir nil) amb-global))))
                        (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
                  ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (second res) nil in res))
    (catch Exception e (imprimir (first res)) amb-global)))
)


(defn evaluar-quote
  "Evalua una forma 'quote' de TLC-LISP."
  [expre amb-global _]
  (if (igual? (second expre) nil)
    (list nil amb-global)
    (list (second expre) amb-global)))


(defn aplicar
  "Aplica a la lista de argumentos 'lae' la función 'fnc' en los ambientes dados."
  ([fnc lae amb-global amb-local]
    (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb-global amb-local))
  ([resu1 resu2 fnc lae amb-global amb-local]
    (cond
      (error? resu1) (list resu1 amb-global)
      (error? resu2) (list resu2 amb-global)
      (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb-global amb-local) amb-global)
      :else (aplicar-lambda fnc lae amb-global amb-local))))


(defn aplicar-lambda
  "Aplica la forma lambda 'fnc' a la lista de argumentos 'lae'."
  [fnc lae amb-global amb-local]
  (cond
    (< (count lae) (count (second fnc))) (list '(*error* too-few-args) amb-global)
    (> (count lae) (count (second fnc))) (list '(*error* too-many-args) amb-global)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb-global amb-local)
    :else (aplicar-lambda-multiple fnc lae amb-global amb-local)))

(defn aplicar-lambda-simple
  "Evalua una forma lambda 'fnc' con un cuerpo simple."
  [fnc lae amb-global amb-local]
  (let [lista-params-args (reduce concat (map list (second fnc) lae)),
        nuevo-amb-local (cond
                          (empty? amb-local) lista-params-args
                          (empty? lista-params-args) amb-local
                          :else (apply concat (apply assoc (apply assoc {} amb-local) lista-params-args)))]
    (evaluar (first (nnext fnc)) amb-global nuevo-amb-local)))


(defn aplicar-lambda-multiple
  "Evalua una forma lambda 'fnc' cuyo cuerpo contiene varias expresiones."
  [fnc lae amb-global amb-local]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
            lae
            (second (aplicar-lambda-simple fnc lae amb-global amb-local))  ; Nuevo ambiente global
            amb-local))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una 'lae' (lista de argumentos evaluados)."
  [fnc lae amb-global amb-local]
  (cond
    (igual? fnc 'add)     (fnc-add lae)
    ; Las funciones primitivas reciben argumentos y retornan un valor (son puras)
    (igual? fnc 'append) (fnc-append lae)
    (igual? fnc 'cons) (fnc-cons lae)
    (igual? fnc 'env) (fnc-env lae amb-global amb-local)
    (igual? fnc 'equal) (fnc-equal lae)
    (igual? fnc 'first) (fnc-first lae)
    (igual? fnc 'ge) (fnc-ge lae)
    (igual? fnc 'gt) (fnc-gt lae)
    (igual? fnc 'length) (fnc-length lae)
    (igual? fnc 'list) (fnc-list lae)
    (igual? fnc 'listp) (fnc-listp lae)
    (igual? fnc 'lt) (fnc-lt lae)
    (igual? fnc 'not) (fnc-not lae)
    (igual? fnc 'null) (fnc-null lae)
    (igual? fnc 'prin3) (fnc-prin3 lae)
    (igual? fnc 'read) (fnc-read lae)
    (igual? fnc 'rest) (fnc-rest lae)
    (igual? fnc 'reverse) (fnc-reverse lae)
    (igual? fnc 'sub) (fnc-sub lae)
    (igual? fnc 'terpri) (fnc-terpri lae)
    ;
    :else (list '*error* 'non-applicable-type fnc)))

(defn fnc-cons
  "Devuelve la inserción de un elem en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad lae 2)]
        (cond
          (seq? ari) ari
        (or (seq? (second lae)) (igual? (second lae) nil)) (cons (first lae) (second lae))
          :else (list '*error* 'not-implemented))))


(defn fnc-first
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (ffirst lae))))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (or (seq? (first lae)) (igual? (first lae) nil)) (count (first lae))
      :else (list '*error* 'arg-wrong-type (first lae)))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1) nil lae))


(defn fnc-listp
  "Devuelve 't' si un elemento es una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (seq? (first lae)) 't
      :else nil)))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
          (igual? (first lae) nil) 't
            :else nil)))


(defn fnc-null
  "Devuelve 't' si un elemento es 'nil' en TLC-Lisp."
  [lae]
  (fnc-not lae))


(defn fnc-prin3
  "Imprime un elemento y lo devuelve."
  [lae]
  (cond
    (< (count lae) 1) (list '*error* 'too-few-args)
        (> (count lae) 1) (list '*error* 'not-implemented)
        (not (seq? (first lae))) (do (print (first lae)) (flush) (first lae))
        :else (do (print (map #(if (igual? % nil) nil %) (first lae))) (flush) (first lae))))


(defn fnc-rest
  "Devuelve una lista sin su 1ra. posición."
  [lae]
  (let [ari (controlar-aridad lae 1)]
        (cond
          (seq? ari) ari
            (igual? (first lae) nil) nil
            (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
            :else (nfirst lae))))


(defn imprimir
    "Imprime, con un salto de linea al final, lo recibido devolviendo 
    el mismo valor. Tambien muestra los errores."
    ([elem]
      (cond
        (not (seq? elem)) (if (igual? elem \space)
                              (do (flush) elem)
                              (do (prn (if (igual? elem nil) nil elem)) (flush) elem))
        (error? elem) (imprimir elem elem)
        :else (do (prn (map #(if (igual? % nil) nil %) elem)) (flush) elem)))
    ([lis orig]
      (if (nil? lis)
            (do (prn) (flush) orig)
            (do (pr (first lis)) (print " ") (imprimir (next lis) orig)))))

; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE TLC-LISP (ADEMAS DE COMPLETAR 'EVALUAR' Y 'APLICAR-FUNCION-PRIMITIVA'):

;"Si la longitud de una lista dada es la esperada, devuelve esa longitud.
; Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
(defn controlar-aridad [lista elems]
  (cond
    (= (count lista) elems) elems
    (> (count lista) elems) (list '*error* 'too-many-args)
    :else (list '*error* 'too-few-args)
  )
)

; Devuelve true si elem es equivalente a nil segun TLC-LISP
(defn equiv-a-nill? [elem]
  (or (nil? elem) (= () elem) (= 'NIL elem))
)

; "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
(defn igual? [a b]
  (cond
    (and (number? a) (number? b)) (= a b)
    (and (symbol? a) (symbol? b)) (= (clojure.string/lower-case a) (clojure.string/lower-case b))
    (and (string? a) (string? b)) (= a b)
    (and (char? a) (char? b)) (= a b)
    (and (seq? a) (seq? b)) (if (= (count a) (count b)) (every? true? (map igual? a b)) false)
    (and (equiv-a-nill? a) (equiv-a-nill? b)) true
    :else false
  )
)

; "Devuelve true o false, segun sea o no el arg. un mensaje de error (una lista con *error* como primer elemento)."
(defn error? [arg]
  (cond
    (not (seq? arg)) false
    (seq? (first arg)) (error? (first arg))
    :else (igual? (first arg) '*error*)
  )
)

; "Si la lista es un mensaje de error, lo devuelve; si no, devuelve nil."
(defn revisar-fnc [lista]
  (cond
    (error? lista) lista
    :else nil
  )
)

; "Devuelve el primer elemento que es un mensaje de error. Si no hay ninguno, devuelve nil."
(defn revisar-lae [lista]
  (cond
    (empty? lista) nil
    (error? (first lista)) (first lista)
    :else (revisar-lae (rest lista)) 
  )
)

; Devuelve una lista de pares clave-valor con la clave correspondiente actualizada al nuevo valor
(defn actualizar-amb-aux [amb clave valor]
  (cond 
    (empty? amb) amb
    (igual? clave (ffirst amb)) (cons (list (ffirst amb) valor) (rest amb))
    :else (cons (first amb) (actualizar-amb-aux (rest amb) clave valor))
  )
)

; "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
; Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza el valor."
(defn actualizar-amb [amb clave valor]
  (cond
    (error? valor) amb
    (error? (buscar clave amb)) (concat amb (list clave valor))
    :else (reduce concat (actualizar-amb-aux (partition 2 amb) clave valor))
  )
)

; Busca una clave en una lista de pares clave-valor
(defn buscar-aux [clave lista]
  (cond
    (empty? lista) (list '*error* 'no-encontrado)
    (igual? clave (ffirst lista)) (second (first lista))
    :else (buscar-aux clave (rest lista))
  )
)

; "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
; y devuelve el valor asociado. Devuelve un mensaje de error si no la encuentra."
(defn buscar [clave amb]
  (let [res (buscar-aux clave (partition 2 amb))] 
    (cond
      (error? res) (list '*error* 'unbound-symbol clave)
      :else res
    )
  )
)

; "Devuelve el resultado de fusionar 2 sublistas."
(defn fnc-append [lae]
  (let [res-aridad (controlar-aridad lae 2)]
    (cond 
      (error? res-aridad) res-aridad
      (and (equiv-a-nill? (first lae)) (equiv-a-nill? (second lae))) nil
      (equiv-a-nill? (second lae)) (first lae)
      (equiv-a-nill? (first lae)) (second lae)
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      (not (seq? (second lae))) (list '*error* 'list 'expected (second lae))
      :else (concat (first lae) (second lae))
    )
  )
)

; "Devuelve la fusion de los ambientes global y local."
(defn fnc-env [lae global local]
  (cond
    (not (empty? lae)) (list '*error* 'too-many-args)
    :else (fnc-append (list global local))
  )
)

; "Compara 2 elementos. Si son iguales, devuelve t. Si no, nil."
(defn fnc-equal [lae]
  (let [res-aridad (controlar-aridad lae 2)]
    (cond
      (error? res-aridad) res-aridad
      (igual? (first lae) (second lae)) 't
      :else nil
    )
  )
)

; "Devuelve la lectura de un elemento de TLC-LISP desde la terminal/consola."
(defn fnc-read [lae]
  (cond
    (error? (controlar-aridad lae 0)) (list '*error* 'not-implemented)
    :else (let [n (read)]
            (cond 
              (equiv-a-nill? n) nil
              :else n
            )
          )
  )
)

; "Imprime un salto de línea y devuelve nil."
(defn fnc-terpri [lae]
  (cond
    (error? (controlar-aridad lae 0)) (list '*error* 'not-implemented)
    :else (println)
  )
)

; "Suma los elementos de una lista. Minimo 2 elementos."
(defn fnc-add [lae]
  (cond
    (> 2 (count lae)) (list '*error* 'too-few-args)
    (every? number? lae) (apply + lae)
    :else (list '*error* 'number-expected (first (filter (comp not number?) lae)))
  )
)

; "Resta los elementos de un lista. Minimo 1 elemento."
(defn fnc-sub [lae]
  (cond
    (> 1 (count lae)) (list '*error* 'too-few-args)
    (every? number? lae) (apply - lae)
    :else (list '*error* 'number-expected (first (filter (comp not number?) lae)))
  )
)

; "Devuelve t si el primer numero es menor que el segundo; si no, nil."
(defn fnc-lt [lae]
  (let [res-aridad (controlar-aridad lae 2)]
    (cond
      (error? res-aridad) res-aridad
      (not-every? number? lae) (list '*error* 'number-expected (first (filter (comp not number?) lae)))
      (< (first lae) (second lae)) 't
      :else nil
    )
  )
)

; "Devuelve t si el primer numero es mayor que el segundo; si no, nil."
(defn fnc-gt [lae]
  (let [res-aridad (controlar-aridad lae 2)]
    (cond
      (error? res-aridad) res-aridad
      (not-every? number? lae) (list '*error* 'number-expected (first (filter (comp not number?) lae)))
      (> (first lae) (second lae)) 't
      :else nil
    )
  )
)

; "Devuelve t si el primer numero es mayor o igual que el segundo; si no, nil."
(defn fnc-ge [lae]
  (let [res-aridad (controlar-aridad lae 2)]
    (cond
      (error? res-aridad) res-aridad
      (not-every? number? lae) (list '*error* 'number-expected (first (filter (comp not number?) lae)))
      (>= (first lae) (second lae)) 't
      :else nil
    )
  )
)

; "Devuelve una lista con sus elementos en orden inverso."
(defn fnc-reverse [lae]
  (let [res-aridad (controlar-aridad lae 1)]
    (cond
      (error? res-aridad) res-aridad
      (seq? (first lae)) (reverse (first lae))
      :else (list '*error* 'list 'expected (first lae))
    )
  )
)

; "Evalua una expresion escalar consultando, si corresponde, los ambientes local y global. Devuelve una lista con el resultado y un ambiente."
(defn evaluar-escalar [expre amb-global amb-local]
  (let [busqueda-local (buscar expre amb-local), busqueda-global (buscar expre amb-global)]
    (cond
      (equiv-a-nill? expre) (list nil amb-global)
      (not (symbol? expre)) (list expre amb-global)
      (not (error? busqueda-local)) (list busqueda-local amb-global)
      (not (error? busqueda-global)) (list busqueda-global amb-global)
      :else (list (list '*error* 'unbound-symbol expre) amb-global)
    )
  )
)

; "Evalua una forma 'de'. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
(defn evaluar-de [expre amb]
  (cond
    (< (count expre) 3) (list (list '*error* 'list 'expected nil) amb)
    (not (seq? (nth expre 2))) (list (list '*error* 'list 'expected (nth expre 2)) amb)
    (equiv-a-nill? (second expre)) (list (list '*error* 'cannot-set nil) amb)
    (not (symbol? (second expre))) (list (list '*error* 'symbol 'expected (second expre)) amb) 
    :else (list (second expre) (actualizar-amb amb (second expre) (apply list 'lambda (nthrest expre 2))))
  )
)

; Evalua todas las expresiones actualizando el ambiente
(defn evaluar-if-aux [expre amb-global amb-local]
  (cond
    (= 1 (count expre)) (evaluar (first expre) amb-global amb-local)
    :else (evaluar-if-aux (rest expre) (second (evaluar (first expre) amb-global amb-local)) amb-local)
  )
)

; "Evalua una forma 'if'. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
(defn evaluar-if [expre amb-global amb-local]
  (cond
    (< (count expre) 3) (list nil amb-global)
    :else (let [cond-evaluada (evaluar (second expre) amb-global amb-local)]
            (cond
              (error? cond-evaluada) cond-evaluada
              (not (equiv-a-nill? (first cond-evaluada))) (evaluar (nth expre 2) amb-global amb-local)
              (= (count expre) 3) (list nil amb-global)
              :else (evaluar-if-aux (nthrest expre 3) amb-global amb-local)
            )
          )
  )
)

; "Evalua una forma 'or'. Devuelve una lista con el resultado y un ambiente."
(defn evaluar-or [expre amb-global amb-local]
  (cond
    (= (count expre) 1) (list nil amb-global)
    :else (let [primer-cond (evaluar (second expre) amb-global amb-local)]
            (cond
              (not (equiv-a-nill? (first primer-cond))) primer-cond
              :else (evaluar-or (rest expre) amb-global amb-local)
            )
          )
  )
)

; "Evalua una forma 'setq'. Devuelve una lista con el resultado y un ambiente actualizado."
(defn evaluar-setq [expre amb-global amb-local]
  (cond
    (< (count expre) 3) (list (list '*error* 'list 'expected nil) amb-global)
    (equiv-a-nill? (second expre)) (list (list '*error* 'cannot-set nil) amb-global)
    (not (symbol? (second expre))) (list (list '*error* 'symbol 'expected (nth expre 1)) amb-global)
    (= (count expre) 3) (let [eval (evaluar (nth expre 2) amb-global amb-local)] (list (first eval) (actualizar-amb (second eval) (second expre) (first eval))))
    :else (let [valor-eval (evaluar (nth expre 2) amb-global amb-local)] 
            (evaluar-setq (drop 2 expre) (actualizar-amb (second valor-eval) (second expre) (first valor-eval)) amb-local)
          )
  )
)