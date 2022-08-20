(ns tp.core-test
  (:require [clojure.test :refer :all]
            [tp.core :refer :all]))

(deftest controlar-aridad-test
  (testing "Pruebas de la funcion: controlar-aridad"
    (is (= 3 (controlar-aridad '(a b c) 3)))
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)

(deftest equiv-a-nill?-test
  (testing "Pruebas de la funcion: equiv-a-nil?"
    (is (= true (equiv-a-nill? nil)))
    (is (= true (equiv-a-nill? 'NIL)))
    (is (= true (equiv-a-nill? '())))
    (is (= false (equiv-a-nill? 3)))
    (is (= false (equiv-a-nill? '(2))))
    (is (= false (equiv-a-nill? 'a)))
    (is (= false (equiv-a-nill? "b")))
  )  
)

(deftest igual?-test
  (testing "Pruebas de la funcion: igual?"
    (is (= true (igual? 1 1)))
    (is (= false (igual? 1 2)))
    (is (= true (igual? 'a 'a)))
    (is (= true (igual? 'A 'A)))
    (is (= true (igual? 'a 'A)))
    (is (= true (igual? 'A 'a)))
    (is (= false (igual? 'a 'b)))
    (is (= true (igual? '(a b c) '(A B C)))) 
    (is (= false (igual? '(a b c) '(A B D))))
    (is (= true (igual? nil nil)))
    (is (= true (igual? nil 'NIL))) 
    (is (= true (igual? 'NIL nil))) 
    (is (= true (igual? 'NIL 'NIL)))
    (is (= true (igual? nil ()))) 
    (is (= true (igual? 'NIL ()))) 
    (is (= true (igual? () ())))
    (is (= false (igual? () '(nil)))) 
    (is (= true (igual? "a" "a"))) 
    (is (= false (igual? "a" "A"))) 
    (is (= false (igual? 'a "a"))) 
    (is (= false (igual? 'a "A"))) 
    (is (= false (igual? 4 ()))) 
    (is (= true (igual? 'NIL 'NiL))) 
    (is (= true (igual? 'COND 'cond))) 
    (is (= true (igual? \space \space))) 
  )
)

(deftest error?-test
  (testing "Pruebas de la funcion: error?"
    (is (= true (error? '(*error* too-few-args)))) 
    (is (= true (error? (list '*error* 'too-few-args)))) 
    (is (= true (error? (list '*ERROR* 'too-few-args))))
    (is (= true (error? (list '*Error* 'too-few-args)))) 
    (is (= true (error? (list '*error*)))) 
    (is (= false (error? (list 'too-few-args))))
    (is (= false (error? '*error*))) 
    (is (= false (error? ()))) 
    (is (= false (error? nil)))
  )
)

(deftest revisar-fnc-test
  (testing "Pruebas de la funcion: revisar-fnc"
    (is (= '(*error* too-few-args) (revisar-fnc '(*error* too-few-args))))
    (is (= nil (revisar-fnc '(too-few-args))))
    (is (= nil (revisar-fnc '*error*))) 
    (is (= nil (revisar-fnc nil))) 
    (is (= nil (revisar-fnc ()))) 
  )
)

(deftest revisar-lae-test
  (testing "Pruebas de la funcion: revisar-lae"
    (is (= nil (revisar-lae '(1 2 3))))
    (is (= nil (revisar-lae nil)))
    (is (= nil (revisar-lae ()))) 
    (is (= '(*error* too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
    (is (= '(*error* too-few-args) (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))
  )
)

(deftest actualizar-amb-aux-test
  (testing "Pruebas de la funcion: actualizar-amb-aux"
    (is (= '() (actualizar-amb-aux '() 'b 4)))
    (is (= '((a 1) (b 4) (c 3)) (actualizar-amb-aux '((a 1) (b 2) (c 3)) 'b 4)))
    (is (= '((a 1) (b 2) (c 3)) (actualizar-amb-aux '((a 1) (b 2) (c 3)) 'd 5)))
  )
)

(deftest actualizar-amb-test
  (testing "Pruebas de la funcion: actualizar-amb"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)))) 
    (is (= '(b 7) (actualizar-amb () 'b 7)))
    (is (= '(a 9) (actualizar-amb '(a 7) 'a 9)))
  )
)

(deftest buscar-aux-test
  (testing "Pruebas de la funcion: buscar-aux"
    (is (= '(*error* no-encontrado) (buscar-aux 'd '((a 1) (b 2) (c 3)))))
    (is (= 2 (buscar-aux 'b '((a 1) (b 2) (c 3)))))
    (is (= 3 (buscar-aux 'c '((a 1) (b 2) (c 3)))))
  )
)

(deftest buscar-test
  (testing "Pruebas de la funcion: buscar-amb"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= '(*error* unbound-symbol f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)

(deftest fnc-append-test
  (testing "Pruebas de la funcion: fnc-append"
    (is (= '(*error* too-few-args) (fnc-append '((1 2)))))
    (is (= '(*error* too-many-args) (fnc-append '((1 2) (3) (4 5) (6 7)))))
    (is (= '(*error* list expected 3) (fnc-append '((1 2) 3))))
    (is (= '(*error* list expected A) (fnc-append '((1 2) A))))
    (is (= '(1 2 3) (fnc-append '((1 2) (3)))))
    (is (= '(1 2) (fnc-append '((1 2) nil))))
    (is (= '(1 2) (fnc-append '(() (1 2)))))
    (is (= nil (fnc-append '(nil nil))))
    (is (= nil (fnc-append '(() ()))))
    (is (= '(*error* list expected 3) (fnc-append '(3 (1 2)))))
    (is (= '(*error* list expected A) (fnc-append '(A (1 2)))))
    (is (= '(1 2) (fnc-append '(nil (1 2)))))
    (is (= '(1 2) (fnc-append '((1 2) ()))))
    (is (= '(2 (3)) (fnc-append '((2) ((3))))))
  )
)

(deftest fnc-env-test
  (testing "Pruebas de la funcion: fnc-env"
    (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4))))
    (is (= '(*error* too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))
  )
)

(deftest fnc-equal-test
  (testing "Pruebas de la funcion: fnc-equal"
    (is (= 't (fnc-equal '(1 1))))
    (is (= 't (fnc-equal '(A a))))
    (is (= 't (fnc-equal '("1" "1"))))
    (is (= 't (fnc-equal '(nil NIL))))
    (is (= nil (fnc-equal '(1 2))))
    (is (= nil (fnc-equal '(A B))))
    (is (= nil (fnc-equal '("1" 1))))
    (is (= '(*error* too-few-args) (fnc-equal ())))
    (is (= '(*error* too-few-args) (fnc-equal '(A))))
    (is (= '(*error* too-many-args) (fnc-equal '(A a A))))
  )
)

(deftest fnc-read-test
  (testing "Pruebas de la funcion: fnc-read"
    (is (= 1 (with-in-str "1" (fnc-read '()))))
    (is (= 'a (with-in-str "a" (fnc-read '()))))
    (is (= "hola" (with-in-str "\"hola\"" (fnc-read '()))))
    ;(is (= "hola" (with-in-str "\"HOLA\"" (fnc-read '()))))
    (is (= '(hola mundo) (with-in-str "(hola mundo)" (fnc-read '()))))
    (is (= '(hola mundo) (with-in-str "(hola \n mundo)" (fnc-read '()))))
    (is (= nil (with-in-str "()" (fnc-read '()))))
    (is (= nil (with-in-str "nil" (fnc-read '()))))
    (is (= '(*error* not-implemented) (with-in-str "hola" (fnc-read '(1)))))
    (is (= '(*error* not-implemented) (with-in-str "hola" (fnc-read '(1 2)))))
  )
)

(deftest fnc-terpri-test
  (testing "Pruebas de la funcion: fnc-terpri"
    (is (= nil (fnc-terpri ())))
    (is (= '(*error* not-implemented) (fnc-terpri '(1))))
    (is (= '(*error* not-implemented) (fnc-terpri '(1 2))))
  )
)

(deftest fnc-add-test 
  (testing "Pruebas de la funcion: fnc-add"
    (is (= '(*error* too-few-args) (fnc-add ())))
    (is (= '(*error* too-few-args) (fnc-add '(3))))
    (is (= 7 (fnc-add '(3 4))))
    (is (= 12 (fnc-add '(3 4 5))))
    (is (= 18 (fnc-add '(3 4 5 6))))
    (is (= '(*error* number-expected A) (fnc-add '(A 4 5 6))))
    (is (= '(*error* number-expected A) (fnc-add '(3 A 5 6))))
    (is (= '(*error* number-expected A) (fnc-add '(3 4 A 6))))
    (is (= '(*error* number-expected A) (fnc-add '(3 4 5 A))))
  )
)

(deftest fnc-sub-test 
  (testing "Pruebas de la funcion: fnc-sub"
    (is (= '(*error* too-few-args) (fnc-sub ())))
    (is (= -3 (fnc-sub '(3))))
    (is (= -1 (fnc-sub '(3 4))))
    (is (= -6 (fnc-sub '(3 4 5))))
    (is (= -12 (fnc-sub '(3 4 5 6))))
    (is (= '(*error* number-expected A) (fnc-sub '(A 4 5 6))))
    (is (= '(*error* number-expected A) (fnc-sub '(3 A 5 6))))
    (is (= '(*error* number-expected A) (fnc-sub '(3 4 A 6))))
    (is (= '(*error* number-expected A) (fnc-sub '(3 4 5 A))))
  )
)

(deftest fnc-lt-test 
  (testing "Pruebas de la funcion: fnc-lt"
    (is (= '(*error* too-few-args) (fnc-lt ())))
    (is (= '(*error* too-few-args) (fnc-lt '(1))))
    (is (= 't (fnc-lt '(1 2))))
    (is (= nil (fnc-lt '(1 1))))
    (is (= nil (fnc-lt '(2 1))))
    (is (= '(*error* number-expected A) (fnc-lt '(A 1))))
    (is (= '(*error* number-expected A) (fnc-lt '(1 A))))
    (is (= '(*error* too-many-args) (fnc-lt '(1 2 3))))
  )
)

(deftest fnc-gt-test 
  (testing "Pruebas de la funcion: fnc-gt"
    (is (= '(*error* too-few-args) (fnc-gt ())))
    (is (= '(*error* too-few-args) (fnc-gt '(1))))
    (is (= 't (fnc-gt '(2 1))))
    (is (= nil (fnc-gt '(1 1))))
    (is (= nil (fnc-gt '(1 2))))
    (is (= '(*error* number-expected A) (fnc-gt '(A 1))))
    (is (= '(*error* number-expected A) (fnc-gt '(1 A))))
    (is (= '(*error* too-many-args) (fnc-gt '(1 2 3))))
  )
)

(deftest fnc-ge-test 
  (testing "Pruebas de la funcion: fnc-ge"
    (is (= '(*error* too-few-args) (fnc-ge ())))
    (is (= '(*error* too-few-args) (fnc-ge '(1))))
    (is (= 't (fnc-ge '(2 1))))
    (is (= 't (fnc-ge '(1 1))))
    (is (= nil (fnc-ge '(1 2))))
    (is (= '(*error* number-expected A) (fnc-ge '(A 1))))
    (is (= '(*error* number-expected A) (fnc-ge '(1 A))))
    (is (= '(*error* too-many-args) (fnc-ge '(1 2 3))))
  )
)

(deftest fnc-reverse-test 
  (testing "Pruebas de la funcion: fnc-reverse"
    (is (= '(*error* too-few-args) (fnc-reverse ())))
    (is (= '(*error* list expected 1) (fnc-reverse '(1))))
    (is (= '(*error* list expected A) (fnc-reverse '(A))))
    (is (= '(1) (fnc-reverse '((1)))))
    (is (= '(3 2 1) (fnc-reverse '((1 2 3)))))
    (is (= '(*error* too-many-args) (fnc-reverse '((1 2 3)(4)))))
  )
)

(deftest evaluar-escalar-test 
  (testing "Pruebas de la funcion: evaluar-escalar"
    (is (= '(32 (v 1 w 3 x 6)) (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("chau" (v 1 w 3 x 6)) (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(3 (v 1 w 3 x 6)) (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(5 (v 1 w 3 x 6)) (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '((*error* unbound-symbol n) (v 1 w 3 x 6)) (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(() (v 1 w 3 x 6)) (evaluar-escalar 'y '(v 1 w 3 x 6) '(F (lambda (x y) (if (gt x 0) (cons x y) y)) L (8 ()) x 8 y ()))))
    (is (= '(nil (v 1 w 3 x 6)) (evaluar-escalar '() '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-de-test 
  (testing "Pruebas de la funcion: evaluar-de"
    (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(de f (x)) '(x 1))))
    (is (= '(f (x 1 f (lambda (x) 2))) (evaluar-de '(de f (x) 2) '(x 1))))
    (is (= '(f (x 1 f (lambda (x) (+ x 1)))) (evaluar-de '(de f (x) (+ x 1)) '(x 1))))
    (is (= '(f (x 1 f (lambda (x y) (+ x y)))) (evaluar-de '(de f (x y) (+ x y)) '(x 1))))
    (is (= '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y))) (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1))))
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de) '(x 1))))
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de f) '(x 1))))
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2) '(x 1))))
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2 3) '(x 1))))
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de (f)) '(x 1))))
    (is (= '((*error* list expected x) (x 1)) (evaluar-de '(de 2 x) '(x 1))))
    (is (= '((*error* symbol expected 2) (x 1)) (evaluar-de '(de 2 (x)) '(x 1))))
    (is (= '((*error* cannot-set nil) (x 1)) (evaluar-de '(de nil (x) 2) '(x 1))))
  )
)

(deftest evaluar-if-aux-test
  (testing "Pruebas de la funcion: evaluar-if-aux"
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if-aux '(1 2 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("hola" (nil nil t t v 1 w 3 x 6 n 8 m 9)) (evaluar-if-aux '((setq n 8) (setq m 9) z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(9 (nil nil t t v 1 w 3 x 6 m 9 n 9)) (evaluar-if-aux '((setq n (setq m 9))) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-test 
  (testing "Pruebas de la funcion: evaluar-if"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(3 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8)) (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(10 (gt gt v 1 w 3 x 6 m 8 n 9 j 10)) (evaluar-if '(if nil a (setq m 8) (setq n 9) (setq j 10)) '(gt gt v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
    (is (= '(11 (gt gt v 1 w 3 x 6 n 9)) (evaluar-if '(if nil a "hola" (setq n 9) y) '(gt gt v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-or-test 
  (testing "Pruebas de la funcion: evaluar-or"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(5 (nil nil t t w 5 x 4)) (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '((*error* unbound-symbol r) (nil nil t t w 5 x 4)) (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(8 (nil nil t t w 5 x 4 b 8)) (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
)

(deftest evaluar-setq-test 
  (testing "Pruebas de la funcion: evaluar-setq"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(7 (nil nil t t + add w 5 x 4 m 7)) (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(7 (nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(2 (nil nil t t + add w 5 x 2)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(5 (nil nil t t + add w 5 x 5)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '((*error* cannot-set nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '((*error* symbol expected 7) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(8 (nil nil t t + add w 5 x 7 m 8)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
    (is (= '(14 (nil nil t t + add w 5 x 7 m 14)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
    (is (= '(9 (nil nil t t + add w 5 x 7 y 8 z 9)) (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
    (is (= '(17 (nil nil t t + add w 5 x 4 n 17)) (evaluar-setq '(setq n 0 N 17) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
    (is (= '(9 (nil nil t t + add w 5 x 9 y 9 z 9)) (evaluar-setq '(setq x (setq y 9) z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
)