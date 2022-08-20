# Interprete TLC-Lisp en Clojure

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

![Logo Clojure](/resources/clojure-logo.png)

Trabajo práctico de la materia Lenguajes Formales 75.14 de la Facultad de Ingeniería de la Universidad de Buenos Aires. El trabajo consiste en el desarrollo de un interprete de TLC-Lisp

## Descripción

TLC-LISP es un dialecto extinto de LISP. Fue lanzado en la década de 1980, y fue utilizado en el campo de la inteligencia artificial.
Actualmente, TLC-LISP ya no se comercializa más y, en consecuencia, para utilizar el software existente desarrollado en él, se desea construir en este trabajo práctico un intérprete que corra en la JVM. Por ello, el lenguaje elegido para su implementación es Clojure.

## Tecnologías

* Clojure v1.10.3
* Leiningen v2.9.8

## Comandos

### `lein run`

Inicia el REPL

### `lein test`

Corre los tests

## Ejemplos

Luego de ejecutar `lein run` se puede utilizar el comando `(load 'demo)` para ver ejemplos del manejo del lenguaje

Tambien se puede utilizar el comando `(load 'jarras)` seguido de `(breadth-first bc)` para ejecutar una función que resuelve el problema de obtener 4 litros de líquido utilizando dos jarras lisas (sin escala), una de 5 litros y otra de 8 litros, ingresando primero el estado inicial (ej: (0 0)) y el estado final (ej (0 4))
