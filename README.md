# Analizador Siva

Este proyecto implementa un analizador léxico y sintáctico para el lenguaje Siva, desarrollado en Go.

## Estructura del Proyecto

- `main.go`: Punto de entrada del programa.
- `pkg/lexer/analizadorLexico.go`: Analizador léxico.
- `pkg/parser/analizadorSintaxis.go`: Analizador sintáctico.
- `tokens/tokens.go`: Definición de los tokens del lenguaje.
- `docs/`: Documentación y definición formal del lenguaje.
- `scripts/`: Ejemplos de programas y sus archivos de tokens.

## Instalación

1. Clona el repositorio.
2. Asegúrate de tener Go instalado (versión 1.18 o superior).
3. Instala las dependencias:
   ```sh
   go mod tidy
   ```

## Uso

El programa espera un archivo fuente Siva (`*.siv`).

Ejemplo de ejecución:

```sh
go run main.go scripts/helloWorld/helloWorld.siv
```

Si el análisis es correcto, no se mostrará ningún error. Si hay errores de sintaxis, se mostrará el mensaje correspondiente con la ubicación del error.

## Definición del Lenguaje Siva

El lenguaje Siva es un lenguaje imperativo con sintaxis inspirada en Python y Go. Sus principales características son:

- **Declaración de variables**:  
  ```siva
  var x: int = 5
  const PI: float = 3.14
  ```

- **Funciones**:  
  ```siva
  func suma(a: int, b: int): int {
      return a + b
  }
  ```

- **Estructuras de control**:
  - If/Elif/Else:
    ```siva
    if x > 0 {
        print("Positivo")
    } elif x < 0 {
        print("Negativo")
    } else {
        print("Cero")
    }
    ```
  - While:
    ```siva
    while x < 10 {
        x = x + 1
    }
    ```
  - For (solo con range):
    ```siva
    for i in range(0, 10) {
        print(i)
    }
    ```

- **Impresión y entrada**:
  ```siva
  print("Hola mundo")
  var nombre: str = input()
  ```

- **Arreglos**:
  ```siva
  var lista: array = [1, 2, 3]
  ```

Consulta la gramática formal en `docs/syntax.bnf` y la definición detallada en `docs/siva_code_definition.md`.

## Ejemplos

En la carpeta `scripts/` encontrarás ejemplos de programas Siva y sus archivos de tokens generados.

## Licencia

MIT
