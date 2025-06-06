		# Code_Definition.md

Ésta será la definicion en código para luego pasarlo a la definición formal usando *BNF*.

## Comentarios
```python
# Este es un comentario
```

## Tipos de datos

- **Enteros**: *int*
- **Flotante**: *float*
- **String**: *str*
- **Booleano**: *bool*
- **Undefined**: *undefined*
- **Array**: *array*

## Definición de variables
```typescript
var nombre_variable: tipo_dato = "algo"
const nombre_variable: tipo_dato = 3.141516
```

## Definiciones de variables para los distintos tipos de datos
```typescript
var edad: int = 22
var cambio: float = 45.84
var nombre: str = "Reuben Rhienhart"
var cliente_frecuente: bool = 1
var tareas: array = []
```

## Delimitadores de bloque de código
```typescript
{}
```

## Condicionales

### Para condicionales con bool
```python
if cliente_frecuente {
    print("Eres cliente frecuente")
} else !cliente_frecuente {
    print("No eres clientes frecuente")
}
```

### Para condicionales con signos de comparacion "\==\" ó "\!=\"
```python
if cliente_frecuente == true {
    print("Eres cliente frecuente")
} 
elif cliente_frecuente != true {
    print("No eres cliente frecuente")
}
```

## Ciclo
El único ciclo será con el típico for-loop y while-loop
```typescript
var dato: int = 0
for dato in range(10) {
    print(dato)
}

while cliente_frecuente {
    print("Eres cliente frecuente")
}

while x < 10 {
    print(x)
    x = x + 1
}
```

## Funciones
```typescript
func nombre_funcion(param1: tipo, param2: tipo): tipo_retorno {
    // Bloque de código
    return valor;
}
```

## Programa principal
```typescript
func main() {
    var resultado: int = 0;
    print("Inicio del programa");
    // Llamadas a funciones y lógica del programa
}
```


## Programa de una calculadora básica
```ts
func main() {
	var res: num = 0;
	var num1: str = input("Ingresa el primer número")
	var num2: str = input("Ingresa el segundo número")
	var op: str = input("Ingresa el operador a usar (+,-,*,/)")

	if op == "+" {
		res: int = int(num1)+int(num2)
	} 
	elif op == "-" {
		res: int = int(num1)-int(num2)
	}
	elif op == "*" {
		res: int = int(num1)*int(num2)
	}
	elif op == "/" {
		res: int = int(num1)/int(num2)
	}
	else {
		print("Favor de escribir un operador válido, reiniciando...")
		main()
	}

	print("El resultado es: " + res)
}
```

```py
# Funciones incluidas en el lenguaje

## Método para imprimir en consola
print(mensaje: str) -> void

## Método para conocer la longitud de strings y arreglos
length(valor: any) -> int

## Funciones de conversión de tipos

### Convertir a entero
int(valor: any) -> int
- Convierte un valor a tipo entero
- Si el valor es un string, intenta convertirlo a número
- Si la conversión falla, devuelve 0

### Convertir a flotante
float(valor: any) -> float
- Convierte un valor a tipo flotante
- Si el valor es un string, intenta convertirlo a número
- Si la conversión falla, devuelve 0.0

### Convertir a string
str(valor: any) -> str
- Convierte cualquier valor a su representación en string

### Convertir a booleano
bool(valor: any) -> bool
- Devuelve false para: 0, "", false, undefined, null
- Devuelve true para cualquier otro valor

## Entrada de usuario
input(prompt: str) -> str
- Muestra el mensaje de prompt al usuario
- Devuelve la entrada del usuario como string

## Hacer append en los arreglos
arreglo.append(elemento: any) -> void
- Agrega un elemento al final del arreglo
