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
El único ciclo será con el típico for-loop
```typescript
for var: int = x in range(10) {
    print(x)
}
```

## Funciones
```go
func print(msg:str) {
	return msg
}

func hola() {
	var mensaje: str = "Hola mundo"
	print(mensaje)
}

func main() {
	hola()
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
	elif op "*" {
		res: int = int(num1)*int(num2)
	}
	elif op "/" {
		res: int = int(num1)/int(num2)
	}
	else {
		print("Favor de escribir un operador válido, reiniciando...")
		main()
	}

	print("El resultado es: " + res)
}
```

```siv
# Funciones incluidas en el lenguaje

## Metodo para imprimir en consola
print(nombre)

## Metodo para conocer la longitud de los "str" y los "arr" -> Arreglos
lenght(nombre)
``` 
