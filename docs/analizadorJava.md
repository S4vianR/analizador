```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Definición de tipos de tokens
type TokenType int

const (
	// Tipos de tokens
	TOKEN_INVALID TokenType = iota
	TOKEN_IDENTIFIER
	TOKEN_NUMBER
	TOKEN_STRING

	// Operadores
	TOKEN_PLUS       // +
	TOKEN_MINUS      // -
	TOKEN_MULTIPLY   // *
	TOKEN_DIVIDE     // /
	TOKEN_MOD        // %
	TOKEN_INCREMENT  // ++
	TOKEN_DECREMENT  // --
	TOKEN_EQUALS     // ==
	TOKEN_NOT_EQUALS // !=
	TOKEN_GREATER    // >
	TOKEN_LESS       // <
	TOKEN_GREATER_EQ // >=
	TOKEN_LESS_EQ    // <=
	TOKEN_AND        // &&
	TOKEN_OR         // ||
	TOKEN_NOT        // !
	TOKEN_ASSIGN     // =

	// Delimitadores
	TOKEN_LPAREN   // (
	TOKEN_RPAREN   // )
	TOKEN_LBRACKET // [
	TOKEN_RBRACKET // ]
	TOKEN_LBRACE   // {
	TOKEN_RBRACE   // }

	// Separadores
	TOKEN_DOT       // .
	TOKEN_COMMA     // ,
	TOKEN_SEMICOLON // ;
	TOKEN_COLON     // :

	// Palabras reservadas - se definirán dinámicamente
	TOKEN_RESERVED_START
)

func (t TokenType) String() string {
	return []string{
		"TOKEN_INVALID",
		"TOKEN_IDENTIFIER",
		"TOKEN_NUMBER",
		"TOKEN_STRING",
		"TOKEN_PLUS",
		"TOKEN_MINUS",
		"TOKEN_MULTIPLY",
		"TOKEN_DIVIDE",
		"TOKEN_MOD",
		"TOKEN_INCREMENT",
		"TOKEN_DECREMENT",
		"TOKEN_EQUALS",
		"TOKEN_NOT_EQUALS",
		"TOKEN_GREATER",
		"TOKEN_LESS",
		"TOKEN_GREATER_EQ",
		"TOKEN_LESS_EQ",
		"TOKEN_AND",
		"TOKEN_OR",
		"TOKEN_NOT",
		"TOKEN_ASSIGN",
		"TOKEN_LPAREN",
		"TOKEN_RPAREN",
		"TOKEN_LBRACKET",
		"TOKEN_RBRACKET",
		"TOKEN_LBRACE",
		"TOKEN_RBRACE",
		"TOKEN_DOT",
		"TOKEN_COMMA",
		"TOKEN_SEMICOLON",
		"TOKEN_COLON",
		"TOKEN_RESERVED_START",
	}[t]
}

// Estructura para representar un token
type Token struct {
	Type    TokenType
	Lexeme  string
	Literal any
}

// Variables globales para definir los tokens, se analizará código Java
var (
	// Mapeo de operadores a tipos de tokens
	operatorTokens = map[string]TokenType{
		"+":  TOKEN_PLUS,
		"-":  TOKEN_MINUS,
		"*":  TOKEN_MULTIPLY,
		"/":  TOKEN_DIVIDE,
		"%":  TOKEN_MOD,
		"++": TOKEN_INCREMENT,
		"--": TOKEN_DECREMENT,
		"==": TOKEN_EQUALS,
		"!=": TOKEN_NOT_EQUALS,
		">":  TOKEN_GREATER,
		"<":  TOKEN_LESS,
		">=": TOKEN_GREATER_EQ,
		"<=": TOKEN_LESS_EQ,
		"&&": TOKEN_AND,
		"||": TOKEN_OR,
		"!":  TOKEN_NOT,
		"=":  TOKEN_ASSIGN,
	}

	// Mapeo de delimitadores a tipos de tokens
	delimiterTokens = map[string]TokenType{
		"(": TOKEN_LPAREN,
		")": TOKEN_RPAREN,
		"[": TOKEN_LBRACKET,
		"]": TOKEN_RBRACKET,
		"{": TOKEN_LBRACE,
		"}": TOKEN_RBRACE,
	}

	// Mapeo de separadores a tipos de tokens
	separatorTokens = map[string]TokenType{
		".": TOKEN_DOT,
		",": TOKEN_COMMA,
		";": TOKEN_SEMICOLON,
		":": TOKEN_COLON,
	}

	// Las listas originales se mantienen para referencia
	OPERATORS      = []string{"+", "-", "*", "/", "%", "++", "--", "==", "!=", ">", "<", ">=", "<=", "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", ">>>", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", ">>>=", "?", ":", "="}
	SEPARATORS     = []string{".", ",", ";", ":"}
	DELIMITERS     = []string{"(", ")", "[", "]", "{", "}"}
	COMMENTS       = []string{"//", "/*", "*/"}
	RESERVED_WORDS = []string{"abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "if", "goto", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "try", "void", "volatile", "while"}
)

// Estructuras para almacenar los tokens encontrados
type TokenFound struct {
	lexeme    string
	tokenType string
}

var tokensFound []TokenFound
var count_reserved_words = 0
var count_operators = 0
var count_separators = 0
var count_delimiters = 0
var count_comments = 0

func main() {
	// Instrucciones del programa:
	// Listar los componentes léxicos encontrados en el siguiente programa en Java

	// Verificar si se proporcionó el nombre del archivo como argumento
	if len(os.Args) < 2 {
		fmt.Println("Se debe proporcionar el nombre del archivo como argumento")
		return
	}

	// Se obtiene el nombre del archivo a analizar
	fileName := os.Args[1]
	// Se abre el archivo
	file, err := os.Open(fileName)
	// Se verifica si hubo un error al abrir el archivo
	if err != nil {
		fmt.Println("Error al abrir el archivo")
		return
	}
	// Se cierra el archivo al finalizar la función
	defer file.Close()

	// Se crea un scanner para leer el archivo línea por línea
	scanner := bufio.NewScanner(file)
	// Se verifica si hubo algún error durante la lectura
	if err := scanner.Err(); err != nil {
		fmt.Println("Error al leer el archivo:", err)
	}

	fmt.Println("Tokens encontrados:")
	fmt.Println("==================")

	for scanner.Scan() {
		// Se obtiene la línea actual
		line := scanner.Text()
		// Se analiza la línea
		analyzeLine(line)
	}

	// Imprimir resumen por categorías
	printTokenSummary()
}

// Función para analizar una línea de código y contar los tokens
func analyzeLine(line string) {
	// Se obtiene la longitud de la línea
	length := len(line)
	// Se inicializa la posición actual
	pos := 0

	// Se recorre la línea
	for pos < length {
		// Se obtiene el siguiente token
		token, newPos := getNextToken(line, pos)
		// Se verifica si el token es válido
		if token != "" {
			// Identificar el tipo de token
			if contains(RESERVED_WORDS, token) {
				tokensFound = append(tokensFound, TokenFound{token, "RESERVED"})
				count_reserved_words++
			} else if contains(OPERATORS, token) {
				tokensFound = append(tokensFound, TokenFound{token, "OPERATOR"})
				count_operators++
			} else if contains(SEPARATORS, token) {
				tokensFound = append(tokensFound, TokenFound{token, "SEPARATOR"})
				count_separators++
			} else if contains(DELIMITERS, token) {
				tokensFound = append(tokensFound, TokenFound{token, "DELIMITER"})
				count_delimiters++
			} else if contains(COMMENTS, token) {
				tokensFound = append(tokensFound, TokenFound{token, "COMMENT"})
				count_comments++
			} else if isIdentifier(token) {
				tokensFound = append(tokensFound, TokenFound{token, "IDENTIFIER"})
			}

			// Imprimir el token encontrado con su nombre específico
			printTokenName(token)
		}
		// Se actualiza la posición actual
		pos = newPos
	}
}

func printTokenName(token string) {
	// Imprimir el token con su nombre específico según el tipo
	if op, ok := operatorTokens[token]; ok {
		fmt.Printf("Token: %s -> %s\n", token, op.String())
	} else if delim, ok := delimiterTokens[token]; ok {
		fmt.Printf("Token: %s -> %s\n", token, delim.String())
	} else if sep, ok := separatorTokens[token]; ok {
		fmt.Printf("Token: %s -> %s\n", token, sep.String())
	} else if contains(RESERVED_WORDS, token) {
		fmt.Printf("Token: %s -> RESERVED_%s\n", token, strings.ToUpper(token))
	}
}

func printTokenSummary() {
	fmt.Println("\nResumen por categorías:")
	fmt.Println("=====================")

	fmt.Println("\nPalabras Reservadas encontradas:")
	for _, t := range tokensFound {
		if t.tokenType == "RESERVED" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nOperadores encontrados:")
	for _, t := range tokensFound {
		if t.tokenType == "OPERATOR" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nDelimitadores encontrados:")
	for _, t := range tokensFound {
		if t.tokenType == "DELIMITER" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nSeparadores encontrados:")
	for _, t := range tokensFound {
		if t.tokenType == "SEPARATOR" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nIdentificadores encontrados:")
	for _, t := range tokensFound {
		if t.tokenType == "IDENTIFIER" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nComentarios encontrados:")
	for _, t := range tokensFound {
		if t.tokenType == "COMMENT" {
			fmt.Printf("  %s\n", t.lexeme)
		}
	}

	fmt.Println("\nResumen de conteo:")
	fmt.Println("=================")
	fmt.Printf("Palabras reservadas: %d\n", count_reserved_words)
	fmt.Printf("Operadores: %d\n", count_operators)
	fmt.Printf("Delimitadores: %d\n", count_delimiters)
	fmt.Printf("Separadores: %d\n", count_separators)
	fmt.Printf("Identificadores: %d\n", len(tokensFound)-count_reserved_words-count_operators-count_separators-count_delimiters-count_comments)
	fmt.Printf("Comentarios: %d\n", count_comments)
}

func getNextToken(line string, pos int) (string, int) {
	length := len(line)
	if pos >= length {
		return "", pos
	}

	// Ignorar espacios en blanco
	for pos < length && (line[pos] == ' ' || line[pos] == '\t') {
		pos++
	}
	if pos >= length {
		return "", pos
	}

	// Verificar literales de cadena
	if line[pos] == '"' {
		start := pos
		pos++ // Avanzar para el siguiente carácter
		for pos < length && line[pos] != '"' {
			pos++
		}
		if pos < length {
			pos++ // Avanzar para incluir la comilla de cierre
			return line[start:pos], pos
		}
	}

	// Verificar operadores de dos caracteres
	if pos+1 < length {
		twoChars := line[pos : pos+2]
		if contains(OPERATORS, twoChars) {
			return twoChars, pos + 2
		}
	}

	// Verificar separadores
	if contains(SEPARATORS, string(line[pos])) {
		return string(line[pos]), pos + 1
	}

	// Verificar delimitadores
	if contains(DELIMITERS, string(line[pos])) {
		return string(line[pos]), pos + 1
	}

	// Verificar operadores de un carácter
	if contains(OPERATORS, string(line[pos])) {
		return string(line[pos]), pos + 1
	}

	// Leer identificadores y palabras reservadas
	if isLetter(line[pos]) {
		start := pos
		for pos < length && (isLetter(line[pos]) || isDigit(line[pos])) {
			pos++
		}
		return line[start:pos], pos
	}

	// Leer números
	if isDigit(line[pos]) {
		start := pos
		for pos < length && isDigit(line[pos]) {
			pos++
		}
		return line[start:pos], pos
	}

	// Si no es ninguno de los anteriores, avanzar
	return string(line[pos]), pos + 1
}

// Funciones auxiliares para verificar caracteres
func isLetter(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

var reservedTypes = []string{"int", "boolean", "String", "float", "double", "char"}
var reservedFunctions = []string{"System", "out", "println"}

func isIdentifier(token string) bool {
	if len(token) == 0 {
		return false
	}
	if !isLetter(token[0]) && token[0] != '_' {
		return false
	}
	for _, c := range token {
		if !isLetter(byte(c)) && !isDigit(byte(c)) && c != '_' {
			return false
		}
	}
	// Excluir literales booleanos y numéricos
	if token == "true" || token == "false" {
		return false
	}
	if _, err := strconv.Atoi(token); err == nil {
		return false
	}
	// Excluir tipos de datos reservados
	for _, reservedType := range reservedTypes {
		if token == reservedType {
			return false
		}
	}
	// Excluir funciones y clases reservadas
	for _, reservedFunction := range reservedFunctions {
		if token == reservedFunction {
			return false
		}
	}
	// Excluir palabras reservadas
	if contains(RESERVED_WORDS, token) {
		return false
	}
	return true
}

// Función para verificar si un elemento está en un arreglo
func contains(arr []string, element string) bool {
	// Se recorre el arreglo
	for _, e := range arr {
		// Se verifica si el elemento está en el arreglo
		if e == element {
			return true
		}
	}
	// Se retorna falso si el elemento no está en el arreglo
	return false
}

```