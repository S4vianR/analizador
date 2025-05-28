package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Lexer representa el analizador léxico
// Se usa para obtener tokens uno por uno para el analizador sintáctico
type Lexer struct {
	tokens     []Token
	currentPos int
	filename   string
	scanner    *bufio.Scanner
	file       *os.File
}

// NewLexer crea un nuevo analizador léxico
func NewLexer(filename string) (*Lexer, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	scanner := bufio.NewScanner(file)
	return &Lexer{
		filename: filename,
		scanner:  scanner,
		file:     file,
	}, nil
}

// Close cierra el archivo
func (l *Lexer) Close() {
	if l.file != nil {
		l.file.Close()
	}
}

// NextToken devuelve el siguiente token
func (l *Lexer) NextToken() (Token, bool) {
	if l.currentPos >= len(l.tokens) {
		if !l.scanner.Scan() {
			return Token{}, false
		}
		line := l.scanner.Text()
		analyzeLine(line)
	}

	token := l.tokens[l.currentPos]
	l.currentPos++
	return token, true
}

// PeekToken devuelve el siguiente token sin avanzar el cursor
func (l *Lexer) PeekToken() (Token, bool) {
	if l.currentPos >= len(l.tokens) {
		if !l.scanner.Scan() {
			return Token{}, false
		}
		line := l.scanner.Text()
		analyzeLine(line, &TokenCollector{})
	}

	return l.tokens[l.currentPos], true
}

// ResetTokenPos reinicia la posición del cursor a 0
func (l *Lexer) ResetTokenPos() {
	l.currentPos = 0
}

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

	// Palabras reservadas
	TOKEN_RESERVED_FUNC      // func
	TOKEN_RESERVED_VAR       // var
	TOKEN_RESERVED_CONST     // const
	TOKEN_RESERVED_IF        // if
	TOKEN_RESERVED_ELIF      // elif
	TOKEN_RESERVED_ELSE      // else
	TOKEN_RESERVED_FOR       // for
	TOKEN_RESERVED_IN        // in
	TOKEN_RESERVED_RANGE     // range
	TOKEN_RESERVED_RETURN    // return
	TOKEN_RESERVED_TRUE      // true
	TOKEN_RESERVED_FALSE     // false
	TOKEN_RESERVED_INT       // int
	TOKEN_RESERVED_FLOAT     // float
	TOKEN_RESERVED_STR       // str
	TOKEN_RESERVED_BOOL      // bool
	TOKEN_RESERVED_UNDEFINED // undefined
	TOKEN_RESERVED_NUM       // num
	TOKEN_RESERVED_PRINT     // print
	TOKEN_RESERVED_LENGHT    // length
	TOKEN_RESERVED_INPUT     // input
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

// Variables globales para definir los tokens, se analizará código Siva
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

	// Listas adaptadas al lenguaje definido por el usuario
	OPERATORS      = []string{"+", "-", "*", "/", "==", "!=", "=", ":"}
	SEPARATORS     = []string{",", ";", ":"}
	DELIMITERS     = []string{"(", ")", "[", "]", "{", "}"}
	COMMENTS       = []string{"#"}
	RESERVED_WORDS = []string{"var", "const", "func", "if", "elif", "else", "for", "in", "range", "return", "true", "false", "int", "float", "str", "bool", "undefined", "num", "print", "lenght", "input"}
)

// Variables globales para el Lexer
var lexer *Lexer

// Función para inicializar el Lexer
type TokenCollector struct {
	tokens []Token
}

func (c *TokenCollector) AddToken(token Token) {
	c.tokens = append(c.tokens, token)
}

// Función para obtener los tokens del TokenCollector
func (c *TokenCollector) GetTokens() []Token {
	return c.tokens
}

func saveTokensToFile() {
	filename := "tokens.txt"
	file, err := os.Create(filename)
	if err != nil {
		fmt.Println("Error al crear el archivo de tokens:", err)
		return
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	for _, token := range tokensFound {
		fmt.Fprintf(writer, "Token: %s -> %s\n", token.lexeme, token.tokenType)
	}

	writer.Flush()
	fmt.Println("Tokens guardados en", filename)
}

func main() {
	// Instrucciones del programa:
	// Listar los componentes léxicos encontrados en el siguiente programa en mi propio lenguaje de programación, Siva

	// Verificar si se proporcionó el nombre del archivo como argumento
	if len(os.Args) < 2 {
		fmt.Println("Se debe proporcionar el nombre del archivo como argumento")
		return
	}

	// Crear el analizador léxico
	lexer, err := NewLexer(os.Args[1])
	if err != nil {
		fmt.Println("Error al crear el analizador léxico:", err)
		return
	}
	defer lexer.Close()

	// Crear un TokenCollector para recopilar los tokens
	collector := &TokenCollector{}

	// Leer y analizar el archivo línea por línea
	for scanner.Scan() {
		line := scanner.Text()
		analyzeLine(line, collector)
	}

	// Agregar los tokens al Lexer
	lexer.tokens = collector.GetTokens()

	// Leer y mostrar tokens uno por uno
	fmt.Println("Tokens encontrados:")
	fmt.Println("==================")

	for {
		token, ok := lexer.NextToken()
		if !ok {
			break
		}
		fmt.Printf("Token: %s -> %s\n", token.Lexeme, token.Type.String())
	}

	// Guardar tokens en archivo
	saveTokensToFile()

	// Imprimir resumen por categorías
	printTokenSummary()
}

// Función para analizar una línea de código y contar los tokens
func analyzeLine(line string, collector *TokenCollector) {
	// Eliminar espacios en blanco al inicio y final
	line = strings.TrimSpace(line)

	// Si la línea está vacía, continuar
	if line == "" {
		return
	}

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
			// Verificar si es un comentario
			if contains(COMMENTS, token) {
				count_comments++
				collector.AddToken(Token{Type: TOKEN_INVALID, Lexeme: token})
			} else if contains(RESERVED_WORDS, token) {
				count_reserved_words++
				var tokenType TokenType
				switch token {
				case "func":
					tokenType = TOKEN_RESERVED_FUNC
				case "var":
					tokenType = TOKEN_RESERVED_VAR
				case "const":
					tokenType = TOKEN_RESERVED_CONST
				case "if":
					tokenType = TOKEN_RESERVED_IF
				case "elif":
					tokenType = TOKEN_RESERVED_ELIF
				case "else":
					tokenType = TOKEN_RESERVED_ELSE
				case "for":
					tokenType = TOKEN_RESERVED_FOR
				case "in":
					tokenType = TOKEN_RESERVED_IN
				case "range":
					tokenType = TOKEN_RESERVED_RANGE
				case "return":
					tokenType = TOKEN_RESERVED_RETURN
				case "true":
					tokenType = TOKEN_RESERVED_TRUE
				case "false":
					tokenType = TOKEN_RESERVED_FALSE
				case "int":
					tokenType = TOKEN_RESERVED_INT
				case "float":
					tokenType = TOKEN_RESERVED_FLOAT
				case "str":
					tokenType = TOKEN_RESERVED_STR
				case "bool":
					tokenType = TOKEN_RESERVED_BOOL
				case "undefined":
					tokenType = TOKEN_RESERVED_UNDEFINED
				case "num":
					tokenType = TOKEN_RESERVED_NUM
				case "print":
					tokenType = TOKEN_RESERVED_PRINT
				case "lenght":
					tokenType = TOKEN_RESERVED_LENGHT
				case "input":
					tokenType = TOKEN_RESERVED_INPUT
				}
				collector.AddToken(Token{Type: tokenType, Lexeme: token})
			} else if contains(OPERATORS, token) {
				count_operators++
				collector.AddToken(Token{
					Type:   operatorTokens[token],
					Lexeme: token,
				})
			} else if contains(DELIMITERS, token) {
				count_delimiters++
				collector.AddToken(Token{
					Type:   delimiterTokens[token],
					Lexeme: token,
				})
			} else if contains(SEPARATORS, token) {
				count_separators++
				collector.AddToken(Token{
					Type:   separatorTokens[token],
					Lexeme: token,
				})
			} else if isIdentifier(token) {
				collector.AddToken(Token{
					Type:   TOKEN_IDENTIFIER,
					Lexeme: token,
				})
				tokensFound = append(tokensFound, TokenFound{token, "IDENTIFIER"})
			} else if _, err := strconv.Atoi(token); err == nil {
				tokensFound = append(tokensFound, TokenFound{token, "NUMBER"})
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
	} else if _, err := strconv.Atoi(token); err == nil {
		fmt.Printf("Token: %s -> TOKEN_NUMBER\n", token)
	} else if isIdentifier(token) {
		fmt.Printf("Token: %s -> TOKEN_IDENTIFIER\n", token)
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

var reservedTypes = []string{"int", "float", "str", "bool", "undefined", "num"}
var reservedFunctions = []string{"print", "lenght", "input"}

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
