package parser

import (
	"encoding/json"
	"fmt"
	"os"

	"analizador/tokens"
)

// TokenInfoSintax represents a token in the JSON file
type TokenInfoSintax struct {
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// Parser represents the syntax analyzer
type Parser struct {
	tokens  []TokenInfoSintax
	current int
	errors  []string
}

// NewParser creates a new Parser instance with the given tokens
func NewParser(tokens []TokenInfoSintax) (*Parser, error) {
	if len(tokens) == 0 {
		return nil, fmt.Errorf("no tokens provided")
	}
	return &Parser{
		tokens:  tokens,
		current: 0,
		errors:  []string{},
	}, nil
}

// NewParserFromFile creates a new Parser instance from a JSON file containing tokens.
// It reads the file, parses the JSON data, and returns a new Parser instance.
// If there's an error reading or parsing the file, it returns an error.
func NewParserFromFile(filename string) (*Parser, error) {
	fmt.Printf("Reading tokens from file: %s\n", filename)
	
	// Read the JSON file
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("error reading file %s: %v", filename, err)
	}

	// Parse the JSON data into a slice of TokenInfoSintax
	var tokens []TokenInfoSintax
	if err := json.Unmarshal(data, &tokens); err != nil {
		return nil, fmt.Errorf("error parsing JSON from %s: %v", filename, err)
	}

	// Create and return a new Parser instance
	return &Parser{
		tokens:  tokens,
		current: 0,
		errors:  []string{},
	}, nil
}

// RawToken represents the token structure from the lexer output
type RawToken struct {
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// Parse starts the syntax analysis
func (p *Parser) Parse() error {
	fmt.Println("Starting syntax analysis...")	
	// Start with the program rule
	p.program()

	if len(p.errors) > 0 {
		fmt.Printf("Found %d syntax errors:\n", len(p.errors))
		for i, err := range p.errors {
			fmt.Printf("%d. %s\n", i+1, err)
		}
		return fmt.Errorf("found %d syntax errors", len(p.errors))
	}

	fmt.Println("Syntax analysis completed successfully")
	return nil
}

// skipComments advances past any comment tokens
func (p *Parser) skipComments() {
    for !p.isAtEnd() && p.peek().Type == string(tokens.TOKEN_COMMENT) {
        p.advance()
    }
}

// program -> declaration*
func (p *Parser) program() {
	for !p.isAtEnd() {
		if p.check(tokens.TOKEN_COMMENT) {
			p.skipComments()
			continue
		}
		p.declaration()
	}
}

// declaration -> funcDecl | varDecl | statement
func (p *Parser) declaration() {
	switch {
	case p.match(tokens.TOKEN_RESERVED_FUNC):
		p.function()
	case p.match(tokens.TOKEN_RESERVED_VAR, tokens.TOKEN_RESERVED_CONST):
		p.varDeclaration()
	default:
		p.statement()
	}
}

// function -> "func" IDENTIFIER "(" parameters? ")" block
func (p *Parser) function() {
	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error("Se esperaba un nombre de función")
		return
	}

	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Se esperaba '(' después del nombre de la función")
		return
	}

	if !p.check(tokens.TOKEN_RPAREN) {
		p.parameters()
	}

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Se esperaba ')' después de los parámetros")
		return
	}

	p.block()
}

// parameters -> IDENTIFIER ":" type ("," IDENTIFIER ":" type)*
func (p *Parser) parameters() {
	for {
		if !p.match(tokens.TOKEN_IDENTIFIER) {
			p.error("Se esperaba un identificador de parámetro")
			return
		}

		if !p.match(tokens.TOKEN_COLON) {
			p.error("Se esperaba ':' después del nombre del parámetro")
			return
		}

		if !p.isType() {
			p.error("Tipo de parámetro inválido")
			return
		}
		p.advance()

		if !p.match(tokens.TOKEN_COMMA) {
			break
		}
	}
}

// varDeclaration -> ("var" | "const") IDENTIFIER ":" type ("=" expression)? ";"
func (p *Parser) varDeclaration() {
	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error("Se esperaba un identificador de variable")
		return
	}

	if !p.match(tokens.TOKEN_COLON) {
		p.error("Se esperaba ':' después del nombre de la variable")
		return
	}

	if !p.isType() {
		p.error("Tipo de variable inválido")
		return
	}
	p.advance()

	// Optional initialization
	if p.match(tokens.TOKEN_ASSIGN) {
		p.expression()
	}

	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Se esperaba ';' después de la declaración de variable")
	}
}

// statement -> exprStmt | ifStmt | forStmt | returnStmt | block | printStmt
func (p *Parser) statement() {
	switch {
	case p.match(tokens.TOKEN_RESERVED_IF):
		p.ifStatement()
	case p.match(tokens.TOKEN_RESERVED_FOR):
		p.forStatement()
	case p.match(tokens.TOKEN_RESERVED_RETURN):
		p.returnStatement()
	case p.match(tokens.TOKEN_RESERVED_PRINT):
		p.printStatement()
	case p.match(tokens.TOKEN_LBRACE):
		p.block()
	default:
		p.expressionStatement()
	}
}

// ifStmt -> "if" expression block ("elif" expression block)* ("else" block)?
func (p *Parser) ifStatement() {
	p.expression()
	p.block()

	// Zero or more elifs
	for p.match(tokens.TOKEN_RESERVED_ELIF) {
		p.expression()
		p.block()
	}

	// Optional else
	if p.match(tokens.TOKEN_RESERVED_ELSE) {
		p.block()
	}
}

// forStmt -> "for" "var" IDENTIFIER ":" "int" "=" expression "in" "range" "(" expression ")" block
func (p *Parser) forStatement() {
	if !p.match(tokens.TOKEN_RESERVED_VAR) {
		p.error("Se esperaba 'var' en la declaración del bucle for")
		return
	}

	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error("Se esperaba un identificador en el bucle for")
		return
	}

	if !p.match(tokens.TOKEN_COLON) {
		p.error("Se esperaba ':' después del identificador en el bucle for")
		return
	}

	if !p.match(tokens.TOKEN_RESERVED_INT) {
		p.error("Se esperaba 'int' como tipo en el bucle for")
		return
	}

	if !p.match(tokens.TOKEN_ASSIGN) {
		p.error("Se esperaba '=' en la inicialización del bucle for")
		return
	}

	p.expression()

	if !p.match(tokens.TOKEN_RESERVED_IN) {
		p.error("Se esperaba 'in' en el bucle for")
		return
	}

	if !p.match(tokens.TOKEN_RESERVED_RANGE) {
		p.error("Se esperaba 'range' en el bucle for")
		return
	}

	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Se esperaba '(' después de 'range'")
		return
	}

	p.expression()

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Se esperaba ')' después de la expresión de rango")
		return
	}

	p.block()
}

// returnStmt -> "return" expression? ";"
func (p *Parser) returnStatement() {
	if !p.check(tokens.TOKEN_SEMICOLON) {
		p.expression()
	}

	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Se esperaba ';' después del valor de retorno")
	}
}

// printStmt -> "print" "(" expression ")" ";"
func (p *Parser) printStatement() {
	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Se esperaba '(' después de 'print'")
		return
	}

	p.expression()

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Se esperaba ')' después de la expresión en 'print'")
		return
	}

	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Se esperaba ';' después de 'print'")
	}
}

// block -> "{" declaration* "}"
func (p *Parser) block() {
	for !p.check(tokens.TOKEN_RBRACE) && !p.isAtEnd() {
		p.declaration()
	}

	if !p.match(tokens.TOKEN_RBRACE) {
		p.error("Se esperaba '}' al final del bloque")
	}
}

// expressionStmt -> expression ";"
func (p *Parser) expressionStatement() {
	p.expression()
	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Se esperaba ';' después de la expresión")
	}
}

// expression -> assignment
func (p *Parser) expression() {
	p.assignment()
}

// assignment -> IDENTIFIER "=" assignment | logic_or
func (p *Parser) assignment() {
	p.logicOr()

	if p.match(tokens.TOKEN_ASSIGN) {
		p.assignment()
	}
}

// logic_or -> logic_and ("or" logic_and)*
func (p *Parser) logicOr() {
	p.logicAnd()

	for p.match(tokens.TOKEN_OR) {
		p.logicAnd()
	}
}

// logic_and -> equality ("and" equality)*
func (p *Parser) logicAnd() {
	p.equality()

	for p.match(tokens.TOKEN_AND) {
		p.equality()
	}
}

// equality -> comparison ( ("!=" | "==") comparison )*
func (p *Parser) equality() {
	p.comparison()

	for p.match(tokens.TOKEN_NOT_EQUAL, tokens.TOKEN_EQUAL) {
		p.comparison()
	}
}

// comparison -> term ( (">" | ">=" | "<" | "<=") term )*
func (p *Parser) comparison() {
	p.term()

	for p.match(tokens.TOKEN_GREATER, tokens.TOKEN_GREATER_EQUAL, tokens.TOKEN_LESS, tokens.TOKEN_LESS_EQUAL) {
		p.term()
	}
}

// term -> factor ( ("-" | "+") factor )*
func (p *Parser) term() {
	p.factor()

	for p.match(tokens.TOKEN_MINUS, tokens.TOKEN_PLUS) {
		p.factor()
	}
}

// factor -> unary ( ("/" | "*" | "%") unary )*
func (p *Parser) factor() {
	p.unary()

	for p.match(tokens.TOKEN_DIVIDE, tokens.TOKEN_MULTIPLY, tokens.TOKEN_MODULO) {
		p.unary()
	}
}

// unary -> ("!" | "-") unary | call
func (p *Parser) unary() {
	if p.match(tokens.TOKEN_NOT, tokens.TOKEN_MINUS) {
		p.unary()
	} else {
		p.call()
	}
}

// call -> primary ( "(" arguments? ")" )*
func (p *Parser) call() {
	p.primary()

	for {
		if p.match(tokens.TOKEN_LPAREN) {
			if !p.check(tokens.TOKEN_RPAREN) {
				p.arguments()
			}

			if !p.match(tokens.TOKEN_RPAREN) {
				p.error("Se esperaba ')' después de los argumentos")
				return
			}
		} else {
			break
		}
	}
}

// arguments -> expression ("," expression)*
func (p *Parser) arguments() {
	for {
		p.expression()
		if !p.match(tokens.TOKEN_COMMA) {
			break
		}
	}
}

// primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
func (p *Parser) primary() {
	switch {
	case p.match(tokens.TOKEN_NUMBER, tokens.TOKEN_STRING):
		// Literals are handled by the lexer
		return
	case p.match(tokens.TOKEN_RESERVED_BOOL, tokens.TOKEN_RESERVED_UNDEFINED):
		// Boolean and undefined literals
		return
	case p.match(tokens.TOKEN_LPAREN):
		p.expression()
		if !p.match(tokens.TOKEN_RPAREN) {
			p.error("Se esperaba ')' después de la expresión")
		}
	case p.match(tokens.TOKEN_IDENTIFIER):
		// Identifier
		return
	default:
		p.error("Se esperaba una expresión")
	}
}

// Helper methods

// match checks if the current token matches any of the given token types and consumes it if it does
func (p *Parser) match(types ...tokens.TokenType) bool {
	// Skip any comments before checking for a match
	p.skipComments()
	
	for _, t := range types {
		if p.check(t) {
			fmt.Printf("    [MATCH] Matched token type: %s, lexeme: '%s'\n", t, p.peek().Lexeme)
			p.advance()
			// Skip any comments after a match
			p.skipComments()
			return true
		}
	}
	return false
}

// check checks if the current token is of the given token type
func (p *Parser) check(tokenType tokens.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	// Skip comments when checking token types
	if p.peek().Type == tokens.TOKEN_COMMENT.String() {
		return false
	}
	return p.peek().Type == tokenType.String()
}

// advance consumes the current token and returns it
func (p *Parser) advance() TokenInfoSintax {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

// isAtEnd checks if we've consumed all tokens
func (p *Parser) isAtEnd() bool {
	return p.current >= len(p.tokens) || 
		(p.current < len(p.tokens) && p.tokens[p.current].Type == tokens.TOKEN_EOF.String())
}

// peek returns the current token without consuming it
func (p *Parser) peek() TokenInfoSintax {
	if p.isAtEnd() {
		return TokenInfoSintax{Type: tokens.TOKEN_EOF.String()}
	}
	return p.tokens[p.current]
}

// previous returns the most recently consumed token
func (p *Parser) previous() TokenInfoSintax {
	if p.current == 0 {
		return TokenInfoSintax{Type: tokens.TOKEN_EOF.String()}
	}
	return p.tokens[p.current-1]
}

// isType checks if the current token is a valid type
func (p *Parser) isType() bool {
	if p.isAtEnd() {
		return false
	}
	tokenType := p.peek().Type
	return tokenType == tokens.TOKEN_RESERVED_INT.String() ||
		tokenType == tokens.TOKEN_RESERVED_FLOAT.String() ||
		tokenType == tokens.TOKEN_RESERVED_STR.String() ||
		tokenType == tokens.TOKEN_RESERVED_BOOL.String() ||
		tokenType == tokens.TOKEN_RESERVED_UNDEFINED.String()
}

// error adds an error message at the current token
func (p *Parser) error(message string) {
	token := p.peek()
	line := token.Line
	column := token.Column

	// If we're at the end, use the previous token's position
	if token.Type == tokens.TOKEN_EOF.String() {
		prev := p.previous()
		line = prev.Line
		column = prev.Column + len(prev.Lexeme) // Position after the last token
	}

	errMsg := fmt.Sprintf("[línea %d, columna %d] Error", line, column)
	if token.Lexeme != "" {
		errMsg += fmt.Sprintf(" en '%s'", token.Lexeme)
	}
	errMsg += fmt.Sprintf(": %s", message)

	p.errors = append(p.errors, errMsg)
}