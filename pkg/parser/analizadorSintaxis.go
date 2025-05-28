package parser

import (
	"analizador/tokens"
	"encoding/json"
	"fmt"
	"os"
)

// Token is an alias for tokens.Token to simplify usage in the parser
type Token = tokens.Token

// tokenInfo represents the JSON structure of a token
// This matches the structure saved by the lexer
type tokenInfo struct {
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// tokenTypeFromString converts a string token type to TokenType
func tokenTypeFromString(s string) tokens.TokenType {
	switch s {
	case "IDENTIFIER":
		return tokens.TOKEN_IDENTIFIER
	case "NUMBER":
		return tokens.TOKEN_NUMBER
	case "STRING":
		return tokens.TOKEN_STRING
	case "PLUS":
		return tokens.TOKEN_PLUS
	case "MINUS":
		return tokens.TOKEN_MINUS
	case "MULTIPLY":
		return tokens.TOKEN_MULTIPLY
	case "DIVIDE":
		return tokens.TOKEN_DIVIDE
	case "MODULO":
		return tokens.TOKEN_MODULO
	case "ASSIGN":
		return tokens.TOKEN_ASSIGN
	case "EQUAL":
		return tokens.TOKEN_EQUAL
	case "NOT_EQUAL":
		return tokens.TOKEN_NOT_EQUAL
	case "LESS":
		return tokens.TOKEN_LESS
	case "LESS_EQUAL":
		return tokens.TOKEN_LESS_EQUAL
	case "GREATER":
		return tokens.TOKEN_GREATER
	case "GREATER_EQUAL":
		return tokens.TOKEN_GREATER_EQUAL
	case "AND":
		return tokens.TOKEN_AND
	case "OR":
		return tokens.TOKEN_OR
	case "NOT":
		return tokens.TOKEN_NOT
	case "LPAREN":
		return tokens.TOKEN_LPAREN
	case "RPAREN":
		return tokens.TOKEN_RPAREN
	case "LBRACE":
		return tokens.TOKEN_LBRACE
	case "RBRACE":
		return tokens.TOKEN_RBRACE
	case "LBRACKET":
		return tokens.TOKEN_LBRACKET
	case "RBRACKET":
		return tokens.TOKEN_RBRACKET
	case "COMMA":
		return tokens.TOKEN_COMMA
	case "COLON":
		return tokens.TOKEN_COLON
	case "SEMICOLON":
		return tokens.TOKEN_SEMICOLON
	case "DOT":
		return tokens.TOKEN_DOT
	case "FUNC":
		return tokens.TOKEN_RESERVED_FUNC
	case "VAR":
		return tokens.TOKEN_RESERVED_VAR
	case "CONST":
		return tokens.TOKEN_RESERVED_CONST
	case "IF":
		return tokens.TOKEN_RESERVED_IF
	case "ELSE":
		return tokens.TOKEN_RESERVED_ELSE
	case "ELIF":
		return tokens.TOKEN_RESERVED_ELIF
	case "FOR":
		return tokens.TOKEN_RESERVED_FOR
	case "IN":
		return tokens.TOKEN_RESERVED_IN
	case "RANGE":
		return tokens.TOKEN_RESERVED_RANGE
	case "RETURN":
		return tokens.TOKEN_RESERVED_RETURN
	case "PRINT":
		return tokens.TOKEN_RESERVED_PRINT
	case "INT":
		return tokens.TOKEN_RESERVED_INT
	case "FLOAT":
		return tokens.TOKEN_RESERVED_FLOAT
	case "STR":
		return tokens.TOKEN_RESERVED_STR
	case "BOOL":
		return tokens.TOKEN_RESERVED_BOOL
	case "UNDEFINED":
		return tokens.TOKEN_RESERVED_UNDEFINED
	case "NUM":
		return tokens.TOKEN_RESERVED_NUM
	case "LENGTH":
		return tokens.TOKEN_RESERVED_LENGTH
	case "INPUT":
		return tokens.TOKEN_RESERVED_INPUT
	case "WHILE":
		return tokens.TOKEN_RESERVED_WHILE
	case "ARRAY":
		return tokens.TOKEN_RESERVED_ARRAY
	case "COMMENT":
		return tokens.TOKEN_COMMENT
	case "EOF":
		return tokens.TOKEN_EOF
	default:
		return tokens.TOKEN_INVALID
	}
}

// tokensLexer is a simple lexer implementation that uses pre-parsed tokens
type tokensLexer struct {
	tokens []Token
	index  int
}

// Parser representa el analizador sintáctico
// Puedes expandir esta estructura para guardar el AST o información de errores
// y el archivo fuente para mensajes de error descriptivos

type Parser struct {
	tokens     []Token
	pos        int
	sourceFile string
}

// NewParserFromFile crea un nuevo parser leyendo los tokens desde un archivo JSON y usando el nombre del archivo fuente para los mensajes de error
func NewParserFromFile(tokensFile string, sourceFile string) (*Parser, error) {
	file, err := os.Open(tokensFile)
	if err != nil {
		return nil, fmt.Errorf("no se pudo abrir el archivo de tokens: %v", err)
	}
	defer file.Close()

	var tokensInfo []tokenInfo
	if err := json.NewDecoder(file).Decode(&tokensInfo); err != nil {
		return nil, fmt.Errorf("error al decodificar el archivo de tokens: %v", err)
	}

	tokens := make([]Token, 0, len(tokensInfo))
	for _, t := range tokensInfo {
		tokens = append(tokens, Token{
			Type:    tokenTypeFromString(t.Type),
			Lexeme:  t.Lexeme,
			Literal: t.Literal,
			Line:    t.Line,
			Column:  t.Column,
		})
	}

	return &Parser{tokens: tokens, pos: 0, sourceFile: sourceFile}, nil
}

// SetSourceFile permite establecer el nombre del archivo fuente para mensajes de error
func (p *Parser) SetSourceFile(filename string) {
	p.sourceFile = filename
}

// nextToken devuelve el siguiente token y avanza la posición
func (p *Parser) nextToken() Token {
	if p.pos < len(p.tokens) {
		tok := p.tokens[p.pos]
		p.pos++
		return tok
	}
	return Token{Type: tokens.TOKEN_EOF}
}

// peekToken devuelve el siguiente token sin avanzar la posición
func (p *Parser) peekToken() Token {
	if p.pos < len(p.tokens) {
		return p.tokens[p.pos]
	}
	return Token{Type: tokens.TOKEN_EOF}
}

// expectToken verifica que el siguiente token sea del tipo esperado
func (p *Parser) expectToken(expected tokens.TokenType) (Token, error) {
	tok := p.nextToken()
	if tok.Type != expected {
		return tok, fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba %v, se encontró %v", p.sourceFile, tok.Line, tok.Column, expected, tok.Type)
	}
	return tok, nil
}

// Parse inicia el análisis sintáctico del programa
func (p *Parser) Parse() (interface{}, error) {
	for p.peekToken().Type != tokens.TOKEN_EOF {
		err := p.parseStatement()
		if err != nil {
			return nil, err
		}
	}
	return nil, nil
}

// parseStatement analiza cualquier sentencia válida según la BNF
func (p *Parser) parseStatement() error {
	tok := p.peekToken()
	switch tok.Type {
	case tokens.TOKEN_RESERVED_FUNC:
		return p.parseFunction()
	case tokens.TOKEN_RESERVED_VAR, tokens.TOKEN_RESERVED_CONST:
		return p.parseVarDecl()
	case tokens.TOKEN_RESERVED_IF:
		return p.parseIf()
	case tokens.TOKEN_RESERVED_FOR:
		return p.parseFor()
	case tokens.TOKEN_RESERVED_WHILE:
		return p.parseWhile()
	case tokens.TOKEN_RESERVED_RETURN:
		p.nextToken()
		// return puede tener expresión opcional
		tok2 := p.peekToken()
		if tok2.Type != tokens.TOKEN_RBRACE && tok2.Type != tokens.TOKEN_EOF {
			p.parseExpression() // consumir expresión completa
		}
		return nil
	case tokens.TOKEN_LBRACE:
		return p.parseBlock()
	case tokens.TOKEN_IDENTIFIER:
		p.nextToken() // consumir el identificador
		tok2 := p.peekToken()
		// Si después de un identificador viene un tipo sin ':', es error
		if tok2.Type == tokens.TOKEN_RESERVED_INT || tok2.Type == tokens.TOKEN_RESERVED_FLOAT || tok2.Type == tokens.TOKEN_RESERVED_STR || tok2.Type == tokens.TOKEN_RESERVED_BOOL || tok2.Type == tokens.TOKEN_RESERVED_UNDEFINED || tok2.Type == tokens.TOKEN_RESERVED_ARRAY || tok2.Type == tokens.TOKEN_RESERVED_NUM {
			return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba ':' antes del tipo de variable", p.sourceFile, tok2.Line, tok2.Column)
		}
		if tok2.Type == tokens.TOKEN_COLON {
			p.nextToken() // consumir ':'
			typeTok := p.peekToken()
			if typeTok.Type == tokens.TOKEN_ASSIGN {
				return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba tipo de variable después de ':', pero se encontró '='", p.sourceFile, typeTok.Line, typeTok.Column)
			}
			typeTok = p.nextToken()
			if typeTok.Type != tokens.TOKEN_IDENTIFIER &&
				typeTok.Type != tokens.TOKEN_RESERVED_INT &&
				typeTok.Type != tokens.TOKEN_RESERVED_FLOAT &&
				typeTok.Type != tokens.TOKEN_RESERVED_STR &&
				typeTok.Type != tokens.TOKEN_RESERVED_BOOL &&
				typeTok.Type != tokens.TOKEN_RESERVED_UNDEFINED &&
				typeTok.Type != tokens.TOKEN_RESERVED_ARRAY &&
				typeTok.Type != tokens.TOKEN_RESERVED_NUM {
				return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba tipo de variable después de ':', se encontró %v", p.sourceFile, typeTok.Line, typeTok.Column, typeTok.Type)
			}
			tok3 := p.peekToken()
			if tok3.Type == tokens.TOKEN_ASSIGN {
				p.nextToken()       // consumir '='
				p.parseExpression() // consumir expresión completa
			} else {
				return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba '=' después del tipo de variable en declaración corta", p.sourceFile, tok3.Line, tok3.Column)
			}
			return nil
		} else if tok2.Type == tokens.TOKEN_ASSIGN {
			p.nextToken()       // consumir '='
			p.parseExpression() // consumir expresión completa
			return nil
		} else if tok2.Type == tokens.TOKEN_LPAREN {
			// llamada a función
			for p.peekToken().Type != tokens.TOKEN_RPAREN && p.peekToken().Type != tokens.TOKEN_EOF {
				p.parseExpression()
			}
			_, err := p.expectToken(tokens.TOKEN_RPAREN)
			return err
		} else {
			// Si no es ninguno de los anteriores, tratar como expresión
			p.parseExpression()
			return nil
		}
	case tokens.TOKEN_RESERVED_PRINT:
		p.nextToken()
		// Consumir la expresión a imprimir
		p.nextToken()
		return nil
	default:
		p.nextToken()
		return nil
	}
}

// parseBlock analiza un bloque de código { ... }
func (p *Parser) parseBlock() error {
	_, err := p.expectToken(tokens.TOKEN_LBRACE)
	if err != nil {
		return err
	}
	for p.peekToken().Type != tokens.TOKEN_RBRACE && p.peekToken().Type != tokens.TOKEN_EOF {
		err := p.parseStatement()
		if err != nil {
			return err
		}
	}
	_, err = p.expectToken(tokens.TOKEN_RBRACE)
	return err
}

// parseBlockOrSingleStatement acepta un bloque { ... } o una sola sentencia
func (p *Parser) parseBlockOrSingleStatement() error {
	tok := p.peekToken()
	if tok.Type == tokens.TOKEN_LBRACE {
		return p.parseBlock()
	}
	return p.parseStatement()
}

// parseIf analiza una sentencia if/elif/else
func (p *Parser) parseIf() error {
	_, err := p.expectToken(tokens.TOKEN_RESERVED_IF)
	if err != nil {
		return err
	}
	// Permitir condición: IDENTIFICADOR [OPERADOR_COMPARACIÓN VALOR] o llamada a función o literal booleano
	condTok := p.nextToken()
	// Permitir expresiones complejas como parte izquierda de la comparación
	if condTok.Type == tokens.TOKEN_IDENTIFIER || condTok.Type == tokens.TOKEN_RESERVED_INT || condTok.Type == tokens.TOKEN_RESERVED_FLOAT || condTok.Type == tokens.TOKEN_RESERVED_STR {
		nextTok := p.peekToken()
		// Si es llamada a función, consumir argumentos
		if nextTok.Type == tokens.TOKEN_LPAREN {
			p.nextToken() // consumir '('
			parenCount := 1
			for parenCount > 0 {
				tok := p.nextToken()
				if tok.Type == tokens.TOKEN_LPAREN {
					parenCount++
				} else if tok.Type == tokens.TOKEN_RPAREN {
					parenCount--
				} else if tok.Type == tokens.TOKEN_EOF {
					return fmt.Errorf("Error de sintaxis en %s:%d:%d: paréntesis sin cerrar en condición de if/elif", p.sourceFile, tok.Line, tok.Column)
				}
			}
			nextTok = p.peekToken()
		}
		// Ahora es obligatorio que haya un operador de comparación
		if nextTok.Type == tokens.TOKEN_EQUAL || nextTok.Type == tokens.TOKEN_NOT_EQUAL || nextTok.Type == tokens.TOKEN_GREATER || nextTok.Type == tokens.TOKEN_LESS || nextTok.Type == tokens.TOKEN_GREATER_EQUAL || nextTok.Type == tokens.TOKEN_LESS_EQUAL {
			p.nextToken() // consumir operador
			valTok := p.nextToken()
			if valTok.Type != tokens.TOKEN_IDENTIFIER && valTok.Type != tokens.TOKEN_STRING && valTok.Type != tokens.TOKEN_NUMBER {
				return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba un valor válido en la condición del if", p.sourceFile, valTok.Line, valTok.Column)
			}
		} else {
			return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba un operador de comparación en la condición del if", p.sourceFile, nextTok.Line, nextTok.Column)
		}
	} else if condTok.Type != tokens.TOKEN_NUMBER && condTok.Type != tokens.TOKEN_STRING {
		return fmt.Errorf("Error de sintaxis en %s:%d:%d: condición de if/elif inválida", p.sourceFile, condTok.Line, condTok.Column)
	}
	if err := p.parseBlockOrSingleStatement(); err != nil {
		return err
	}
	for p.peekToken().Type == tokens.TOKEN_RESERVED_ELIF {
		p.nextToken()
		condTok := p.nextToken()
		if condTok.Type == tokens.TOKEN_IDENTIFIER || condTok.Type == tokens.TOKEN_RESERVED_INT || condTok.Type == tokens.TOKEN_RESERVED_FLOAT || condTok.Type == tokens.TOKEN_RESERVED_STR {
			nextTok := p.peekToken()
			if nextTok.Type == tokens.TOKEN_LPAREN {
				p.nextToken() // consumir '('
				parenCount := 1
				for parenCount > 0 {
					tok := p.nextToken()
					if tok.Type == tokens.TOKEN_LPAREN {
						parenCount++
					} else if tok.Type == tokens.TOKEN_RPAREN {
						parenCount--
					} else if tok.Type == tokens.TOKEN_EOF {
						return fmt.Errorf("Error de sintaxis en %s:%d:%d: paréntesis sin cerrar en condición de elif", p.sourceFile, tok.Line, tok.Column)
					}
				}
				nextTok = p.peekToken()
			}
			// Ahora es obligatorio que haya un operador de comparación
			if nextTok.Type == tokens.TOKEN_EQUAL || nextTok.Type == tokens.TOKEN_NOT_EQUAL || nextTok.Type == tokens.TOKEN_GREATER || nextTok.Type == tokens.TOKEN_LESS || nextTok.Type == tokens.TOKEN_GREATER_EQUAL || nextTok.Type == tokens.TOKEN_LESS_EQUAL {
				p.nextToken() // consumir operador
				valTok := p.nextToken()
				if valTok.Type != tokens.TOKEN_IDENTIFIER && valTok.Type != tokens.TOKEN_STRING && valTok.Type != tokens.TOKEN_NUMBER {
					return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba un valor válido en la condición del elif", p.sourceFile, valTok.Line, valTok.Column)
				}
			} else {
				return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba un operador de comparación en la condición del elif", p.sourceFile, nextTok.Line, nextTok.Column)
			}
		} else if condTok.Type != tokens.TOKEN_NUMBER && condTok.Type != tokens.TOKEN_STRING {
			return fmt.Errorf("Error de sintaxis en %s:%d:%d: condición de elif inválida", p.sourceFile, condTok.Line, condTok.Column)
		}
		if err := p.parseBlockOrSingleStatement(); err != nil {
			return err
		}
	}
	if p.peekToken().Type == tokens.TOKEN_RESERVED_ELSE {
		p.nextToken()
		if err := p.parseBlockOrSingleStatement(); err != nil {
			return err
		}
	}
	return nil
}

// parseFor analiza un ciclo for
func (p *Parser) parseFor() error {
	_, err := p.expectToken(tokens.TOKEN_RESERVED_FOR)
	if err != nil {
		return err
	}
	// Consumir variable_declaration, in, range, (, expresión, )
	for {
		tok := p.peekToken()
		if tok.Type == tokens.TOKEN_LBRACE || tok.Type == tokens.TOKEN_EOF {
			break
		}
		p.nextToken()
	}
	return p.parseBlockOrSingleStatement()
}

// parseWhile analiza un ciclo while
func (p *Parser) parseWhile() error {
	_, err := p.expectToken(tokens.TOKEN_RESERVED_WHILE)
	if err != nil {
		return err
	}
	p.nextToken() // condición (simplificado)
	return p.parseBlockOrSingleStatement()
}

// parseFunction analiza la declaración de una función
func (p *Parser) parseFunction() error {
	_, err := p.expectToken(tokens.TOKEN_RESERVED_FUNC)
	if err != nil {
		return err
	}

	_, err = p.expectToken(tokens.TOKEN_IDENTIFIER)
	if err != nil {
		return err
	}

	_, err = p.expectToken(tokens.TOKEN_LPAREN)
	if err != nil {
		return err
	}
	// Analizar parámetros
	if err := p.parseParameters(); err != nil {
		return err
	}
	_, err = p.expectToken(tokens.TOKEN_RPAREN)
	if err != nil {
		return err
	}

	// Tipo de retorno opcional
	tok := p.peekToken()
	if tok.Type == tokens.TOKEN_COLON {
		p.nextToken() // consumir ':'
		typeTok := p.nextToken()
		if typeTok.Type != tokens.TOKEN_IDENTIFIER &&
			typeTok.Type != tokens.TOKEN_RESERVED_INT &&
			typeTok.Type != tokens.TOKEN_RESERVED_FLOAT &&
			typeTok.Type != tokens.TOKEN_RESERVED_STR &&
			typeTok.Type != tokens.TOKEN_RESERVED_BOOL &&
			typeTok.Type != tokens.TOKEN_RESERVED_UNDEFINED &&
			typeTok.Type != tokens.TOKEN_RESERVED_ARRAY &&
			typeTok.Type != tokens.TOKEN_RESERVED_NUM {
			return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba tipo de retorno después de ':', se encontró %v", p.sourceFile, typeTok.Line, typeTok.Column, typeTok.Type)
		}
	}

	return p.parseBlock()
}

// parseParameters analiza la lista de parámetros de una función
func (p *Parser) parseParameters() error {
	if p.peekToken().Type == tokens.TOKEN_RPAREN {
		return nil // Sin parámetros
	}
	for {
		_, err := p.expectToken(tokens.TOKEN_IDENTIFIER)
		if err != nil {
			return err
		}
		_, err = p.expectToken(tokens.TOKEN_COLON)
		if err != nil {
			return err
		}
		typeTok := p.nextToken()
		if typeTok.Type != tokens.TOKEN_IDENTIFIER &&
			typeTok.Type != tokens.TOKEN_RESERVED_INT &&
			typeTok.Type != tokens.TOKEN_RESERVED_FLOAT &&
			typeTok.Type != tokens.TOKEN_RESERVED_STR &&
			typeTok.Type != tokens.TOKEN_RESERVED_BOOL &&
			typeTok.Type != tokens.TOKEN_RESERVED_UNDEFINED &&
			typeTok.Type != tokens.TOKEN_RESERVED_ARRAY &&
			typeTok.Type != tokens.TOKEN_RESERVED_NUM {
			return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba tipo de parámetro, se encontró %v", p.sourceFile, typeTok.Line, typeTok.Column, typeTok.Type)
		}
		tok := p.peekToken()
		if tok.Type == tokens.TOKEN_COMMA {
			p.nextToken() // consumir la coma
			continue
		}
		break
	}
	return nil
}

// parseVarDecl analiza una declaración de variable o constante
func (p *Parser) parseVarDecl() error {
	p.nextToken() // VAR o CONST
	_, err := p.expectToken(tokens.TOKEN_IDENTIFIER)
	if err != nil {
		return err
	}
	_, err = p.expectToken(tokens.TOKEN_COLON)
	if err != nil {
		return err
	}
	typeTok := p.nextToken()
	if typeTok.Type != tokens.TOKEN_IDENTIFIER &&
		typeTok.Type != tokens.TOKEN_RESERVED_INT &&
		typeTok.Type != tokens.TOKEN_RESERVED_FLOAT &&
		typeTok.Type != tokens.TOKEN_RESERVED_STR &&
		typeTok.Type != tokens.TOKEN_RESERVED_BOOL &&
		typeTok.Type != tokens.TOKEN_RESERVED_UNDEFINED &&
		typeTok.Type != tokens.TOKEN_RESERVED_ARRAY &&
		typeTok.Type != tokens.TOKEN_RESERVED_NUM {
		return fmt.Errorf("Error de sintaxis en %s:%d:%d: se esperaba tipo de variable, se encontró %v", p.sourceFile, typeTok.Line, typeTok.Column, typeTok.Type)
	}
	tok := p.peekToken()
	if tok.Type == tokens.TOKEN_ASSIGN {
		p.nextToken()
		p.parseExpression() // consumir expresión completa
	}
	return nil
}

// parseExpression consume tokens hasta que encuentra un token de control de flujo, cierre de bloque o fin de expresión
func (p *Parser) parseExpression() {
	for {
		tok := p.peekToken()
		switch tok.Type {
		case tokens.TOKEN_EOF, tokens.TOKEN_RBRACE, tokens.TOKEN_RPAREN, tokens.TOKEN_RBRACKET,
			tokens.TOKEN_RESERVED_ELIF, tokens.TOKEN_RESERVED_ELSE, tokens.TOKEN_RESERVED_IF,
			tokens.TOKEN_RESERVED_FOR, tokens.TOKEN_RESERVED_WHILE, tokens.TOKEN_RESERVED_FUNC,
			tokens.TOKEN_RESERVED_VAR, tokens.TOKEN_RESERVED_CONST, tokens.TOKEN_RESERVED_RETURN,
			tokens.TOKEN_RESERVED_PRINT:
			return
		}
		p.nextToken()
	}
}
