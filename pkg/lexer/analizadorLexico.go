package lexer

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"

	"analizador/tokens"
)

// tokenCollector collects tokens during lexical analysis
type tokenCollector struct {
	tokens []tokens.Token
}

// AddToken adds a token to the collector
func (c *tokenCollector) AddToken(token tokens.Token) {
	c.tokens = append(c.tokens, token)
}

// GetTokens returns the collected tokens
func (c *tokenCollector) GetTokens() []tokens.Token {
	return c.tokens
}

// Lexer represents the lexical analyzer
type Lexer struct {
	source     string
	tokens     []tokens.Token
	current    int
	lineNumber int
}

// Global variables for reserved words
var (
	reservedTypes     = []string{"int", "float", "str", "bool", "undefined", "num"}
	reservedFunctions = []string{"print", "length", "input"}
)

// NewLexer creates a new Lexer instance
func NewLexer(source string) *Lexer {
	return &Lexer{
		source:     source,
		tokens:     make([]tokens.Token, 0),
		current:    0,
		lineNumber: 1,
	}
}

// NewLexerFromFile creates a new Lexer instance from a file
func NewLexerFromFile(filename string) *Lexer {
	content, err := os.ReadFile(filename)
	if err != nil {
		fmt.Printf("Error al leer el archivo: %v\n", err)
		return nil
	}
	return NewLexer(string(content))
}

// ScanTokens scans the source code and returns the list of tokens
func (l *Lexer) ScanTokens() ([]tokens.Token, error) {
	scanner := bufio.NewScanner(strings.NewReader(l.source))
	collector := &tokenCollector{}

	for scanner.Scan() {
		line := scanner.Text()
		l.analyzeLine(line, collector)
		l.lineNumber++
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error scanning source: %v", err)
	}

	// Add EOF token
	collector.tokens = append(collector.tokens, tokens.Token{
		Type:   tokens.TOKEN_EOF,
		Lexeme: "",
	})

	l.tokens = collector.tokens
	return l.tokens, nil
}

// NextToken returns the next token and advances the position
func (l *Lexer) NextToken() (tokens.Token, bool) {
	if l.current >= len(l.tokens) {
		return tokens.Token{Type: tokens.TOKEN_EOF}, false
	}
	token := l.tokens[l.current]
	l.current++
	return token, true
}

// PeekToken returns the next token without advancing the position
func (l *Lexer) PeekToken() (tokens.Token, bool) {
	if l.current >= len(l.tokens) {
		return tokens.Token{Type: tokens.TOKEN_EOF}, false
	}
	return l.tokens[l.current], true
}

// ResetTokenPos resets the token position to the beginning
func (l *Lexer) ResetTokenPos() {
	l.current = 0
}

// TokenFound represents a token found during lexical analysis
type TokenFound struct {
	Type    tokens.TokenType
	Lexeme  string
	Literal interface{}
	Line    int
	Column  int
}

// analyzeLine analyzes a single line of source code and adds tokens to the collector
func (l *Lexer) analyzeLine(line string, collector *tokenCollector) {
	// Map of operators to token types
	operators := map[string]tokens.TokenType{
		"+":  tokens.TOKEN_PLUS,
		"-":  tokens.TOKEN_MINUS,
		"*":  tokens.TOKEN_MULTIPLY,
		"/":  tokens.TOKEN_DIVIDE,
		"%":  tokens.TOKEN_MODULO,
		"=":  tokens.TOKEN_ASSIGN,
		"==": tokens.TOKEN_EQUAL,
		"!=": tokens.TOKEN_NOT_EQUAL,
		"<":  tokens.TOKEN_LESS,
		"<=": tokens.TOKEN_LESS_EQUAL,
		">":  tokens.TOKEN_GREATER,
		">=": tokens.TOKEN_GREATER_EQUAL,
		"&&": tokens.TOKEN_AND,
		"||": tokens.TOKEN_OR,
		"!":  tokens.TOKEN_NOT,
	}

	// Map of delimiters to token types
	delimiters := map[string]tokens.TokenType{
		"(": tokens.TOKEN_LPAREN,
		")": tokens.TOKEN_RPAREN,
		"{": tokens.TOKEN_LBRACE,
		"}": tokens.TOKEN_RBRACE,
		"[": tokens.TOKEN_LBRACKET,
		"]": tokens.TOKEN_RBRACKET,
		",": tokens.TOKEN_COMMA,
		";": tokens.TOKEN_SEMICOLON,
		":": tokens.TOKEN_COLON,
		".": tokens.TOKEN_DOT,
	}

	// Map of reserved words to token types
	keywords := map[string]tokens.TokenType{
		"var":       tokens.TOKEN_RESERVED_VAR,
		"const":     tokens.TOKEN_RESERVED_CONST,
		"if":        tokens.TOKEN_RESERVED_IF,
		"else":      tokens.TOKEN_RESERVED_ELSE,
		"elif":      tokens.TOKEN_RESERVED_ELIF,
		"for":       tokens.TOKEN_RESERVED_FOR,
		"in":        tokens.TOKEN_RESERVED_IN,
		"range":     tokens.TOKEN_RESERVED_RANGE,
		"func":      tokens.TOKEN_RESERVED_FUNC,
		"return":    tokens.TOKEN_RESERVED_RETURN,
		"print":     tokens.TOKEN_RESERVED_PRINT,
		"int":       tokens.TOKEN_RESERVED_INT,
		"float":     tokens.TOKEN_RESERVED_FLOAT,
		"str":       tokens.TOKEN_RESERVED_STR,
		"bool":      tokens.TOKEN_RESERVED_BOOL,
		"undefined": tokens.TOKEN_RESERVED_UNDEFINED,
		"num":       tokens.TOKEN_RESERVED_NUM,
		"length":    tokens.TOKEN_RESERVED_LENGTH,
		"input":     tokens.TOKEN_RESERVED_INPUT,
	}

	// Process the line character by character
	var currentToken strings.Builder
	inString := false
	escapeNext := false
	stringDelim := byte(0)

	for i := 0; i < len(line); i++ {
		ch := line[i]

		// Handle string literals
		if inString {
			if escapeNext {
				switch ch {
				case 'n':
					currentToken.WriteByte('\n')
				case 't':
					currentToken.WriteByte('\t')
				case 'r':
					currentToken.WriteByte('\r')
				default:
					currentToken.WriteByte(ch)
				}
				escapeNext = false
				continue
			}

			if ch == '\\' {
				escapeNext = true
				continue
			}

			if ch == stringDelim {
				// End of string literal
				collector.AddToken(tokens.Token{
					Type:    tokens.TOKEN_STRING,
					Lexeme:  currentToken.String(),
					Literal: currentToken.String(),
				})
				currentToken.Reset()
				inString = false
				continue
			}

			currentToken.WriteByte(ch)
			continue
		}

		// Check for start of string
		if ch == '"' || ch == '\'' {
			inString = true
			stringDelim = ch
			continue
		}

		// Skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
			if currentToken.Len() > 0 {
				tokenStr := currentToken.String()
				// Check for keywords and identifiers
				if tokenType, isKeyword := keywords[tokenStr]; isKeyword {
					collector.AddToken(tokens.Token{
						Type:   tokenType,
						Lexeme: tokenStr,
					})
				} else if _, isOp := operators[tokenStr]; isOp {
					collector.AddToken(tokens.Token{
						Type:   operators[tokenStr],
						Lexeme: tokenStr,
					})
				} else if _, isDelim := delimiters[tokenStr]; isDelim {
					collector.AddToken(tokens.Token{
						Type:   delimiters[tokenStr],
						Lexeme: tokenStr,
					})
				} else {
					// Try to parse as number
					if _, err := strconv.ParseFloat(tokenStr, 64); err == nil {
						collector.AddToken(tokens.Token{
							Type:    tokens.TOKEN_NUMBER,
							Lexeme:  tokenStr,
							Literal: tokenStr,
						})
					} else {
						// Default to identifier
						collector.AddToken(tokens.Token{
							Type:   tokens.TOKEN_IDENTIFIER,
							Lexeme: tokenStr,
						})
					}
				}
				currentToken.Reset()
			}
			continue
		}

		// Check for comments
		if ch == '#' {
			// The rest of the line is a comment
			comment := line[i:]
			collector.AddToken(tokens.Token{
				Type:   tokens.TOKEN_COMMENT,
				Lexeme: comment,
			})
			break
		}

		// Check for operators and delimiters
		currentToken.WriteByte(ch)
		tokenStr := currentToken.String()

		// Check for multi-character operators first
		if i+1 < len(line) {
			twoCharOp := tokenStr + string(line[i+1])
			if _, exists := operators[twoCharOp]; exists {
				collector.AddToken(tokens.Token{
					Type:   operators[twoCharOp],
					Lexeme: twoCharOp,
				})
				currentToken.Reset()
				i++ // Skip next character since we've processed it
				continue
			}
		}

		// Check for single-character operators and delimiters
		if tokenType, isOp := operators[tokenStr]; isOp {
			collector.AddToken(tokens.Token{
				Type:   tokenType,
				Lexeme: tokenStr,
			})
			currentToken.Reset()
		} else if delimType, isDelim := delimiters[tokenStr]; isDelim {
			collector.AddToken(tokens.Token{
				Type:   delimType,
				Lexeme: tokenStr,
			})
			currentToken.Reset()
		}
	}

	// Add any remaining token
	if currentToken.Len() > 0 {
		tokenStr := currentToken.String()
		if tokenType, isKeyword := keywords[tokenStr]; isKeyword {
			collector.AddToken(tokens.Token{
				Type:   tokenType,
				Lexeme: tokenStr,
			})
		} else if _, isOp := operators[tokenStr]; isOp {
			collector.AddToken(tokens.Token{
				Type:   operators[tokenStr],
				Lexeme: tokenStr,
			})
		} else if _, isDelim := delimiters[tokenStr]; isDelim {
			collector.AddToken(tokens.Token{
				Type:   delimiters[tokenStr],
				Lexeme: tokenStr,
			})
		} else {
			// Try to parse as number
			if _, err := strconv.ParseFloat(tokenStr, 64); err == nil {
				collector.AddToken(tokens.Token{
					Type:    tokens.TOKEN_NUMBER,
					Lexeme:  tokenStr,
					Literal: tokenStr,
				})
			} else {
				// Default to identifier
				collector.AddToken(tokens.Token{
					Type:   tokens.TOKEN_IDENTIFIER,
					Lexeme: tokenStr,
				})
			}
		}
	}

	// Add end-of-line token
}

// TokenInfo represents the information of a token for JSON output
type TokenInfo struct {
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// SaveTokensToFile saves tokens to a JSON file
func (l *Lexer) SaveTokensToFile(filename string) error {
	// Create a slice of TokenInfo from the tokens
	var tokensInfo []TokenInfo
	for _, t := range l.tokens {
		tokensInfo = append(tokensInfo, TokenInfo{
			Type:    t.Type.String(), // Convertir TokenType a string
			Lexeme:  t.Lexeme,
			Literal: t.Literal,
			Line:    t.Line,
			Column:  t.Column,
		})
	}

	// Create the file
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("error al crear el archivo: %v", err)
	}
	defer file.Close()

	// Convertir tokens a JSON
	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "  ") // Formato legible
	if err := encoder.Encode(tokensInfo); err != nil {
		return fmt.Errorf("error al codificar tokens a JSON: %v", err)
	}

	return nil
}

// isLetter checks if a byte is a letter or underscore
func isLetter(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

// isDigit checks if a byte is a digit
func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

// isIdentifier checks if a string is a valid identifier
func isIdentifier(token string) bool {
	if token == "" {
		return false
	}

	// Check if first character is a letter or underscore
	firstChar := token[0]
	if !((firstChar >= 'a' && firstChar <= 'z') || (firstChar >= 'A' && firstChar <= 'Z') || firstChar == '_') {
		return false
	}

	// Check remaining characters
	for i := 1; i < len(token); i++ {
		c := token[i]
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') {
			return false
		}
	}

	// Check if it's a reserved word
	if contains(reservedTypes, token) || contains(reservedFunctions, token) {
		return false
	}

	return true
}

// contains checks if a string is present in a slice
func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}
