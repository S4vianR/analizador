package tokens

// Lexer defines the interface that a lexer must implement
type Lexer interface {
	NextToken() (Token, bool)
	PeekToken() (Token, bool)
	ResetTokenPos()
}

// TokenType represents the type of a token
type TokenType int

// Token represents a token in the source code
type Token struct {
	Type    TokenType
	Lexeme  string
	Literal any
	Line    int
	Column  int
}

// Token types
const (
	TOKEN_INVALID TokenType = iota
	TOKEN_IDENTIFIER
	TOKEN_NUMBER
	TOKEN_STRING
	TOKEN_PLUS
	TOKEN_MINUS
	TOKEN_MULTIPLY
	TOKEN_DIVIDE
	TOKEN_MODULO
	TOKEN_ASSIGN
	TOKEN_EQUAL
	TOKEN_NOT_EQUAL
	TOKEN_LESS
	TOKEN_LESS_EQUAL
	TOKEN_GREATER
	TOKEN_GREATER_EQUAL
	TOKEN_AND
	TOKEN_OR
	TOKEN_NOT
	TOKEN_LPAREN
	TOKEN_RPAREN
	TOKEN_LBRACE
	TOKEN_RBRACE
	TOKEN_LBRACKET
	TOKEN_RBRACKET
	TOKEN_COMMA
	TOKEN_COLON
	TOKEN_SEMICOLON
	TOKEN_DOT
	TOKEN_RESERVED_VAR
	TOKEN_RESERVED_CONST
	TOKEN_RESERVED_IF
	TOKEN_RESERVED_ELSE
	TOKEN_RESERVED_ELIF
	TOKEN_RESERVED_FOR
	TOKEN_RESERVED_IN
	TOKEN_RESERVED_RANGE
	TOKEN_RESERVED_FUNC
	TOKEN_RESERVED_RETURN
	TOKEN_RESERVED_PRINT
	TOKEN_RESERVED_INT
	TOKEN_RESERVED_FLOAT
	TOKEN_RESERVED_STR
	TOKEN_RESERVED_BOOL
	TOKEN_RESERVED_UNDEFINED
	TOKEN_RESERVED_NUM
	TOKEN_RESERVED_LENGTH
	TOKEN_RESERVED_INPUT
	TOKEN_COMMENT
	TOKEN_EOF
)

// String returns the string representation of the token type
func (t TokenType) String() string {
	switch t {
	case TOKEN_IDENTIFIER:
		return "IDENTIFIER"
	case TOKEN_NUMBER:
		return "NUMBER"
	case TOKEN_STRING:
		return "STRING"
	case TOKEN_PLUS:
		return "PLUS"
	case TOKEN_MINUS:
		return "MINUS"
	case TOKEN_MULTIPLY:
		return "MULTIPLY"
	case TOKEN_DIVIDE:
		return "DIVIDE"
	case TOKEN_MODULO:
		return "MODULO"
	case TOKEN_ASSIGN:
		return "ASSIGN"
	case TOKEN_EQUAL:
		return "EQUAL"
	case TOKEN_NOT_EQUAL:
		return "NOT_EQUAL"
	case TOKEN_LESS:
		return "LESS"
	case TOKEN_LESS_EQUAL:
		return "LESS_EQUAL"
	case TOKEN_GREATER:
		return "GREATER"
	case TOKEN_GREATER_EQUAL:
		return "GREATER_EQUAL"
	case TOKEN_AND:
		return "AND"
	case TOKEN_OR:
		return "OR"
	case TOKEN_NOT:
		return "NOT"
	case TOKEN_LPAREN:
		return "LPAREN"
	case TOKEN_RPAREN:
		return "RPAREN"
	case TOKEN_LBRACE:
		return "LBRACE"
	case TOKEN_RBRACE:
		return "RBRACE"
	case TOKEN_LBRACKET:
		return "LBRACKET"
	case TOKEN_RBRACKET:
		return "RBRACKET"
	case TOKEN_COMMA:
		return "COMMA"
	case TOKEN_COLON:
		return "COLON"
	case TOKEN_SEMICOLON:
		return "SEMICOLON"
	case TOKEN_DOT:
		return "DOT"
	case TOKEN_RESERVED_VAR:
		return "VAR"
	case TOKEN_RESERVED_CONST:
		return "CONST"
	case TOKEN_RESERVED_IF:
		return "IF"
	case TOKEN_RESERVED_ELSE:
		return "ELSE"
	case TOKEN_RESERVED_ELIF:
		return "ELIF"
	case TOKEN_RESERVED_FOR:
		return "FOR"
	case TOKEN_RESERVED_IN:
		return "IN"
	case TOKEN_RESERVED_RANGE:
		return "RANGE"
	case TOKEN_RESERVED_FUNC:
		return "FUNC"
	case TOKEN_RESERVED_RETURN:
		return "RETURN"
	case TOKEN_RESERVED_PRINT:
		return "PRINT"
	case TOKEN_RESERVED_INT:
		return "INT"
	case TOKEN_RESERVED_FLOAT:
		return "FLOAT"
	case TOKEN_RESERVED_STR:
		return "STR"
	case TOKEN_RESERVED_BOOL:
		return "BOOL"
	case TOKEN_RESERVED_UNDEFINED:
		return "UNDEFINED"
	case TOKEN_RESERVED_NUM:
		return "NUM"
	case TOKEN_RESERVED_LENGTH:
		return "LENGTH"
	case TOKEN_RESERVED_INPUT:
		return "INPUT"
	case TOKEN_COMMENT:
		return "COMMENT"
	case TOKEN_EOF:
		return "EOF"
	default:
		return "INVALID"
	}
}
