package parser

import (
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"

	tokens "analizador/tokens"
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
	tokens     []TokenInfoSintax
	current    int
	errors     []string
	sourceFile string // Path to the source file being parsed
}

// NewParser creates a new Parser instance with the given tokens
func NewParser(tokens []TokenInfoSintax) (*Parser, error) {
	if len(tokens) == 0 {
		return nil, fmt.Errorf("no tokens provided")
	}

	// Filter out comments and whitespace tokens
	filteredTokens := make([]TokenInfoSintax, 0, len(tokens))
	for _, token := range tokens {
		tokenType := token.Type
		if tokenType != "COMMENT" && tokenType != "WHITESPACE" {
			filteredTokens = append(filteredTokens, token)
		}
	}

	return &Parser{
		tokens:  filteredTokens,
		current: 0,
		errors:  []string{},
	}, nil
}

// SetSourceFile sets the path to the source file being parsed
func (p *Parser) SetSourceFile(path string) {
	p.sourceFile = path
}

// getCurrentSourceFile returns the path to the current source file
func (p *Parser) getCurrentSourceFile() string {
	return p.sourceFile
}

// NewParserFromFile creates a new Parser instance from a JSON file containing tokens.
// It reads the file, parses the JSON data, and returns a new Parser instance.
// If there's an error reading or parsing the file, it returns an error.
func NewParserFromFile(filename string) (*Parser, error) {
	// Read the file
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("error reading file %s: %w", filename, err)
	}

	// Unmarshal the JSON data into a slice of RawToken
	var rawTokens []RawToken
	if err := json.Unmarshal(data, &rawTokens); err != nil {
		return nil, fmt.Errorf("error parsing JSON: %w", err)
	}

	// Convert RawToken slice to TokenInfoSintax slice
	tokens := make([]TokenInfoSintax, 0, len(rawTokens))
	for _, rt := range rawTokens {
		// Skip comments and whitespace tokens during parsing
		tokenType := rt.Type
		if tokenType == "COMMENT" || tokenType == "WHITESPACE" {
			continue
		}

		tokens = append(tokens, TokenInfoSintax{
			Type:    rt.Type,
			Lexeme:  rt.Lexeme,
			Literal: rt.Literal,
			Line:    rt.Line,
			Column:  rt.Column,
		})
	}

	if len(tokens) == 0 {
		return nil, fmt.Errorf("no valid tokens found in file %s", filename)
	}

	// Create a new parser with the filtered tokens
	parser := &Parser{
		tokens:  tokens,
		current: 0,
		errors:  []string{},
	}

	// Set the source file path for better error messages
	parser.SetSourceFile(filename)

	return parser, nil
}

// RawToken represents the token structure from the lexer output
type RawToken struct {
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// Parse starts the syntax analysis and returns the parsed program AST
func (p *Parser) Parse() (interface{}, error) {
	// Start parsing from the first token
	p.current = 0
	p.errors = p.errors[:0] // Clear any previous errors

	// Parse the entire program
	program := p.program()

	// If we have any errors, return the first one
	if len(p.errors) > 0 {
		return nil, fmt.Errorf("syntax error: %s", p.errors[0])
	}

	fmt.Println("Syntax analysis completed successfully")
	return program, nil
}

// skipComments advances past any comment tokens
func (p *Parser) skipComments() {
    for !p.isAtEnd() && p.peek().Type == tokens.TOKEN_COMMENT.String() {
        p.advance()
    }
}

// program -> declaration*
func (p *Parser) program() interface{} {
	// Skip any leading comments
	p.skipComments()

	// Parse all declarations in the program
	var declarations []interface{}
	for !p.isAtEnd() {
		// Skip any comments between declarations
		p.skipComments()
		if p.isAtEnd() {
			break
		}

		// Parse the next declaration
		if decl := p.declaration(); decl != nil {
			declarations = append(declarations, decl)
		}

		// Skip any comments after the declaration
		p.skipComments()
	}

	// Return the program AST
	return map[string]interface{}{
		"type":         "program",
		"declarations": declarations,
	}
}

// declaration -> funcDecl | varDecl | statement
func (p *Parser) declaration() interface{} {
	p.skipComments()
	if p.isAtEnd() {
		return nil
	}

	// Check for function declaration
	if p.match(tokens.TOKEN_FUNC) {
		return p.function()
	}

	// Check for variable/constant declaration
	if p.match(tokens.TOKEN_VAR) || p.match(tokens.TOKEN_CONST) {
		return p.varDeclaration()
	}

	// Otherwise, parse as a statement
	return p.statement()
}

// function -> "func" IDENTIFIER "(" parameters? ")" block
func (p *Parser) function() interface{} {
	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error("Expected function name")
		return nil
	}
	name := p.previous()

	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Expected '(' after function name")
		return nil
	}

	// Parse parameters if they exist
	params := []map[string]interface{}{}
	if !p.check(tokens.TOKEN_RPAREN) {
		params = p.parameters()
	}

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Expected ')' after parameters")
		return nil
	}

	// Parse function body
	body := p.block()
	if body == nil {
		p.error("Expected function body")
		return nil
	}

	// Return the function declaration
	return map[string]interface{}{
		"type":   "function_declaration",
		"name":   name.Lexeme,
		"params": params,
		"body":   body,
	}
}

// parameters -> IDENTIFIER ":" type ("," IDENTIFIER ":" type)*
func (p *Parser) parameters() []map[string]interface{} {
	params := []map[string]interface{}{}
	
	for {
		if !p.match(tokens.TOKEN_IDENTIFIER) {
			p.error("Expected parameter name")
			return params
		}
		name := p.previous()

		if !p.match(tokens.TOKEN_COLON) {
			p.error("Expected ':' after parameter name")
			return params
		}

		if !p.isType() {
			p.error("Expected parameter type")
			return params
		}
		paramType := p.previous()

		// Add the parameter to the list
		params = append(params, map[string]interface{}{
			"name": name.Lexeme,
			"type": paramType.Lexeme,
		})

		// If there's no comma, we're done
		if !p.match(tokens.TOKEN_COMMA) {
			break
		}
	}

	return params
}

// varDeclaration -> ("var" | "const") IDENTIFIER ":" type ("=" expression)? ";"
func (p *Parser) varDeclaration() interface{} {
	// Get the declaration type (var or const)
	declToken := p.previous()
	isConst := declToken.Lexeme == "const"

	// Get the variable name
	if p.isAtEnd() {
		p.error("Expected identifier after '" + declToken.Lexeme + "'")
		return nil
	}

	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error(fmt.Sprintf("Expected variable name after '%s', got: %s", 
			declToken.Lexeme, p.peek().Lexeme))
		return nil
	}
	name := p.previous()

	// Get the type annotation
	if !p.match(tokens.TOKEN_COLON) {
		p.error(fmt.Sprintf("Expected ':' after variable name '%s'", name.Lexeme))
		return nil
	}

	if !p.isType() {
		p.error(fmt.Sprintf("Expected type after ':', got: %s", p.peek().Lexeme))
		return nil
	}
	typeToken := p.previous()

	// Check for optional initialization
	var initializer interface{}
	if p.match(tokens.TOKEN_ASSIGN) {
		// Parse the expression
		if p.isAtEnd() {
			p.error("Expected expression after '='")
			return nil
		}
		initializer = p.expression()
		if initializer == nil {
			p.error("Expected expression after '='")
			return nil
		}
	} else if isConst {
		p.error(fmt.Sprintf("Constant '%s' must be initialized", name.Lexeme))
		return nil
	}

	// Expect a semicolon
	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error(fmt.Sprintf("Expected ';' after variable declaration '%s'", name.Lexeme))
		return nil
	}

	// Return the variable declaration
	return map[string]interface{}{
		"type":        "variable_declaration",
		"declaration": declToken.Lexeme,
		"name":        name.Lexeme,
		"var_type":    typeToken.Lexeme,
		"initializer": initializer,
	}
}

// statement handles different types of statements
func (p *Parser) statement() interface{} {
	// Skip any leading comments
	p.skipComments()

	switch {
	case p.match(tokens.TOKEN_PRINT):
		stmt := p.printStatement()
		if stmt == nil {
			return nil
		}
		return stmt
	case p.match(tokens.TOKEN_RETURN):
		stmt := p.returnStatement()
		if stmt == nil {
			return nil
		}
		return stmt
	case p.match(tokens.TOKEN_IF):
		stmt := p.ifStatement()
		if stmt == nil {
			return nil
		}
		return stmt
	case p.match(tokens.TOKEN_WHILE):
		stmt := p.whileStatement()
		if stmt == nil {
			return nil
		}
		return stmt
	case p.match(tokens.TOKEN_FOR):
		stmt := p.forStatement()
		if stmt == nil {
			return nil
		}
		return stmt
	case p.match(tokens.TOKEN_LBRACE):
		block := p.block()
		if block == nil {
			return nil
		}
		return block
	default:
		exprStmt := p.expressionStatement()
		if exprStmt == nil {
			return nil
		}
		return exprStmt
	}
}

// ifStmt -> "if" expression block ("elif" expression block)* ("else" block)?
func (p *Parser) ifStatement() interface{} {
	if !p.match(tokens.TOKEN_IF) {
		p.error("Expected 'if' keyword")
		return
	}

	// Parse the condition
	condition := p.expression()
	if condition == nil {
		p.error("expected condition after 'if'")
		return
	}

	// Parse the then branch
	thenBranch := p.block()
	if thenBranch == nil {
		p.error("Expected block after 'if' condition")
		return
	}

	// Handle 'elif' branches
	var elifBranches []map[string]interface{}
	for p.match(tokens.TOKEN_ELIF) {
		elifCondition := p.expression()
		if elifCondition == nil {
			p.error("expected condition after 'elif'")
			return
		}

		elifBranch := p.block()
		if elifBranch == nil {
			p.error("expected block after 'elif' condition")
			return
		}

		elifBranches = append(elifBranches, map[string]interface{}{
			"condition": elifCondition,
			"branch":    elifBranch,
		})
	}

	// Handle 'else' branch
	var elseBranch interface{}
	if p.match(tokens.TOKEN_ELSE) {
		elseBranch = p.block()
		if elseBranch == nil {
			p.error("expected block after 'else'")
			return
		}
	}

	// Build the if statement
	_ = map[string]interface{}{
		"type":         "if_statement",
		"condition":    condition,
		"then_branch":  thenBranch,
		"elif_branches": elifBranches,
		"else_branch":  elseBranch,
	}
}

// forStmt -> "for" "var" IDENTIFIER ":" "int" "=" expression "in" "range" "(" expression ")" block
func (p *Parser) forStatement() {
	if !p.match(tokens.TOKEN_FOR) {
		p.error("Expected 'for'")
		return
	}

	if !p.match(tokens.TOKEN_VAR) {
		p.error("Expected 'var' after 'for'")
		return
	}

	if !p.match(tokens.TOKEN_IDENTIFIER) {
		p.error("Expected variable identifier")
		return
	}
	varName := p.previous()

	if !p.match(tokens.TOKEN_COLON) {
		p.error("Expected ':' after identifier")
		return
	}

	if !p.match(tokens.TOKEN_INT) {
		p.error("Expected 'int' as variable type")
		return
	}

	if !p.match(tokens.TOKEN_ASSIGN) {
		p.error("Expected '=' after type")
		return
	}

	initializer := p.expression()
	if initializer == nil {
		p.error("Expected initializer expression")
		return
	}

	if !p.match(tokens.TOKEN_IN) {
		p.error("Expected 'in' after expression")
		return
	}

	if !p.match(tokens.TOKEN_RANGE) {
		p.error("Expected 'range' after 'in'")
		return
	}

	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Expected '(' after 'range'")
		return
	}

	rangeExpr := p.expression()
	if rangeExpr == nil {
		p.error("Expected range expression")
		return
	}

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Expected ')' after range expression")
		return
	}

	body := p.block()
	if body == nil {
		p.error("Expected loop body")
		return
	}

	// Return the for loop AST node
	_ = map[string]interface{}{
		"type":       "for_loop",
		"variable":   varName.Lexeme,
		"initializer": initializer,
		"range_expr": rangeExpr,
		"body":       body,
	}
}

// returnStmt -> "return" expression? ";"
func (p *Parser) returnStatement() {
	if !p.match(tokens.TOKEN_RETURN) {
		p.error("Expected 'return' keyword")
		return
	}

	var value interface{}
	if !p.check(tokens.TOKEN_SEMICOLON) {
		value = p.expression()
		if value == nil {
			p.error("expected expression after 'return'")
			return
		}
	}

	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Expected ';' after return value")
		return
	}

	// Return the return statement
	_ = map[string]interface{}{
		"type":  "return_statement",
		"value": value,
	}
}

// printStmt -> "print" "(" expression ")" ";"
func (p *Parser) printStatement() {
	if !p.match(tokens.TOKEN_PRINT) {
		p.error("Expected 'print' keyword")
		return
	}

	if !p.match(tokens.TOKEN_LPAREN) {
		p.error("Expected '(' after 'print'")
		return
	}

	expr := p.expression()
	if expr == nil {
		p.error("expected expression in 'print' statement")
		return
	}

	if !p.match(tokens.TOKEN_RPAREN) {
		p.error("Expected ')' after expression in 'print'")
		return
	}

	if !p.match(tokens.TOKEN_SEMICOLON) {
		p.error("Expected ';' after 'print' statement")
		return
	}

	// Return the print statement
	_ = map[string]interface{}{
		"type":       "print_statement",
		"expression": expr,
	}
}

// block -> "{" declaration* "}"
func (p *Parser) block() interface{} {
	if !p.match(tokens.TOKEN_LEFT_BRACE) {
		p.error("Expected '{' before block")
		return nil
	}

	var statements []interface{}
	for !p.check(tokens.TOKEN_RIGHT_BRACE) && !p.isAtEnd() {
		stmt := p.declaration()
		if stmt != nil {
			statements = append(statements, stmt)
		}
	}

	if !p.match(tokens.TOKEN_RIGHT_BRACE) {
		p.error("Expected '}' after block")
		return nil
	}

	return map[string]interface{}{
		"type":       "block",
		"statements": statements,
	}
}

// expressionStatement handles expression statements
func (p *Parser) expressionStatement() interface{} {
	expr := p.expression()
	if expr == nil {
		return nil
	}
	
	if p.check(tokens.TOKEN_SEMICOLON) {
		p.advance()
	}
	
	return map[string]interface{}{
		"type":       "expression_statement",
		"expression": expr,
	}
}

// expression handles expressions
func (p *Parser) expression() interface{} {
	expr := p.assignment()
	if expr == nil {
		return nil
	}
	return expr
}

// assignment handles assignment expressions
func (p *Parser) assignment() interface{} {
	expr := p.or()
	if expr == nil {
		return nil
	}

	if p.match(tokens.TOKEN_ASSIGN) {
		equals := p.previous()
		value := p.assignment()

		if value == nil {
			p.error("Expected expression after '='")
			return nil
		}

		if ident, ok := expr.(map[string]interface{}); ok && ident["type"] == "variable" {
			return map[string]interface{}{
				"type":   "assignment",
				"name":   ident["name"],
				"value":  value,
				"line":   equals.Line,
				"column": equals.Column,
			}
		}

		p.error("Invalid assignment target")
	}

	return expr
}

// logic_or -> logic_and ("or" logic_and)*
func (p *Parser) logicOr() interface{} {
	left := p.logicAnd()

	for p.match(tokens.TOKEN_OR) {
		operator := p.previous()
		right := p.logicAnd()
		if right == nil {
			p.error("expected expression after 'or'")
			return nil
		}
		left = map[string]interface{}{
			"type":     "binary",
			"left":     left,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return left
}

// logic_and -> equality ("and" equality)*
func (p *Parser) logicAnd() interface{} {
	left := p.equality()

	for p.match(tokens.TOKEN_AND) {
		operator := p.previous()
		right := p.equality()
		if right == nil {
			p.error("expected expression after 'and'")
			return nil
		}
		left = map[string]interface{}{
			"type":     "logical",
			"left":     left,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return left
}

// equality handles equality and inequality expressions
func (p *Parser) equality() interface{} {
	expr := p.comparison()
	if expr == nil {
		return nil
	}

	for p.match(tokens.TOKEN_EQUAL, tokens.TOKEN_NOT_EQUAL) {
		operator := p.previous()
		right := p.comparison()
		if right == nil {
			p.error("expected expression after operator")
			return nil
		}
		expr = map[string]interface{}{
			"type":     "binary",
			"left":     expr,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return expr
}

// comparison handles comparison expressions
func (p *Parser) comparison() interface{} {
	expr := p.term()
	if expr == nil {
		return nil
	}

	for p.match(tokens.TOKEN_GREATER, tokens.TOKEN_GREATER_EQUAL, tokens.TOKEN_LESS, tokens.TOKEN_LESS_EQUAL) {
		operator := p.previous()
		right := p.term()
		if right == nil {
			p.error("expected expression after operator")
			return nil
		}
		expr = map[string]interface{}{
			"type":     "binary",
			"left":     expr,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return expr
}

// term handles addition and subtraction
func (p *Parser) term() interface{} {
	expr := p.factor()
	if expr == nil {
		return nil
	}

	for p.match(tokens.TOKEN_PLUS, tokens.TOKEN_MINUS) {
		operator := p.previous()
		right := p.factor()
		if right == nil {
			p.error("expected expression after operator")
			return nil
		}
		expr = map[string]interface{}{
			"type":     "binary",
			"left":     expr,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return expr
}

// factor handles multiplication, division, and modulo
func (p *Parser) factor() interface{} {
	expr := p.unary()
	if expr == nil {
		return nil
	}

	for p.match(tokens.TOKEN_MULTIPLY, tokens.TOKEN_DIVIDE, tokens.TOKEN_MODULO) {
		operator := p.previous()
		right := p.unary()
		if right == nil {
			p.error("expected expression after operator")
			return nil
		}
		expr = map[string]interface{}{
			"type":     "binary",
			"left":     expr,
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return expr
}

// unary handles unary operators
func (p *Parser) unary() interface{} {
	if p.match(tokens.TOKEN_MINUS, tokens.TOKEN_NOT) {
		operator := p.previous()
		right := p.unary()
		if right == nil {
			p.error("expected expression after operator")
			return nil
		}
		return map[string]interface{}{
			"type":     "unary",
			"operator": operator.Lexeme,
			"right":    right,
		}
	}

	return p.call()
}

// call handles function calls
func (p *Parser) call() interface{} {
	expr := p.primary()
	if expr == nil {
		return nil
	}

	for {
		if p.match(tokens.TOKEN_LPAREN) {
			// Handle function call
			var args []interface{}
			if !p.check(tokens.TOKEN_RPAREN) {
				args = p.arguments()
				if args == nil {
					return nil
				}
			}

			if !p.match(tokens.TOKEN_RPAREN) {
				p.error("expected ')' after arguments")
				return nil
			}

			expr = map[string]interface{}{
				"type":      "call",
				"callee":    expr,
				"arguments": args,
			}
		} else {
			break
		}
	}

	return expr
}

// arguments handles function call arguments
func (p *Parser) arguments() []interface{} {
	var args []interface{}

	for {
		if len(args) >= 255 {
			p.error("can't have more than 255 arguments")
			return nil
		}

		expr := p.expression()
		if expr == nil {
			return nil
		}
		args = append(args, expr)

		if !p.match(tokens.TOKEN_COMMA) {
			break
		}
	}

	return args
}

// primary handles primary expressions
func (p *Parser) primary() interface{} {
	if p.match(tokens.TOKEN_NUMBER, tokens.TOKEN_STRING) {
		token := p.previous()
		return map[string]interface{}{
			"type":  "literal",
			"value": token.Literal,
		}
	}

	if p.match(tokens.TOKEN_TRUE) {
		return map[string]interface{}{
			"type":  "literal",
			"value": true,
		}
	}

	if p.match(tokens.TOKEN_FALSE) {
		return map[string]interface{}{
			"type":  "literal",
			"value": false,
		}
	}

	if p.match(tokens.TOKEN_NIL) {
		return map[string]interface{}{
			"type":  "literal",
			"value": nil,
		}
	}

	if p.match(tokens.TOKEN_IDENTIFIER) {
		token := p.previous()
		name := token.Lexeme
		
		// Check if it's a type conversion
		if p.check(tokens.TOKEN_LPAREN) {
			switch name {
			case "int", "float", "str", "bool":
				p.advance() // Consume the LPAREN
				expr := p.expression()
				if expr == nil {
					p.error("expected expression in type conversion")
					return nil
				}

				if !p.match(tokens.TOKEN_RPAREN) {
					p.error("expected ')' after type conversion")
					return nil
				}

				return map[string]interface{}{
					"type":    "type_conversion",
					"to_type": name,
					"expr":    expr,
				}
			}
		}

		return map[string]interface{}{
			"type": "variable",
			"name": name,
		}
	}

	if p.match(tokens.TOKEN_LPAREN) {
		expr := p.expression()
		if expr == nil {
			return nil
		}

		if !p.match(tokens.TOKEN_RPAREN) {
			p.error("expected ')' after expression")
			return nil
		}

		return map[string]interface{}{
			"type": "grouping",
			"expr": expr,
		}
	}

	p.error("expected expression")
	return nil
}

// check checks if the current token matches the given token type
func (p *Parser) check(tokenType tokens.TokenType) bool {
	if p.isAtEnd() {
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
		return TokenInfoSintax{Type: tokens.TOKEN_EOF.String(), Lexeme: "", Literal: nil, Line: 0, Column: 0}
	}
	return p.tokens[p.current]
}

// previous returns the most recently consumed token
func (p *Parser) previous() TokenInfoSintax {
	if p.current == 0 {
		return TokenInfoSintax{Type: tokens.TOKEN_EOF.String(), Lexeme: "", Literal: nil, Line: 0, Column: 0}
	}
	return p.tokens[p.current-1]
}

// match checks if the current token matches any of the given token types
func (p *Parser) match(types ...tokens.TokenType) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
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

// synchronize discards tokens until we find a statement boundary
func (p *Parser) synchronize() {
	p.advance()

	for !p.isAtEnd() {
		// If we just had a semicolon, we're at a statement boundary
		if p.previous().Type == "SEMICOLON" {
			return
		}

		// Check for statement-starting keywords
		switch p.peek().Type {
		case "FUNC", "VAR", "CONST",
			"FOR", "IF", "WHILE",
			"PRINT", "RETURN":
			return
		}

		p.advance()
	}
}

// error adds an error message at the current token with context
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

	// Get the line of code where the error occurred
	var lineContent string
	var indicator string
	
	// Try to read the source file to get the line content
	sourceFile := p.getCurrentSourceFile()
	if sourceFile != "" {
		if content, err := os.ReadFile(sourceFile); err == nil {
			lines := strings.Split(string(content), "\n")
			if line > 0 && line <= len(lines) {
				lineContent = strings.TrimSpace(lines[line-1])
				indicator = strings.Repeat(" ", column-1) + "^"
			}
		}
	}

	errMsg := fmt.Sprintf("\nError en línea %d, columna %d:\n", line, column)
	
	if lineContent != "" {
		errMsg += fmt.Sprintf("  %s\n  %s\n", lineContent, indicator)
	}
	
	errMsg += fmt.Sprintf("  Error: %s\n", message)

	// Add context about what was expected if this is a syntax error
	switch message {
	case "Se esperaba ';' después de la expresión":
		errMsg += "  Nota: Asegúrate de terminar las expresiones con punto y coma (;)"
	case "Se esperaba ')' después de la expresión":
		errMsg += "  Nota: Revisa que todos los paréntesis estén balanceados"
	case "Se esperaba '}' al final del bloque":
		errMsg += "  Nota: Asegúrate de cerrar todos los bloques con '}'"
	}

	p.errors = append(p.errors, errMsg)
}