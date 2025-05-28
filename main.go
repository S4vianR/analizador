package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"analizador/pkg/lexer"
	"analizador/pkg/parser"
)

func main() {
	// Verificar que se proporcione un archivo .siv
	if len(os.Args) < 2 {
		log.Fatal("Uso: go run main.go <archivo.siv>")
	}

	// Obtener la ruta del archivo de entrada
	sourceFile := os.Args[1]
	if filepath.Ext(sourceFile) != ".siv" {
		log.Fatal("El archivo debe tener extensión .siv")
	}

	// 1. Ejecutar el analizador léxico
	fmt.Println("🔍 Ejecutando análisis léxico...")
	l := lexer.NewLexerFromFile(sourceFile)
	if l == nil {
		log.Fatal("No se pudo crear el analizador léxico")
	}

	// Escanear los tokens
	_, err := l.ScanTokens()
	if err != nil {
		log.Fatalf("Error en el análisis léxico: %v", err)
	}

	// Guardar los tokens en un archivo JSON
	tokensFile := strings.TrimSuffix(sourceFile, ".siv") + ".tokens.json"
	if err := l.SaveTokensToFile(tokensFile); err != nil {
		log.Fatalf("Error al guardar tokens: %v", err)
	}
	fmt.Printf("✅ Tokens guardados en: %s\n", tokensFile)

	// Cargar y analizar los tokens usando el parser
	fmt.Println("\n🔍 Ejecutando análisis sintáctico...")
	p, err := parser.NewParserFromFile(tokensFile)
	if err != nil {
		log.Fatalf("Error al crear el analizador sintáctico: %v", err)
	}

	// Analizar la sintaxis
	err = p.Parse()
	if err != nil {
		log.Fatalf("❌ Error en el análisis sintáctico: %v", err)
	}

	fmt.Println("✅ Análisis completado exitosamente!")
}
