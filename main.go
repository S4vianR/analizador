package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"analizador/pkg/lexer"
	"analizador/pkg/parser"

	"github.com/schollz/progressbar/v3"
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

	// Crear y configurar la barra de progreso
	bar := progressbar.NewOptions(100,
		progressbar.OptionSetDescription("Analizando código..."),
		progressbar.OptionSetTheme(progressbar.Theme{
			Saucer:        "=",
			SaucerHead:    ">",
			SaucerPadding: " ",
			BarStart:      "[",
			BarEnd:        "]",
		}),
		progressbar.OptionShowCount(),
		progressbar.OptionSetWidth(50),
	)

	// Simular progreso mientras se carga el lexer
	for i := 0; i < 100; i++ {
		bar.Add(1)
		time.Sleep(10 * time.Millisecond)
	}
	fmt.Println()

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

	// Crear barra de progreso para el análisis sintáctico
	syntaxBar := progressbar.NewOptions(100,
		progressbar.OptionSetDescription("Analizando sintaxis..."),
		progressbar.OptionSetTheme(progressbar.Theme{
			Saucer:        "=",
			SaucerHead:    ">",
			SaucerPadding: " ",
			BarStart:      "[",
			BarEnd:        "]",
		}),
		progressbar.OptionShowCount(),
		progressbar.OptionSetWidth(50),
		progressbar.OptionClearOnFinish(),
	)

	// Canal para controlar la finalización de la barra de progreso
	done := make(chan bool)
	// Canal para forzar la finalización
	forceFinish := make(chan bool, 1)

	// Iniciar una goroutine para actualizar la barra de progreso
	go func() {
		for i := 0; i < 100; i++ {
			select {
			case <-forceFinish:
				done <- true
				return
			default:
				syntaxBar.Add(1)
				time.Sleep(15 * time.Millisecond)
			}
		}
		done <- true
	}()

	// Crear el parser usando el archivo de tokens y el archivo fuente
	p, err := parser.NewParserFromFile(tokensFile, sourceFile)
	if err != nil {
		log.Fatalf("Error al crear el analizador sintáctico: %v", err)
	}

	// Canal para recibir el AST y el error del análisis
	astChan := make(chan struct {
		ast interface{}
		err error
	}, 1)

	// Ejecutar el análisis sintáctico en una goroutine
	go func() {
		ast, err := p.Parse()
		astChan <- struct {
			ast interface{}
			err error
		}{ast, err}
	}()

	// Esperar a que termine el análisis o la barra de progreso
	select {
	case result := <-astChan:
		// Si el análisis termina primero, forzar la finalización de la barra
		forceFinish <- true
		<-done                // Esperar a que la goroutine de la barra termine
		fmt.Print("\r\033[K") // Limpiar la línea actual

		if result.err != nil {
			// Mostrar el error en rojo
			fmt.Printf("\n❌ Error de sintaxis en la línea %s\n", result.err)
			os.Exit(1)
		}

		// Mostrar mensaje de éxito claro
		fmt.Printf("\n✅ El código es válido.\n")

	case <-done:
		// Si la barra de progreso termina primero, esperar el resultado del análisis
		result := <-astChan
		fmt.Print("\r\033[K") // Limpiar la línea actual

		if result.err != nil {
			// Mostrar el error en rojo
			fmt.Printf("\n❌ Error de sintaxis en la línea %s\n", result.err)
			os.Exit(1)
		}

		// Mostrar mensaje de éxito claro
		fmt.Printf("\n✅ El código es válido.\n")
	}
}
