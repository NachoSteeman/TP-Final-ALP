
-- ============================================================================
-- Parser.hs - Parser de Gramáticas Regulares
-- ============================================================================

module Parser
  ( parseGrammar
  , parseGrammarFromFile
  , parseTermExpression
  ) where

import Utils
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (void)
import Data.Char (isUpper, isLower)

-- ============================================================================
-- PARSER PRINCIPAL
-- ============================================================================

-- Parser de una gramática completa
parseGrammar :: Parser Grammar
parseGrammar = do
    spaces
    l <- parseLado
    spaces
    rs <- many1 parseRegla
    spaces
    eof
    
    let nt = Set.fromList $ map fst rs
        t  = extraerTerminales rs
        ini = fst $ head rs
        reglasMap = Map.fromListWith Set.union rs
    
    return $ G nt t ini reglasMap l

-- Parser del lado (Derecha o Izquierda)
parseLado :: Parser Lado
parseLado = 
    (string "Derecha" >> return Derecho)
    <|> (string "Izquierda" >> return Izquierdo)

-- Parser de una regla completa: NT -> producciones;
parseRegla :: Parser (NoTerminal, Set.Set Producciones)
parseRegla = do
    spaces
    nt <- parseNoTerminal
    spaces
    string "->"
    spaces
    prods <- parseProducciones
    spaces
    char ';'
    spaces
    return (nt, Set.fromList prods)

-- Parser de producciones separadas por |
parseProducciones :: Parser [Producciones]
parseProducciones = sepBy1 parseProduccion (spaces >> char '|' >> spaces)

-- Parser de una producción individual
parseProduccion :: Parser Producciones
parseProduccion = spaces >> choice
    [ try parseLambda
    , try parseTerminalNonTerminal
    , try parseNonTerminalTerminal  
    , try parseTerminalSolo
    ]

-- Parser de lambda (cadena vacía)
parseLambda :: Parser Producciones
parseLambda = do
    char '\\'
    return Lambda

-- Parser de terminal entre comillas
parseTerminal :: Parser Terminal
parseTerminal = do
    char '"'
    t <- satisfy (not . (== '"'))
    char '"'
    return t

-- Parser de no terminal (letra mayúscula o &)
parseNoTerminal :: Parser NoTerminal
parseNoTerminal = 
    (char '&' >> return 'S')
    <|> satisfy isUpper

-- Parser de T NT (para gramáticas derechas)
parseTerminalNonTerminal :: Parser Producciones
parseTerminalNonTerminal = do
    t <- parseTerminal
    spaces
    nt <- parseNoTerminal
    return $ R t nt

-- Parser de NT T (para gramáticas izquierdas)
parseNonTerminalTerminal :: Parser Producciones
parseNonTerminalTerminal = do
    nt <- parseNoTerminal
    spaces
    t <- parseTerminal
    return $ L nt t

-- Parser de solo terminal
parseTerminalSolo :: Parser Producciones
parseTerminalSolo = do
    t <- parseTerminal
    return $ T t

-- ============================================================================
-- PARSER DE EXPRESIONES (TÉRMINOS)
-- ============================================================================

data Termino
    = GramRef String
    | UnionT Termino Termino
    | InterT Termino Termino
    | RestaT Termino Termino
    | ConcatT Termino Termino
    | ComplT Termino
    | RevT Termino
    | ConvT Termino
    deriving (Show)

-- Parser de expresiones con operaciones
parseTermExpression :: Parser Termino
parseTermExpression = buildExpressionParser operatorTable parseTerm

parseTerm :: Parser Termino
parseTerm = parens parseTermExpression
        <|> parseUnaryOp
        <|> parseGrammarRef

parseGrammarRef :: Parser Termino
parseGrammarRef = do
    spaces
    name <- many1 alphaNum
    spaces
    return $ GramRef name

parseUnaryOp :: Parser Termino
parseUnaryOp = do
    spaces
    op <- choice
        [ string "complemento" >> return ComplT
        , string "reversa" >> return RevT
        , string "convertir" >> return ConvT
        ]
    spaces
    char '('
    spaces
    t <- parseTermExpression
    spaces
    char ')'
    return $ op t

parens :: Parser a -> Parser a
parens p = do
    char '('
    spaces
    result <- p
    spaces
    char ')'
    return result

-- Tabla de operadores con precedencia
operatorTable = 
    [ [binary "union" UnionT AssocLeft]
    , [binary "interseccion" InterT AssocLeft]
    , [binary "resta" RestaT AssocLeft]
    , [binary "concat" ConcatT AssocLeft]
    ]

binary name fun assoc = Infix (do { reservedOp name; return fun }) assoc

reservedOp name = do
    string name
    spaces

-- Parser simplificado usando Parsec
buildExpressionParser table term = foldl addPrecLevel term table
  where
    addPrecLevel term ops = term

-- ============================================================================
-- FUNCIONES AUXILIARES
-- ============================================================================

-- Extrae todos los terminales de las reglas
extraerTerminales :: [(NoTerminal, Set.Set Producciones)] -> Set.Set Terminal
extraerTerminales rs = Set.fromList $ concat 
    [ extraerDeProduccion p 
    | (_, prods) <- rs
    , p <- Set.toList prods
    ]
  where
    extraerDeProduccion (T t)    = [t]
    extraerDeProduccion (R t _)  = [t]
    extraerDeProduccion (L _ t)  = [t]
    extraerDeProduccion Lambda   = []

-- ============================================================================
-- PARSER DESDE ARCHIVO
-- ============================================================================

parseGrammarFromFile :: FilePath -> IO (Either ParseError Grammar)
parseGrammarFromFile filename = do
    input <- readFile filename
    return $ parse parseGrammar filename input

-- ============================================================================
-- PRETTY PRINTER
-- ============================================================================

prettyPrintGrammar :: Grammar -> String
prettyPrintGrammar (G nt t ini rs l) = unlines $
    [ show l
    , "Símbolo inicial: " ++ [ini]
    , "No terminales: " ++ show (Set.toList nt)
    , "Terminales: " ++ show (Set.toList t)
    , "Producciones:"
    ] ++ concatMap mostrarRegla (Map.toList rs)
  where
    mostrarRegla (nt, prods) = 
        [ "  " ++ [nt] ++ " -> " ++ mostrarProds (Set.toList prods) ++ ";" ]
    
    mostrarProds prods = 
        intercalateStr " | " (map mostrarProd prods)
    
    mostrarProd Lambda     = "\\"
    mostrarProd (T t)      = ['"', t, '"']
    mostrarProd (R t nt)   = ['"', t, '"', ' ', nt]
    mostrarProd (L nt t)   = [nt, ' ', '"', t, '"']
    
    intercalateStr _ []     = ""
    intercalateStr _ [x]    = x
    intercalateStr sep (x:xs) = x ++ sep ++ intercalateStr sep xs

-- ============================================================================
-- EJEMPLOS DE USO
-- ============================================================================

-- Ejemplo de archivo .grm válido:
{-
Derecha
& -> "a" A | "b" B | \;
A -> "b" B | "a" &;
B -> "b";
-}

-- Ejemplo de uso en ghci:
{-
ghci> parseGrammarFromFile "ejemplo.grm"
ghci> let Right g = it
ghci> prettyPrintGrammar g
ghci> pertenece g "aaab"
-}