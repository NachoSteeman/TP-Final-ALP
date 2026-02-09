import Utils
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Set as Set
import qualified Data.Map as Map


-- cabal install parsec


--   Derecha                              Izquierda
--   & -> "a" A | "b" B | \;               & -> A "b" | B "a";
--   A -> "b" B | "a" &;                   A -> B "b" | A "a";
--   B -> "b";                             B -> & "a" | \;



-- gramatica    = ("Derecha" | "Izquierda") producciones
-- producciones = NT "->" reglas 
-- reglas       = (t nt| t)  ("|" reglas | ";" producciones) -- Aca producciones puede terminar)
-- 
-- 
-- gramatica    = lado reglas
-- reglas       = NT "->"  producciones
-- producciones = produccion (";" |"|" producciones)
-- produccion   =   NT T | T NT | N | \


parserGramatica :: Grammar 
parserGramatica = do lado <- string
                     r <- many1 reglas -- aca tengo una lista de reglas
                     let nt  = Set.fromList (map fst r)
                         t   = -- ?
                         ini = fst head r 
                     in  return (G nt t ini r lado)

reglas :: Parser (Map.Map NoTerminal (Set.Set Producciones))
reglas    = do nt <- noTerminal
               symbol "->" 
               prod <- many1 producciones -- una lista de [Producciones] 
               return (Map.insert nt prod) -- Mal, arreglar


--
producciones :: Map.Map NoTerminal (Set.Set Producciones) -> NoTerminal -> Map.Map NoTerminal (Set.Set Producciones)
producciones map nt = do p <- produccion 
                             symbol ";"
                             return Map.insert nt p map
                         <|>   symbol '|' 
                               producciones (Map.insert nt p map) nt
--
--
produccion :: Parser Producciones
produccion = do char '\\'
                return Lambda           
            <|> try (L <$> upper <*> lower)           
            <|> try (R <$> lower <*> upper)            
            <|>  (T <$> lower)


-- <$> aplica una función “dentro” de un contexto
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- <*> aplica una función que ya está en un contexto a un valor también en contexto
                     

            





-- ============================================================================
-- 2. PARSER DE GRAMÁTICAS
-- ============================================================================

{-
    -- Parser principal para archivos .grm
    parseGrammarFile :: Parser Grammar
    parseGrammarFile = do
      spaces
      gType <- parseGrammarType
      spaces
      prods <- many1 parseProduction
      spaces
      eof
      let start = findStartSymbol prods
      return $ Grammar gType start prods
    
    -- Parser del tipo de gramática
    parseGrammarType :: Parser GrammarType
    parseGrammarType = do
      choice 
        [ string "Derecha" >> return RightGrammar
        , string "Izquierda" >> return LeftGrammar
        ]
    
    -- Parser de producciones
    parseProduction :: Parser Production
    parseProduction = do
      spaces
      nt <- parseNonTerminal
      spaces
      string "->"
      spaces
      prod <- parseRightHandSide
      spaces
      char ';'
      spaces
      return $ Production nt (fst prod) (snd prod)
    
    -- Parser del lado derecho de una producción
    parseRightHandSide :: Parser (Maybe Terminal, Maybe NonTerminal)
    parseRightHandSide = choice
      [ try parseLambda
      , try parseTerminalOnly
      , try parseTerminalNonTerminal
      ]
    
    parseLambda :: Parser (Maybe Terminal, Maybe NonTerminal)
    parseLambda = do
      char '\\'
      return (Nothing, Nothing)
    
    parseTerminalOnly :: Parser (Maybe Terminal, Maybe NonTerminal)
    parseTerminalOnly = do
      t <- parseTerminalSymbol
      notFollowedBy (spaces >> (letter <|> char '&'))
      return (Just t, Nothing)
    
    parseTerminalNonTerminal :: Parser (Maybe Terminal, Maybe NonTerminal)
    parseTerminalNonTerminal = do
      t <- parseTerminalSymbol
      spaces
      nt <- parseNonTerminal
      return (Just t, Just nt)
    
    parseTerminalSymbol :: Parser Terminal
    parseTerminalSymbol = do
      char '"'
      c <- anyChar
      char '"'
      return c
    
    parseNonTerminal :: Parser NonTerminal
    parseNonTerminal = do
      choice
        [ char '&' >> return "S"  -- & es el símbolo inicial
        , (:) <$> upper <*> many alphaNum
        ]
    
    -- Encuentra el símbolo inicial
    findStartSymbol :: [Production] -> NonTerminal
    findStartSymbol prods = 
      case filter (\p -> leftSide p == "S" || leftSide p == "&") prods of
        (p:_) -> leftSide p
        [] -> leftSide (head prods)

-}