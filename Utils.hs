module Utils
  ( Grammar(..),
    Producciones(..),
    Lado(..),
    Terminal,
    NoTerminal,
    
    union,
    interseccion,
    resta,
    concatenacion,
    
    complemento,
    reversa,
    
    pertenece,
    esEquivalente,

    derIzq,
    izqDer 
  )
where

-- Importo una libreria para trabajar con conjuntos y Diccionarios
import qualified Data.Set as Set
import qualified Data.Map as Map


-- Los T y NT van a ser representados por un conjunto de strings en minuscula y mayuscula respectivamente
-- S es un string en mayuscula


type Terminal   = Char 
type NoTerminal = Char

-- Las producciones:
data Producciones  = T Terminal | R  Terminal NoTerminal | L  NoTerminal Terminal | Lambda   deriving (Eq, Ord, Show)
data Lado = Derecho | Izquierdo

data Grammar2 = G 
    {
      noTerminales  :: Set.Set NoTerminal,
      terminales    :: Set.Set Terminal,
      inicial       :: NoTerminal,
      reglas        :: Map.Map NoTerminal (Set.Set Producciones),
      lado          :: Lado
    }


type State  = Int
data Symbol = T Char | Lambda   deriving (Eq, Ord)


data Automata = A 
    {
      estados      :: Set.Set State,
      alfabeto     :: Set.Set Char,
      transiciones :: Set.Set (State, Symbol, State),
      eInicial     :: State,
      eAceptacion  :: Set.Set State

    }
Puedo llevar el estado actual en una monada !?
-- Precedencias:
  -- 
  -- 

-- opBinarias:
union :: Automata -> Automata -> Automata
union automata1 automata2 = let  (A e1 a1 t1 i1 f1, A e2 a2 t2 i2 f2) = renombrar automata1 automata2
                   e'  = Set.union e1 e2
                   ini = nuevoEstado (Set.union e1 e2)
                   e   = Set.insert ini e'
                   a   = Set.union a1 a2 
                   t   = Set.insert (ini, Lambda, i1) (Set.insert (ini, Lambda, i2) (Set.union t1 t2))  -- Las transciciones no iniciales entran y las iniciales se modifican 
                   f   = Set.union f1 f2 
              in (A e a ini t f)

nuevoEstado :: Set.Set State -> State
nuevoEstado e
  | Set.null e = 0 -- Si es un cjto nulo, ie. no hay estados el primer estado es 0
  | otherwise   = Set.findMax e + 1 -- Si ya hay estados le asigno el siguiente


renombrar :: Automata -> Automata -> (Automata, Automata)
renombrar a1 a2 = (a1', a2')
  where
    a1' = a1
    offset = nuevoEstado (estados a1)
    a2' = renombrarAutomata offset a2


renombrarAutomata :: Int -> Automata -> Automata
renombrarAutomata offset (A e a t i f) =
  A
    { estados      = Set.map (+ offset) e
    , alfabeto     = a
    , transiciones = Set.map renombrarTrans t
    , eInicial     = i + offset
    , eAceptacion  = Set.map (+ offset) f
    }
  where
    renombrarTrans (q, s, p) = (q + offset, s, p + offset)


--    let nt  = union nt1 nt2 
--        t  = union t1 t2 -- Y agrego el nuevo terminal '#'aca tambien
--        st = -- Aca un terminal nuevo '#'
--        p  = union p1 p2 -- Y agrego tambien una transicion de '#' a st1 y de '#' a st2
--        s  = RightG



-- Puedo hacerla con producto cartesiano o con deMorgan L1 ​∩ L2 ​= c( c(L1) ​​∪ c(L2​​) )
interseccion :: Automata -> Automata -> Automata
interseccion = undefined


-- Ver 
resta :: Automata -> Automata -> Automata
resta a1 a2 = interseccion (a1 (complemento a2))  -- : L1 - L2 = L1 interseccion (complemento L2)


-- Brookshear p61.
concatenacion :: Automata -> Automata -> Automata
concatenacion  automata1 automata2 =
  let (A e1 a1 t1 i1 Ac1, A e2 a2 t2 i2 Ac2) = renombrar automata1 automata2
      e  = Set.union e1 e2 
      a  = Set.union a1 a2
      t  = Set.union (concat' Ac1 i2) (Set.union t1 t2)  
      i  = i1
      Ac = Ac2
  in (A e a t i Ac)

concat' :: Set.Set State -> State -> Set.Set (State, Symbol, State)
concat' Ac1 i2 = Set.map (\s-> (s, Lambda, i2)) Ac1


  
  
  
   (G (Set.Union nt1 nt2)
                                                               (Set.Union t1   t2)
                                                               (ini1)
                                                               (r1 union r2) -- Armar 
                                                               (l1)
                                                          )


-- opUnarias:

-- VER: El complemento se implementa sobre autómatas deterministas completos, invirtiendo el conjunto de estados de aceptación.
-- Precondicion: el autómata es un AFD completo
complemento :: Automata -> Automata
complemento (A e a t i ac) = (A e a t i (Set.difference e ac)) 


-- Reversa
reversa :: Automata -> Automata
reversa (A e a t i ac) = let 
  ini'  = nuevoEstado e
  e'    = Set.insert ini' e 
  a'    = a 
  t'    = Set.union (inv' t) (iniAac ini' ac) 
  ac'   = Set.singleton i
  in (A e' a' t' ini' ac')

iniAac :: State -> Set.Set State -> Set.Set (State ,Symbol, State) 
iniAac ini ac = Set.map (\e-> (ini, Lambda, e))  ac

inv' :: Set.Set (State ,Symbol, State) -> Set.Set (State, Symbol, State)
inv' t = Set.map (\(e1,s1,e2) -> (e2, s1, e1))





-- Pertenece:
pertenece :: Grammar -> String -> Bool
pertenece (G nt t ini r l) c      =  let Just v = Map.lookup ini r
                                     in Set.member c v -- Si solo busco un caracter
pertenece g (x:xs) =      



-- Equivalencia:
esEquivalente :: Automata -> Automata -> Bool
esEquivalente = esNull (L1 - L2) && esNull (L2 - L1)
-- Si L1 - L2 = null y L2 - L1 = null

-- Para ver si el lenguaje es vacio tengo que hacer dfs sobre el grafo que genera y ver que no llego a ningun estado final
esNull :: Automata -> Bool
esNull a = undefined

-- Conversion de lado:
derIzq :: Grammar -> Grammar
derIzq = undefined


izqDer :: Grammar -> Grammar
izqDer = undefined



aefAGramm :: Automata -> Gramatica

grammAAEF :: Gramatica -> Automata  






















-- ============================================================================
-- 1. DEFINICIÓN DE TIPOS
-- ============================================================================

-- Tipos básicos
type Terminal = Char
type NonTerminal = String
type Symbol = String

-- Tipo de gramática: Derecha o Izquierda
data GrammarType = RightGrammar | LeftGrammar
  deriving (Show, Eq)

-- Producción: A -> aB | a | λ
data Production = Production 
  { leftSide :: NonTerminal
  , terminal :: Maybe Terminal  -- Nothing representa λ
  , rightSide :: Maybe NonTerminal
  } deriving (Show, Eq)

-- Gramática Regular
data Grammar = Grammar
  { grammarType :: GrammarType
  , startSymbol :: NonTerminal
  , productions :: [Production]
  } deriving (Show, Eq)

-- Autómata Finito (para procesamiento interno)
data Automaton = Automaton
  { states :: Set.Set String
  , alphabet :: Set.Set Terminal
  , transitions :: Map.Map (String, Terminal) (Set.Set String)
  , startState :: String
  , acceptStates :: Set.Set String
  } deriving (Show, Eq)

-- Términos para operaciones
data Term 
  = GrammarTerm String
  | Union Term Term
  | Intersection Term Term
  | Concatenation Term Term
  | Complement Term
  | Reverse Term
  | ConvertSide Term
  deriving (Show, Eq)

-- ============================================================================
-- 4. CONSULTA DE PERTENENCIA
-- ============================================================================

accepts :: Grammar -> String -> Bool
accepts g str = acceptsAutomaton (grammarToAutomaton g) str

acceptsAutomaton :: Automaton -> String -> Bool
acceptsAutomaton aut str = 
  let finalStates = processString (Set.singleton (startState aut)) str
  in not $ Set.null $ Set.intersection finalStates (acceptStates aut)
  where
    processString currentStates [] = currentStates
    processString currentStates (c:cs) =
      let nextStates = Set.unions 
            [ Map.findWithDefault Set.empty (s, c) (transitions aut)
            | s <- Set.toList currentStates ]
      in if Set.null nextStates 
         then Set.empty
         else processString nextStates cs

-- ============================================================================
-- 5. OPERACIONES SOBRE GRAMÁTICAS
-- ============================================================================

-- Unión de dos gramáticas
unionGrammar :: Grammar -> Grammar -> Grammar
unionGrammar g1 g2 = Grammar
  { grammarType = grammarType g1
  , startSymbol = "S_union"
  , productions = newProds
  }
  where
    newProds = 
      [ Production "S_union" Nothing (Just (startSymbol g1))
      , Production "S_union" Nothing (Just (startSymbol g2))
      ] ++ renameProductions "G1_" (productions g1)
        ++ renameProductions "G2_" (productions g2)

-- Concatenación de gramáticas
concatenationGrammar :: Grammar -> Grammar -> Grammar
concatenationGrammar g1 g2 = Grammar
  { grammarType = grammarType g1
  , startSymbol = startSymbol g1
  , productions = newProds
  }
  where
    acceptingStates = [ leftSide p | p <- productions g1, isAccepting p ]
    isAccepting (Production _ Nothing _) = True
    isAccepting (Production _ _ Nothing) = True
    isAccepting _ = False
    
    connectingProds = 
      [ Production s Nothing (Just (startSymbol g2))
      | s <- acceptingStates ]
    
    newProds = productions g1 
            ++ connectingProds
            ++ renameProductions "G2_" (productions g2)

-- Complemento de una gramática
complementGrammar :: Grammar -> Grammar
complementGrammar g = Grammar
  { grammarType = grammarType g
  , startSymbol = startSymbol g
  , productions = newProds
  }
  where
    aut = grammarToAutomaton g
    allStates = Set.toList (states aut)
    newAcceptStates = Set.difference (states aut) (acceptStates aut)
    
    newProds = 
      [ Production s t mr 
      | (s, t) <- Map.keys (transitions aut)
      , let targets = Map.findWithDefault Set.empty (s, t) (transitions aut)
      , mr <- map Just (Set.toList targets)
      ] ++ [ Production s Nothing Nothing | s <- Set.toList newAcceptStates ]

-- Reversa de una gramática
reverseGrammar :: Grammar -> Grammar
reverseGrammar g = Grammar
  { grammarType = flipType (grammarType g)
  , startSymbol = "S_rev"
  , productions = reversedProds
  }
  where
    flipType RightGrammar = LeftGrammar
    flipType LeftGrammar = RightGrammar
    
    reversedProds = case grammarType g of
      RightGrammar -> 
        [ Production (fromMaybe "F" mr) t (Just l)
        | Production l t mr <- productions g
        , t /= Nothing
        ] ++ [Production "S_rev" Nothing (Just (startSymbol g))]
      
      LeftGrammar ->
        [ Production l t (fromMaybe (Just "F") (fmap Just mr))
        | Production l t mr <- productions g
        , t /= Nothing
        ] ++ [Production "S_rev" Nothing (Just (startSymbol g))]

-- Conversión de lado (derecha ↔ izquierda)
convertSide :: Grammar -> Grammar
convertSide g = g { grammarType = flipType (grammarType g) }
  where
    flipType RightGrammar = LeftGrammar
    flipType LeftGrammar = RightGrammar

-- Función auxiliar para renombrar producciones
renameProductions :: String -> [Production] -> [Production]
renameProductions prefix prods = 
  map renameProd prods
  where
    renameProd (Production l t mr) = 
      Production (prefix ++ l) t (fmap (prefix ++) mr)


-- ============================================================================
-- 6. CONSULTA DE EQUIVALENCIA
-- ============================================================================

equivalent :: Grammar -> Grammar -> Bool
equivalent g1 g2 = 
  let testStrings = generateTestStrings 8 (extractAlphabet g1 g2)
  in all (\s -> accepts g1 s == accepts g2 s) testStrings

extractAlphabet :: Grammar -> Grammar -> [Terminal]
extractAlphabet g1 g2 = nub $ 
  [ t | Production _ (Just t) _ <- productions g1 ++ productions g2 ]

generateTestStrings :: Int -> [Terminal] -> [String]
generateTestStrings maxLen alpha = 
  "" : [ s | len <- [1..maxLen], s <- replicateM len alpha ]
  where
    replicateM 0 _ = [[]]
    replicateM n xs = [ x:rest | x <- xs, rest <- replicateM (n-1) xs ]

-- ============================================================================
-- 7. PRETTY PRINTER
-- ============================================================================

prettyPrintGrammar :: Grammar -> String
prettyPrintGrammar g = unlines $
  [ show (grammarType g)
  , "Símbolo inicial: " ++ startSymbol g
  , "Producciones:"
  ] ++ map prettyPrintProduction (productions g)

prettyPrintProduction :: Production -> String
prettyPrintProduction (Production l t mr) =
  "  " ++ l ++ " -> " ++ terminalStr ++ nonTerminalStr
  where
    terminalStr = case t of
      Nothing -> "λ"
      Just c -> ['"', c, '"']
    
    nonTerminalStr = case mr of
      Nothing -> ""
      Just nt -> " " ++ nt

prettyPrintTerm :: Term -> String
prettyPrintTerm (GrammarTerm name) = name
prettyPrintTerm (Union t1 t2) = 
  "(" ++ prettyPrintTerm t1 ++ " ∪ " ++ prettyPrintTerm t2 ++ ")"
prettyPrintTerm (Intersection t1 t2) =
  "(" ++ prettyPrintTerm t1 ++ " ∩ " ++ prettyPrintTerm t2 ++ ")"
prettyPrintTerm (Concatenation t1 t2) =
  "(" ++ prettyPrintTerm t1 ++ " · " ++ prettyPrintTerm t2 ++ ")"
prettyPrintTerm (Complement t) =
  "¬(" ++ prettyPrintTerm t ++ ")"
prettyPrintTerm (Reverse t) =
  "R(" ++ prettyPrintTerm t ++ ")"
prettyPrintTerm (ConvertSide t) =
  "Convert(" ++ prettyPrintTerm t ++ ")"

-- ============================================================================
-- 8. EVALUADOR DE TÉRMINOS
-- ============================================================================

type GrammarEnvironment = Map.Map String Grammar

evalTerm :: GrammarEnvironment -> Term -> Maybe Grammar
evalTerm env (GrammarTerm name) = Map.lookup name env
evalTerm env (Union t1 t2) = do
  g1 <- evalTerm env t1
  g2 <- evalTerm env t2
  return $ unionGrammar g1 g2
evalTerm env (Concatenation t1 t2) = do
  g1 <- evalTerm env t1
  g2 <- evalTerm env t2
  return $ concatenationGrammar g1 g2
evalTerm env (Complement t) = do
  g <- evalTerm env t
  return $ complementGrammar g
evalTerm env (Reverse t) = do
  g <- evalTerm env t
  return $ reverseGrammar g
evalTerm env (ConvertSide t) = do
  g <- evalTerm env t
  return $ convertSide g
evalTerm env (Intersection t1 t2) = do
  g1 <- evalTerm env t1
  g2 <- evalTerm env t2
  let comp_g2 = complementGrammar g2
  let union_result = unionGrammar g1 comp_g2
  return $ complementGrammar union_result

-- ============================================================================
-- 9. EJEMPLOS DE USO
-- ============================================================================

-- Ejemplo de gramática derecha: L = {a^n b | n ≥ 0}
exampleGrammar1 :: Grammar
exampleGrammar1 = Grammar
  { grammarType = RightGrammar
  , startSymbol = "S"
  , productions = 
      [ Production "S" (Just 'a') (Just "S")
      , Production "S" (Just 'b') Nothing
      ]
  }

-- Ejemplo de gramática izquierda: L = {a b^n | n ≥ 0}
exampleGrammar2 :: Grammar
exampleGrammar2 = Grammar
  { grammarType = LeftGrammar
  , startSymbol = "S"
  , productions =
      [ Production "S" (Just 'b') (Just "S")
      , Production "S" (Just 'a') Nothing
      ]
  }

-- Función principal de demostración
main :: IO ()
main = do
  putStrLn "=== Analizador de Gramáticas Regulares ==="
  putStrLn ""
  
  -- Mostrar gramáticas de ejemplo
  putStrLn "Gramática 1:"
  putStrLn $ prettyPrintGrammar exampleGrammar1
  putStrLn ""
  
  putStrLn "Gramática 2:"
  putStrLn $ prettyPrintGrammar exampleGrammar2
  putStrLn ""
  
  -- Pruebas de pertenencia
  let testStr1 = "aaab"
  putStrLn $ "¿'" ++ testStr1 ++ "' pertenece a G1? " ++ show (accepts exampleGrammar1 testStr1)
  
  let testStr2 = "abbb"
  putStrLn $ "¿'" ++ testStr2 ++ "' pertenece a G2? " ++ show (accepts exampleGrammar2 testStr2)
  putStrLn ""
  
  -- Operaciones
  let g_union = unionGrammar exampleGrammar1 exampleGrammar2
  putStrLn "Unión de G1 y G2:"
  putStrLn $ prettyPrintGrammar g_union
  putStrLn ""
  
  -- Equivalencia
  putStrLn $ "¿G1 equivalente a G2? " ++ show (equivalent exampleGrammar1 exampleGrammar2)