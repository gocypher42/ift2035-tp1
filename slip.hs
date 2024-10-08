-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

-- Libraire d'analyse syntaxique (et lexicale).
import Data.Char -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust
import Text.ParserCombinators.Parsec

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp
  = Snil -- La liste vide
  | Scons Sexp Sexp -- Une paire
  | Ssym String -- Un symbole
  | Snum Int -- Un entier
  -- Génère automatiquement un pretty-printer et une fonction de
  -- comparaison structurelle.
  deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do _ <- char c; return ()

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do
  pChar ';'
  _ <- many (satisfy (\c -> not (c == '\n')))
  pChar '\n'
  return ()

-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do _ <- many (do { _ <- space; return () } <|> pComment); return ()

-- Un nombre entier est composé de chiffres.
integer :: Parser Int
integer =
  do
    c <- digit
    integer' (digitToInt c)
    <|> do
      _ <- satisfy (\c -> (c == '-'))
      n <- integer
      return (-n)
  where
    integer' :: Int -> Parser Int
    integer' n =
      do
        c <- digit
        integer' (10 * n + (digitToInt c))
        <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")

pSymbol :: Parser Sexp
pSymbol = do
  s <- many1 (pSymchar)
  return
    ( case parse integer "" s of
        Right n -> Snum n
        _ -> Ssym s
    )

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do
  pChar '\''
  pSpaces
  e <- pSexp
  return (Scons (Ssym "quote") (Scons e Snil))

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList = do pChar '('; pSpaces; pTail

pTail :: Parser Sexp
pTail =
  do pChar ')'; return Snil
    <|> do
      pChar '.'
      pSpaces
      e <- pSexp
      pSpaces
      pChar ')' <|> error ("Missing ')' after: " ++ show e)
      return e
    <|> do e <- pSexp; pSpaces; es <- pTail; return (Scons e es)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do
  pSpaces
  pList <|> pQuote <|> pSymbol
    <|> do
      x <- pAny
      case x of
        Nothing -> pzero
        Just c -> error ("Unexpected char '" ++ [c] ++ "'")

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do
  pSpaces
  many
    ( do
        e <- pSexpTop
        pSpaces
        return e
    )

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
  readsPrec _p s = case parse pSexp "" s of
    Left _ -> []
    Right e -> [(e, "")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
  let showTail Snil = showChar ')'
      showTail (Scons e1' e2') =
        showChar ' ' . showSexp' e1' . showTail e2'
      showTail e = showString " . " . showSexp' e . showChar ')'
   in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read

showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

type Tag = String

type Pat = Maybe (Tag, [Var])

data BindingType = Lexical | Dynamic
  deriving (Show, Eq)

data Lexp
  = Lnum Int -- Constante entière.
  | Lvar Var -- Référence à une variable.
  | Lfn Var Lexp -- Fonction anonyme prenant un argument.
  | Lpipe Lexp Lexp -- Appel de fonction, avec un argument.
  | Lcons Tag [Lexp] -- Constructeur de liste vide.
  | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
  | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
  deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons left Snil) = s2l left
s2l (Scons (Ssym "lambda") (Scons var function)) =
  case s2l var of
    Lvar v -> Lfn v (s2l function)
    Lpipe _ _ -> mkLambdaFromPipe var function
    e -> error ("Malformed Sexp: " ++ (show e))
s2l (Scons (Ssym "cons") right) = mkLcons right
s2l (Scons (Ssym "case") (Scons left right)) =
  Lcase (s2l left) (mkBranches right)
s2l
  ( Scons
      (Ssym "if")
      ( Scons
          condition
          (Scons ifTrue (Scons ifFalse Snil))
        )
    ) =
    let trueLexp = (Just ("true", []), s2l ifTrue)
        falseLexp = (Just ("false", []), s2l ifFalse)
     in Lcase (s2l condition) [trueLexp, falseLexp]
s2l (Scons (Ssym "slet") (Scons variables body)) =
  let varsContent = getVarsContent variables
      body' = s2l body
   in mkLlet Lexical varsContent body'
s2l (Scons (Ssym "dlet") (Scons variables body)) =
  let varsContent = getVarsContent variables
      body' = s2l body
   in mkLlet Dynamic varsContent body'
s2l (Scons left right) = Lpipe (s2l left) (s2l right)
-- ¡¡ COMPLETER !!
s2l se = error ("Malformed Sexp: " ++ (show se))

-- s2l se = error ("Malformed Sexp: " ++ (showSexp se))

mkLambdaFromPipe :: Sexp -> Sexp -> Lexp
mkLambdaFromPipe Snil function = s2l function
mkLambdaFromPipe (Scons (Ssym v) right) function =
  Lfn v (mkLambdaFromPipe right function)
mkLambdaFromPipe args _ = error ("Malformed Sexp: " ++ (show args))

mkLlet :: BindingType -> [(Var, [Var], Lexp)] -> Lexp -> Lexp
mkLlet _ [] body = body
mkLlet binding ((varName, undVars, value) : xs) body =
  let value' = generateValue undVars value
   in Llet binding varName value' (mkLlet binding xs body)

getVarsContent :: Sexp -> [(Var, [Var], Lexp)]
getVarsContent Snil = []
getVarsContent (Scons left right) = getVarContent left : getVarsContent right
getVarsContent e = error ("not implemented " ++ show e)

getVarContent :: Sexp -> (Var, [Var], Lexp)
getVarContent (Scons left right) =
  let (varName, undVars) = getVarCallInfo left
   in (varName, undVars, s2l right)
getVarContent e = error ("not implemented " ++ show e)

getVarCallInfo :: Sexp -> (Var, [Var])
getVarCallInfo (Ssym v) = (v, [])
getVarCallInfo (Scons (Ssym v) right) = (v, getVar right)
getVarCallInfo e = error ("not implemented " ++ show e)

generateValue :: [Var] -> Lexp -> Lexp
generateValue [] lexp = lexp
generateValue (x : xs) lexp = Lfn x (generateValue xs lexp)

getUndVars :: Sexp -> [Var]
getUndVars Snil = []
getUndVars (Scons (Ssym var) right) = var : getUndVars right
getUndVars _ = error "not implemented"

mkLcons :: Sexp -> Lexp
mkLcons (Scons (Ssym tag) content) = Lcons tag (mkLconsExpo content)
mkLcons _ = error "not implemented"

mkLconsExpo :: Sexp -> [Lexp]
mkLconsExpo Snil = []
mkLconsExpo (Scons left right) = (s2l left) : mkLconsExpo right
mkLconsExpo _ = error "not implemented"

mkBranches :: Sexp -> [(Pat, Lexp)]
mkBranches (Scons (Scons (Ssym "_") right) _) = [(Nothing, s2l right)]
mkBranches (Scons left right) = (mkPatern left) : mkBranches right
mkBranches _ = error "not implemented"

mkPatern :: Sexp -> (Pat, Lexp)
mkPatern (Scons left right) = (getTag left, s2l right)
mkPatern _ = error "not implemented"

getTag :: Sexp -> Pat
getTag (Scons (Ssym tag) right) = Just (tag, getVar right)
getTag _ = error "not implemented"

getVar :: Sexp -> [Var]
getVar Snil = []
getVar (Scons (Ssym var) right) = var : getVar right
getVar _ = error "not implemented"

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value
  = Vnum Int
  | Vcons Tag [Value]
  | Vfn (Env -> Value -> Value)

instance Show Value where
  showsPrec p (Vnum n) = showsPrec p n
  showsPrec p (Vcons tag vs) =
    let showTail [] = showChar ']'
        showTail (v : vs') =
          showChar ' ' . showsPrec p v . showTail vs'
     in showChar '[' . showString tag . showTail vs
  showsPrec _ (Vfn _) =
    showString ("<function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 =
  let false = Vcons "false" []
      true = Vcons "true" []
      mkbop (name, op) =
        ( name,
          Vfn
            ( \_ (Vnum x) ->
                Vfn
                  ( \_ (Vnum y) ->
                      Vnum (x `op` y)
                  )
            )
        )
      mkcmp (name, op) =
        ( name,
          Vfn
            ( \_ (Vnum x) ->
                Vfn
                  ( \_ (Vnum y) ->
                      if x `op` y then true else false
                  )
            )
        )
   in [ ("false", false),
        ("true", true)
      ]
        ++ map
          mkbop
          [ ("+", (+)),
            ("*", (*)),
            ("/", div),
            ("-", (-))
          ]
        ++ map
          mkcmp
          [ ("<=", (<=)),
            ("<", (<)),
            (">=", (>=)),
            (">", (>)),
            ("=", (==))
          ]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n
eval [] [] (Lvar _) = error "Var not found in senv and denv "
eval ((var, val) : _) [] (Lvar s) | var == s = val
eval (_ : _senvs) [] s@(Lvar _) = eval _senvs [] s
eval _senv ((var, val) : _) (Lvar s) | var == s = val
eval _senv (_ : _denvs) s@(Lvar _) = eval _senv _denvs s
eval _senv _denv (Lfn var func) =
  Vfn (\env value -> eval ((var, value) : _senv) env func)
eval _senv _denv (Lcons tag content) =
  Vcons tag (evalLconsList _senv _denv content)
eval _senv _denv (Lcase cons patterns) =
  let Vcons tag content = eval _senv _denv cons
      (vars, lexp) = getMatchingPattern tag patterns
      senv' = generateEnv vars content ++ _senv
   in eval senv' _denv lexp
eval _senv _denv (Llet Lexical var value lexp) =
  eval ((var, eval _senv _denv value) : _senv) _denv lexp
eval _senv _denv (Llet Dynamic var value lexp) =
  eval _senv ((var, eval _senv _denv value) : _denv) lexp
eval _senv _denv (Lpipe left right) =
  case eval _senv _denv right of
    Vfn fn ->
      let arg = eval _senv _denv left
       in fn _denv arg
    e -> error ("Can't eval: " ++ show e)

evalLconsList :: Env -> Env -> [Lexp] -> [Value]
evalLconsList _ _ [] = []
evalLconsList _senv _denv (x : xs) =
  eval _senv _denv x : evalLconsList _senv _denv xs

getMatchingPattern :: Var -> [(Pat, Lexp)] -> ([Var], Lexp)
getMatchingPattern _ [] = error "Empty pattern"
getMatchingPattern _ ((Nothing, lexp) : _xs) = ([], lexp)
getMatchingPattern tag ((Just (tag', vars), lexp) : _xs) =
  if tag == tag'
    then (vars, lexp)
    else getMatchingPattern tag _xs

generateEnv :: [Var] -> [Value] -> [(Var, Value)]
generateEnv [] _ = []
generateEnv (x : xs) (y : ys) = (x, y) : generateEnv xs ys
generateEnv _ _ = error "not implemented"

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
  do
    s <- readFile filename
    (hPutStr stdout . show)
      ( let sexps s' = case parse pSexps filename s' of
              Left _ -> [Ssym "#<parse-error>"]
              Right es -> es
         in map evalSexp (sexps s)
      )
