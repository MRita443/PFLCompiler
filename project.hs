-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

import Data.Char
import Debug.Trace
import Data.List (sortBy)
import Data.Function (on)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Const
  = Int Integer
  | TT
  | FF
  deriving (Show)

type Stack = [Const]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = "The stack is empty"
stack2Str [Int x] = show x
stack2Str [TT] = "True"
stack2Str [FF] = "False"
stack2Str (Int x : xs) = show x ++ "," ++ stack2Str xs
stack2Str (TT : xs) = "True," ++ stack2Str xs
stack2Str (FF : xs) = "False," ++ stack2Str xs

type State = [(String, Const)]

createEmptyState :: State
createEmptyState = []

mySort :: Ord a => [(a, b)] -> [(a, b)]
mySort = sortBy (compare `on` fst)

state2Str :: State -> String
state2Str [] = "The storage is empty"
state2Str [(variable, Int x)] = variable ++ "=" ++ show x
state2Str [(variable, TT)] = variable ++ "=True"
state2Str [(variable, FF)] = variable ++ "=False"
state2Str ((variable, Int x) : xs) = variable ++ "=" ++ show x ++ "," ++ state2Str xs
state2Str ((variable, TT) : xs) = variable ++ "=True," ++ state2Str xs
state2Str ((variable, FF) : xs) = variable ++ "=False," ++ state2Str xs

-- TODO: Acrescentar os restantes tipos de codigo ao run

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n : xs), stack, state) = run (xs, Int n : stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

newtype VarLiteral = VarLit String deriving (Show)

data Aexp = AddLit Aexp Aexp | MultLit Aexp Aexp | SubLit Aexp Aexp | NumLit Integer | VarLiteral deriving (Show) -- Este tipos são da linguagem high level

data Bexp = IntEqLit Aexp Aexp | BoolEqLit Bexp Bexp | LessEqLit Aexp Aexp | AndLit Bexp Bexp | NegLit Bexp | TrueLit | FalseLit deriving (Show)

data Stm = WhileLit Bexp Aexp | IfLit Bexp Aexp (Maybe Aexp) | AtrALit VarLiteral Aexp | AtrBLit VarLiteral Bexp deriving (Show)

type Program = [Stm]

-- TODO: Switch Aexps for parseA

compA :: Aexp -> Code
compA (AddLit a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (MultLit a1 a2) = compA a1 ++ compA a2 ++ [Mult]
compA (SubLit a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (NumLit a1) = [Push a1]

compB :: Bexp -> Code
compB (IntEqLit a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (BoolEqLit b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (LessEqLit a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndLit b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (NegLit b1) = compB b1 ++ [Neg]
compB TrueLit = [Tru]
compB FalseLit = [Fals]

-- compile :: Program -> Code
-- compile = compBool (Code)

data Token
  = PlusTok
  | TimesTok
  | MinusTok
  | DivTok
  | OpenParTok
  | CloseParTok
  | IfTok
  | ThenTok
  | ElseTok
  | WhileTok
  | DoTok
  | AtrTok
  | IntEqTok
  | LessTok
  | LessEqTok
  | GreaterTok
  | GreaterEqTok
  | NegTok
  | AndTok
  | BoolEqTok
  | TrueTok
  | FalseTok
  | DelimTok
  | IntTok Integer
  | VarTok String
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
  | isAlpha c = lexWord (c : cs)
  | isDigit c = lexNum (c : cs)
  | isSpace c = lexer cs
  | otherwise = lexSymbol (c : cs)

lexWord :: String -> [Token]
lexWord cs = case span isAlpha cs of
  ("if", rest) -> IfTok : lexer rest
  ("then", rest) -> ThenTok : lexer rest
  ("else", rest) -> ElseTok : lexer rest
  ("while", rest) -> WhileTok : lexer rest
  ("do", rest) -> DoTok : lexer rest
  ("not", rest) -> NegTok : lexer rest
  ("and", rest) -> AndTok : lexer rest
  ("True", rest) -> TrueTok : lexer rest
  ("False", rest) -> FalseTok : lexer rest
  (var, rest) -> VarTok var : lexer rest

lexNum :: String -> [Token]
lexNum cs = IntTok (read num) : lexer rest
  where
    (num, rest) = span isDigit cs

lexNegNum :: String -> [Token]
lexNegNum cs = IntTok (-(read num)) : lexer rest
  where
    (num, rest) = span isDigit cs

lexSymbol :: String -> [Token]
lexSymbol ('=' : '=' : cs) = IntEqTok : lexer cs
lexSymbol (':' : '=' : cs) = AtrTok : lexer cs
lexSymbol ('<' : '=' : cs) = LessEqTok : lexer cs
lexSymbol ('>' : '=' : cs) = GreaterEqTok : lexer cs
lexSymbol ('(' : '-' : cs) = OpenParTok : lexNegNum cs
lexSymbol (c : cs) = case c of
  '<' -> LessTok : lexer cs
  '>' -> GreaterTok : lexer cs
  '+' -> PlusTok : lexer cs
  '-' -> MinusTok : lexer cs
  '*' -> TimesTok : lexer cs
  '/' -> DivTok : lexer cs
  '=' -> BoolEqTok : lexer cs
  '(' -> OpenParTok : lexer cs
  ')' -> CloseParTok : lexer cs
  ';' -> DelimTok : lexer cs
  where
    (d : ds) = cs

{-
Parsing Order (Reverse of priority)
 #Each function calls the next one so higher priority operations are done first

 - Numerical Expressions: Sum/Subtraction -> Product/Division -> Integer/Parentheses
 - Boolean Expressions: And -> Boolean Equality -> Not -> Integer Equality/Integer Inequality
-}

{- parse :: String -> Program
parse = undefined -- TODO -}

{- parse :: [Token] -> Program
parse tokens =
  case parseSum tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error" -}

parseA :: [Token] -> Aexp
parseA tokens =
  case parseSum tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

parseIntPar :: [Token] -> Maybe (Aexp, [Token])
parseIntPar (IntTok n : restTokens) = Just (NumLit n, restTokens)
parseIntPar (OpenParTok : restTokens1) =
  case parseSum restTokens1 of
    Just (expr, CloseParTok : restTokens2) ->
      Just (expr, restTokens2)
    Just x -> trace ("parseIntPar: " ++ show x) error "Syntax Error: Missing closing parenthesis" -- TODO: Error no closing paren
    Nothing -> Nothing
parseIntPar tokens = trace (show tokens) Nothing

parseProd :: [Token] -> Maybe (Aexp, [Token])
parseProd tokens =
  case parseIntPar tokens of
    Just (expr1, TimesTok : restTokens1) ->
      case parseProd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MultLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseProd: " ++ show result ++ "\n") result

parseSum :: [Token] -> Maybe (Aexp, [Token])
parseSum tokens =
  case parseProd tokens of
    Just (expr1, PlusTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AddLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseSum: " ++ show result ++ "\n") result

parseLessOrIntEq :: [Token] -> Maybe (Bexp, [Token])
parseLessOrIntEq (TrueTok : restTokens) = Just (TrueLit, restTokens)
parseLessOrIntEq (FalseTok : restTokens) = Just (FalseLit, restTokens)
parseLessOrIntEq (OpenParTok : restTokens) =
  case parseAnd restTokens of
    Just (expr, CloseParTok : restTokens2) ->
      Just (expr, restTokens2)
    Just x -> trace ("parsePar: " ++ show x ++ "\n") error "Syntax Error: Missing closing parenthesis" -- TODO: Error no closing paren
    Nothing -> Nothing
parseLessOrIntEq tokens =
  case parseSum tokens of
    Just (expr1, LessEqTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (LessEqLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, IntEqTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (IntEqLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseLessOrIntEq: " ++ show result ++ "\n") Nothing

parseNeg :: [Token] -> Maybe (Bexp, [Token])
parseNeg (NegTok : restTokens) =
  case parseLessOrIntEq restTokens of
    Just (expr1, restTokens1) ->
      Just (NegLit expr1, restTokens1)
    result -> trace ("parseNeg: " ++ show result ++ "\n") result
parseNeg tokens = parseLessOrIntEq tokens

parseBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNeg tokens of
    Just (expr1, BoolEqTok : restTokens1) ->
      case parseBoolEq restTokens1 of
        Just (expr2, restTokens2) ->
          Just (BoolEqLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseBoolEq: " ++ show result ++ "\n") result

parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAnd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AndLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseAnd: " ++ show result ++ "\n") result

-- TODO: Make it so statements can contain other statements
parseStatement :: [Token] -> Maybe (Stm, [Token])
parseStatement (WhileTok : restTokens) =
  -- Could have chosen to enforce the need for parentheses after while and do, but chose not to
  case parseAnd restTokens of
    Just (expr1, DoTok : restTokens1) ->
      case parseSum restTokens1 of -- TODO: Replace for function that parses a bunch of statements prob parseA
        Just (expr2, restTokens2) ->
          Just (WhileLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> trace ("parseStatement: " ++ show result ++ "\n") Nothing
parseStatement (IfTok : restTokens) =
  -- Could have chosen to enforce the need for parentheses after if, else and then, but chose not to
  case parseAnd restTokens of
    Just (expr1, ThenTok : restTokens1) ->
      case parseSum restTokens1 of -- TODO: Replace for function that parses a bunch of statements prob parseA
        Just (expr2, ElseTok : restTokens2) ->
          case parseSum restTokens2 of
            Just (expr3, restTokens3) ->
              Just (IfLit expr1 expr2 (Just expr3), restTokens3)
            Nothing -> trace "parseStatement: AfterThen \n" Nothing
        Just (expr2, restTokens2) ->
          Just (IfLit expr1 expr2 Nothing, restTokens2)
        Nothing -> trace "parseStatement: AfterThen \n" Nothing
    result -> trace ("parseStatement: " ++ show result ++ "\n") Nothing
parseStatement (VarTok x : AtrTok : restTokens) =
  case parseSum restTokens of
    Just (expr1, restTokens1) ->
      Just (AtrALit (VarLit x) expr1, restTokens1)
    _ -> case parseAnd restTokens of
      Just (expr2, restTokens2) ->
        Just (AtrBLit (VarLit x) expr2, restTokens2)
      result -> trace ("parseStatement: " ++ show result ++ "\n") Nothing

-- TODO: Ver caso em que só se fornece um parenteses de fecho


-- To help you test your parser
{- testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState) -}

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")