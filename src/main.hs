module Project where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Either ()
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (trace)

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst
  = Push Integer
  | Add
  | Mult
  | Sub
  | Tru
  | Fals
  | Equ
  | Le
  | And
  | Neg
  | Fetch String
  | Store String
  | Noop
  | Branch Code Code
  | Loop Code Code
  deriving (Show)

type Code = [Inst]

data BoolConst = TT | FF deriving (Eq)

instance Show BoolConst where
  show TT = "True"
  show FF = "False"

and :: BoolConst -> BoolConst -> BoolConst
and TT TT = TT
and _ _ = FF

neg :: BoolConst -> BoolConst
neg TT = FF
neg FF = TT

data Const
  = Int Integer
  | BoolConst BoolConst

instance Show Const where
  show (Int a) = show a
  show (BoolConst x) = show x

type Stack = [Const]

-- | Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- | Converts a stack given as input to a string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = show x
stack2Str (x : xs) = show x ++ "," ++ stack2Str xs

type State = Map String Const

-- | Creates an empty storage
createEmptyState :: State
createEmptyState = Map.empty

-- | Converts a storage given as input to a string
state2Str :: State -> String
state2Str state
  | Map.null state = ""
  | otherwise = removeTrailingComma $ intercalate "," $ Map.foldrWithKey accumulate [] state
  where
    accumulate key value acc = (key ++ "=" ++ show value) : acc
    removeTrailingComma str =
      if not (null str) && last str == ','
        then init str
        else str

-- | Runs the list of instructions (Code) returning as ouput an empty code list, a stack and the output values in the storage
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Push n : xs, stack, state) = run (xs, Int n : stack, state)
run (Add : xs, Int n1 : Int n2 : stack, state) = run (xs, Int (n1 + n2) : stack, state)
run (Mult : xs, Int n1 : Int n2 : stack, state) = run (xs, Int (n1 * n2) : stack, state)
run (Sub : xs, Int n1 : Int n2 : stack, state) = run (xs, Int (n1 - n2) : stack, state)
run (Tru : xs, stack, state) = run (xs, BoolConst TT : stack, state)
run (Fals : xs, stack, state) = run (xs, BoolConst FF : stack, state)
run (Equ : xs, Int n1 : Int n2 : stack, state)
  | n1 == n2 = run (xs, BoolConst TT : stack, state)
  | otherwise = run (xs, BoolConst FF : stack, state)
run (Equ : xs, BoolConst x : BoolConst y : stack, state) =
  if x == y
    then run (xs, BoolConst TT : stack, state)
    else run (xs, BoolConst FF : stack, state)
run (Le : xs, Int n1 : Int n2 : stack, state)
  | n1 <= n2 = run (xs, BoolConst TT : stack, state)
  | otherwise = run (xs, BoolConst FF : stack, state)
run (And : xs, BoolConst x : BoolConst y : stack, state) = run (xs, BoolConst (Project.and x y) : stack, state)
run (Neg : xs, BoolConst x : stack, state) = run (xs, BoolConst (Project.neg x) : stack, state)
run (Fetch x : xs, stack, state) =
  case Map.lookup x state of
    Just val -> run (xs, val : stack, state)
    Nothing -> error "Run-time error"
run (Store x : xs, val : stack, state) = run (xs, stack, Map.insert x val state)
run (Noop : xs, stack, state) = run (xs, stack, state)
run (Branch c1 c2 : xs, BoolConst TT : stack, state) = run (c1 ++ xs, stack, state)
run (Branch c1 c2 : xs, BoolConst FF : stack, state) = run (c2 ++ xs, stack, state)
run (Loop c1 c2 : xs, stack, state) = run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ xs, stack, state)
run (_, _, _) = error "Run-time error"

-- | Helps test the assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

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

data Aexp = AddLit Aexp Aexp | MultLit Aexp Aexp | SubLit Aexp Aexp | NumLit Integer | VarLit String deriving (Show)

data Bexp = IntEqLit Aexp Aexp | BoolEqLit Bexp Bexp | LessEqLit Aexp Aexp | AndLit Bexp Bexp | NegLit Bexp | TrueLit | FalseLit deriving (Show)

data Stm = WhileLit Bexp [Stm] | IfLit Bexp [Stm] (Maybe [Stm]) | AtrLit String (Either Aexp Bexp) deriving (Show)

type Program = [Stm]

-- | Compiles arithmetic expressions
compA :: Aexp -> Code
compA (AddLit a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MultLit a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SubLit a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (NumLit a1) = [Push a1]
compA (VarLit a1) = [Fetch a1]

-- | Compiles boolean expressions
compB :: Bexp -> Code
compB (IntEqLit a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BoolEqLit b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LessEqLit a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AndLit b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (NegLit b1) = compB b1 ++ [Neg]
compB TrueLit = [Tru]
compB FalseLit = [Fals]

-- | Auxiliary function to compile statements
compileStm :: Stm -> Code
compileStm (AtrLit variable exp) =
  case exp of
    Left aExp -> compA aExp ++ [Store variable]
    Right bExp -> compB bExp ++ [Store variable]
compileStm (WhileLit bExp body) =
  let bodyCode = compile body
      condCode = compB bExp
   in [Loop condCode bodyCode]
compileStm (IfLit condStm thenStm maybeElseStm) =
  let thenCode = compile thenStm
      elseCode = maybe [] compile maybeElseStm
      condCode = compB condStm
   in condCode ++ [Branch thenCode elseCode]

-- | Main compiler function
compile :: Program -> Code
compile = concatMap compileStm

data Token
  = PlusTok
  | TimesTok
  | MinusTok
  | OpenParTok
  | CloseParTok
  | IfTok
  | ThenTok
  | ElseTok
  | WhileTok
  | DoTok
  | AtrTok
  | IntEqTok
  | LessEqTok
  | NegTok
  | AndTok
  | BoolEqTok
  | TrueTok
  | FalseTok
  | DelimTok
  | IntTok Integer
  | VarTok String
  deriving (Show, Eq)

-- | Tokenizes a string
lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
  | isAlpha c = lexWord (c : cs)
  | isDigit c = lexNum (c : cs)
  | isSpace c = lexer cs
  | otherwise = lexSymbol (c : cs)

-- | Tokenizes a word
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

-- | Tokenizes a number
lexNum :: String -> [Token]
lexNum cs = IntTok (read num) : lexer rest
  where
    (num, rest) = span isDigit cs

-- | Tokenizes a negative number
lexNegNum :: String -> [Token]
lexNegNum cs = IntTok (-(read num)) : lexer rest
  where
    (num, rest) = span isDigit cs

-- | Tokenizes a symbol
lexSymbol :: String -> [Token]
lexSymbol ('=' : '=' : cs) = IntEqTok : lexer cs
lexSymbol (':' : '=' : cs) = AtrTok : lexer cs
lexSymbol ('<' : '=' : cs) = LessEqTok : lexer cs
lexSymbol ('(' : '-' : cs) = OpenParTok : lexNegNum cs
lexSymbol (c : cs) = case c of
  '+' -> PlusTok : lexer cs
  '-' -> MinusTok : lexer cs
  '*' -> TimesTok : lexer cs
  '=' -> BoolEqTok : lexer cs
  '(' -> OpenParTok : lexer cs
  ')' -> CloseParTok : lexer cs
  ';' -> DelimTok : lexer cs
  where
    (d : ds) = cs

{-
Parsing Order (Reverse of priority)
 #Each function calls the next one so higher priority operations are done first

 - Numerical Expressions: Sum/Subtraction -> Product -> Integer/Variables/Parentheses
 - Boolean Expressions: And -> Boolean Equality -> Not -> Integer Equality/Integer Inequality
-}

-- | Parses a program language string into literals
parse :: String -> Program
parse input = parseTokens (lexer input) []

-- | Parses a list of tokens
parseTokens :: [Token] -> Program -> Program
parseTokens [] currProgram = currProgram
parseTokens tokens currProgram =
  case parseStatement tokens of
    Just (lits, rest) -> parseTokens rest (currProgram ++ [lits])
    _ -> error "Parse error"

-- | Parses integer, variable or parenthesis tokens
parseIntVarPar :: [Token] -> Maybe (Aexp, [Token])
parseIntVarPar (IntTok n : restTokens) = Just (NumLit n, restTokens)
parseIntVarPar (VarTok x : restTokens) = Just (VarLit x, restTokens)
parseIntVarPar (OpenParTok : restTokens1) =
  case parseSum restTokens1 of
    Just (expr, CloseParTok : restTokens2) ->
      Just (expr, restTokens2)
    Just x -> error "Syntax Error: Missing closing parenthesis"
    Nothing -> Nothing
parseIntVarPar tokens = Nothing

-- | Parses product tokens
parseProd :: [Token] -> Maybe (Aexp, [Token])
parseProd tokens =
  case parseIntVarPar tokens of
    Just (expr1, TimesTok : restTokens1) ->
      case parseProd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MultLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- | Parses sum tokens
parseSum :: [Token] -> Maybe (Aexp, [Token])
parseSum tokens =
  case parseProd tokens of
    Just (expr1, PlusTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AddLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, MinusTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SubLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- | Parses integer comparison tokens
parseLessEqBools :: [Token] -> Maybe (Bexp, [Token])
parseLessEqBools (TrueTok : restTokens) = Just (TrueLit, restTokens)
parseLessEqBools (FalseTok : restTokens) = Just (FalseLit, restTokens)
parseLessEqBools (OpenParTok : restTokens) =
  case parseAnd restTokens of
    Just (expr, CloseParTok : restTokens2) ->
      Just (expr, restTokens2)
    Just x -> error "Syntax Error: Missing closing parenthesis"
    Nothing -> Nothing
parseLessEqBools tokens =
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
    result -> Nothing

-- | Parses negation tokens
parseNeg :: [Token] -> Maybe (Bexp, [Token])
parseNeg (NegTok : restTokens) =
  case parseLessEqBools restTokens of
    Just (expr1, restTokens1) ->
      Just (NegLit expr1, restTokens1)
    result -> result
parseNeg tokens = parseLessEqBools tokens

-- | Parses boolean equality tokens
parseBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNeg tokens of
    Just (expr1, BoolEqTok : restTokens1) ->
      case parseBoolEq restTokens1 of
        Just (expr2, restTokens2) ->
          Just (BoolEqLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- | Parses and tokens
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAnd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AndLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- | Parses the code block relative to an else statement
parseElse :: [Token] -> Bexp -> [Stm] -> Maybe (Stm, [Token])
parseElse tokens boolExpr thenStms =
  case parseStatements tokens of
    Just (elseStms, DelimTok : restTokens1) ->
      Just (IfLit boolExpr thenStms (Just elseStms), restTokens1)
    Just (elseStms, restTokens2) ->
      if length elseStms == 1
        then Just (IfLit boolExpr thenStms (Just elseStms), restTokens2) -- If there is only one statement inside else, we do not need an else specific delimiter
        else error "Syntax Error: Missing delimiter after else statement" -- If there is more than one statement, it is an error
    Nothing -> Nothing

-- | Parses the code block relative to a then (and possible else) statement
parseThen :: [Token] -> Bexp -> Maybe (Stm, [Token])
parseThen tokens boolExpr =
  case parseStatements tokens of
    Just (thenStms, DelimTok : ElseTok : restTokens) ->
      -- There is an Else after Then
      parseElse restTokens boolExpr thenStms
    Just (thenStms, ElseTok : restTokens) ->
      parseElse restTokens boolExpr thenStms
    Just (thenStms, DelimTok : restTokens) ->
      -- There is no Else after Then
      Just (IfLit boolExpr thenStms Nothing, restTokens)
    Just (thenStms, restTokens) ->
      if length thenStms == 1
        then Just (IfLit boolExpr thenStms Nothing, restTokens) -- If there is only one statement inside then, we do not need a do specific delimiter
        else error "Syntax Error: Missing delimiter after then statement" -- If there is more than one statement, it is an error
    Nothing -> Nothing

-- | Parses a single statement
parseStatement :: [Token] -> Maybe (Stm, [Token])
parseStatement (WhileTok : restTokens) =
  -- Could have chosen to enforce the need for parentheses after while and do, but chose not to
  case parseAnd restTokens of
    Just (boolExpr, DoTok : restTokens1) ->
      case parseStatements restTokens1 of
        Just (doStms, DelimTok : restTokens2) ->
          Just (WhileLit boolExpr doStms, restTokens2)
        Just (doStms, restTokens2) ->
          if length doStms == 1
            then Just (WhileLit boolExpr doStms, restTokens2) -- If there is only one statement inside do, we do not need a do specific delimiter
            else error "Syntax Error: Missing delimiter after while...do statement" -- If there is more than one statement, it is an error
        Nothing -> Nothing
    Just _ -> error "Syntax Error: Missing do statement after while"
    Nothing -> Nothing
parseStatement (IfTok : restTokens) =
  -- Could have chosen to enforce the need for parentheses after if, else and then, but chose not to
  case parseAnd restTokens of
    Just (boolExpr, ThenTok : restTokens1) ->
      parseThen restTokens1 boolExpr
    result -> error "Syntax Error: Missing then statement after if"
parseStatement (VarTok x : AtrTok : restTokens) =
  case parseSum restTokens of
    Just (aExp, DelimTok : restTokens1) ->
      Just (AtrLit x (Left aExp), restTokens1)
    Just y -> error "Syntax Error: Missing delimiter after attribution statement"
    _ -> case parseAnd restTokens of
      Just (bExp, DelimTok : restTokens2) ->
        Just (AtrLit x (Right bExp), restTokens2)
      Just _ -> error "Syntax Error: Missing delimiter after attribution statement"
      Nothing -> Nothing
parseStatement _ = error "Syntax Error: Invalid statement"

-- | Recursive auxiliar function for parsing a block of multiple statements
parseStatementsAux :: [Token] -> [Stm] -> Maybe ([Stm], [Token])
parseStatementsAux tokens currStms =
  case parseStatement tokens of
    Just (stms, CloseParTok : restTokens) ->
      -- Found the closing parenthesis delimiting the block's end
      Just (currStms ++ [stms], restTokens)
    Just (stms1, restTokens1) -> parseStatementsAux restTokens1 (currStms ++ [stms1])
    Nothing -> Nothing

-- | Parses a block of (possibly) multiple statements
parseStatements :: [Token] -> Maybe ([Stm], [Token])
parseStatements (OpenParTok : restTokens) =
  -- Parenthesis indicate a block of multiple statements
  if CloseParTok `elem` restTokens
    then parseStatementsAux restTokens []
    else error "Syntax Error: Missing closing parenthesis after block of statements"
parseStatements tokens =
  -- Lack of parenthesis indicate a single statement
  case parseStatement tokens of
    Just (stm, restTokens) ->
      Just ([stm], restTokens) -- We chose not to check for ; here in order to get more comprehensive error messages from the parseStatements function

-- | Helps test the parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

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
