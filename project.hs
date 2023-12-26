-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.Char

-- Part 1

-- Do not modify our definition of Inst and Code
-- TODO: Maior, maior igual, menor, divisão
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- createEmptyStack :: Stack
createEmptyStack = undefined -- TODO, Uncomment the function signature after defining Stack

-- stack2Str :: Stack -> String
stack2Str = undefined -- TODO, Uncomment all the other function type declarations as you implement them

-- createEmptyState :: State
createEmptyState = undefined -- TODO, Uncomment the function signature after defining State

-- state2Str :: State -> String
state2Str = undefined -- TODO

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

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

-- Part 2

-- TODO: Define the types NumExp, BoolExp, Stm and Program

data NumExp = AddLit NumExp NumExp | MultLit NumExp NumExp | SubLit NumExp NumExp | NumLit Integer deriving (Show)--Este tipos são da linguagem high level
data BoolExp = EqualNumLit NumExp NumExp | EqualBoolLit BoolExp BoolExp | LessEqualLit NumExp NumExp | AndLit BoolExp BoolExp | NegLit BoolExp | TrueLit | FalseLit deriving (Show)
-- data NumExp = NumExp | BoolExp
type Program = [NumExp]

compNum :: NumExp -> Code
compNum (AddLit a1 a2) = (compNum a1) ++ (compNum a2) ++ [Add]
compNum (MultLit a1 a2) = (compNum a1) ++ (compNum a2) ++ [Mult]
compNum (SubLit a1 a2) = (compNum a1) ++ (compNum a2) ++ [Sub]
compNum (NumLit a1) = [Push a1]

compBool :: BoolExp -> Code
compBool (EqualNumLit a1 a2) = (compNum a1) ++ (compNum a2) ++ [Equ]
compBool (EqualBoolLit b1 b2) = (compBool b1) ++ (compBool b2) ++ [Equ]
compBool (LessEqualLit a1 a2) = (compNum a1) ++ (compNum a2) ++ [Le]
compBool (AndLit b1 b2) = (compBool b1) ++ (compBool b2) ++ [And]
compBool (NegLit b1) = (compBool b1) ++ [Neg]
compBool (TrueLit) = [Tru]
compBool (FalseLit) = [Fals]

--compile :: Program -> Code
--compile = compBool (Code)

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
  | NotTok
  | AndTok
  | BoolEqTok
  | DelimTok
  | IntTok Integer
  | VarTok String
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isAlpha c = lexWord (c:cs)
  | isDigit c = lexNum (c:cs)
  | isSpace c = lexer cs
  | otherwise = lexSymbol (c:cs)


lexWord :: String -> [Token]
lexWord cs = case (span isAlpha cs) of
                      ("if", rest) -> IfTok : lexer rest
                      ("then", rest) -> ThenTok : lexer rest
                      ("else", rest) -> ElseTok : lexer rest
                      ("while", rest) -> WhileTok : lexer rest
                      ("do", rest) -> DoTok : lexer rest
                      ("not", rest) -> NotTok : lexer rest
                      ("and", rest) -> AndTok : lexer rest
                      (var, rest) -> VarTok var : lexer rest

lexNum :: String -> [Token]     
lexNum cs = IntTok (read num) : lexer rest
                  where (num, rest) = span isDigit cs

lexNegNum :: String -> [Token]
lexNegNum cs = IntTok (-(read num)) : lexer rest
                  where (num, rest) = span isDigit cs

lexSymbol :: String -> [Token]
lexSymbol ('=':'=':cs) = IntEqTok : lexer cs
lexSymbol (':':'=':cs) = AtrTok : lexer cs
lexSymbol ('<':'=':cs) = LessEqTok : lexer cs
lexSymbol ('>':'=':cs) = GreaterEqTok : lexer cs
lexSymbol ('(':'-':cs) = OpenParTok : lexNegNum cs
lexSymbol (c:cs) = case (c) of
                        ('<') -> LessTok : lexer cs
                        ('>') -> GreaterTok : lexer cs
                        ('+') -> PlusTok : lexer cs
                        ('-') -> MinusTok : lexer cs
                        ('*') -> TimesTok : lexer cs
                        ('/') -> DivTok : lexer cs
                        ('=') -> BoolEqTok : lexer cs
                        ('(') -> OpenParTok : lexer cs
                        (')') -> CloseParTok : lexer cs
                        (';') -> DelimTok : lexer cs
                        where (d:ds) = cs



parse :: [Token] -> NumExp
parse tokens =
  case parseSumOrProdOrIntOrPar tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

parseIntOrParenExpr :: [Token] -> Maybe (NumExp, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (NumLit n, restTokens)
parseIntOrParenExpr (OpenParTok : restTokens1)
  = case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, (CloseParTok : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- TODO: Error no closing paren 
    Nothing -> Nothing -- TODO: Error
parseIntOrParenExpr tokens = Nothing

parseProdOrIntOrPar :: [Token] -> Maybe (NumExp, [Token])
parseProdOrIntOrPar tokens
  = case parseIntOrParenExpr tokens of
    Just (expr1, (TimesTok : restTokens1)) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MultLit expr1 expr2, restTokens2)
        Nothing -> Nothing -- TODO: Error
    result -> result

parseSumOrProdOrIntOrPar::[Token] -> Maybe (NumExp, [Token])
parseSumOrProdOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AddLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, state2Str state)
  --where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")