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

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp = AddA Aexp Aexp | MultA Aexp Aexp | SubA Aexp Aexp | Num Integer --Este tipos são da linguagem high level
data Bexp = EqualA Aexp Aexp | EqualB Bexp Bexp | LessEqual Aexp Aexp | AndB Bexp Bexp | NegB Bexp | TrueB | FalseB

compA :: Aexp -> Code
compA (AddA a1 a2) = (compA a1) ++ (compA a2) ++ [Add]
compA (MultA a1 a2) = (compA a1) ++ (compA a2) ++ [Mult]
compA (SubA a1 a2) = (compA a1) ++ (compA a2) ++ [Sub]
compA (Num a1) = [Push a1]

compB :: Bexp -> Code
compB (EqualA a1 a2) = (compA a1) ++ (compA a2) ++ [Equ]
compB (EqualB b1 b2) = (compB b1) ++ (compB b2) ++ [Equ]
compB (LessEqual a1 a2) = (compA a1) ++ (compA a2) ++ [Le]
compB (AndB b1 b2) = (compB b1) ++ (compB b2) ++ [And]
compB (NegB b1) = (compB b1) ++ [Neg]
compB (TrueB) = [Tru]
compB (FalseB) = [Fals]

--compile :: Program -> Code
--compile = compB (Code)

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
  | IntTok Int
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

lexSymbol :: String -> [Token]
lexSymbol ('=':'=':cs) = IntEqTok : lexer cs
lexSymbol (':':'=':cs) = AtrTok : lexer cs
lexSymbol ('<':'=':cs) = LessEqTok : lexer cs
lexSymbol ('>':'=':cs) = GreaterEqTok : lexer cs
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


-- parse :: String -> Program
parse = undefined -- TODO

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