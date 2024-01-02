# Compiler
## Functional and Logic Programming - Practical Assignment 2
### _T06_G05_

- Diogo Filipe Couto Monteiro: 50%
- Maria Rita Nogueira Lopes: 50%

## Part 1

For this part of the assignment, we started by creating a new type for the stack and the storage, the stack being a list of both integers and the constants TT and FF (`BoolConst`), while the storage is a map of a string variable with the same type as the values. We did it this way with Map instead of a list of tuples, since with Map they would already be sorted alphabetically and it would also be more useful since the function itself allows variables not to be repeated and is easier to access.

```haskell
data BoolConst = TT | FF deriving (Eq)

instance Show BoolConst where
  show TT = "True"
  show FF = "False"

data Const
  = Int Integer
  | BoolConst BoolConst

instance Show Const where
  show (Int a) = show a
  show (BoolConst x) = show x

type Stack = [Const]

type State = Map String Const
```

Creating the empty stack and storage functions was straightforward while transforming them into a string was done element by element for the stack and for the storage. We created a function that converts a non-empty Map representing a state into a string where each key-value pair is represented as "key=value" and separated by commas, and it ensures there is no trailing comma at the end of the generated string.

```haskell
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = show x
stack2Str (x : xs) = show x ++ "," ++ stack2Str xs

createEmptyState :: State
createEmptyState = Map.empty

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
```

For the last part of this section, we created the function `run` that runs the list of instructions returning as output an empty code list, a stack and the output values in the storage. We followed what the template said about what the various basic arithmetic and boolean operations did, as well as the six instructions that modify the evaluation stack. Running a wrong configuration raises an exception with the string: ”Run-time error”.

```haskell
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
```

## Part 2

To tackle this part of the assignment, we started by defining the _data_ we would use to represent arithmetic or boolean expressions, and the language's statements, as well as a type synonym to represent a program:

```haskell
data Aexp = AddLit Aexp Aexp | MultLit Aexp Aexp | SubLit Aexp Aexp | NumLit Integer | VarLit String deriving (Show)

data Bexp = IntEqLit Aexp Aexp | BoolEqLit Bexp Bexp | LessEqLit Aexp Aexp | AndLit Bexp Bexp | NegLit Bexp | TrueLit | FalseLit deriving (Show)

data Stm = WhileLit Bexp [Stm] | IfLit Bexp [Stm] (Maybe [Stm]) | AtrLit String (Either Aexp Bexp) deriving (Show)

type Program = [Stm]
```
A key highlight of these definitions is that we chose to use `Maybe` to represent the `else` case of an if-statement because we assumed it is optional. We also made use of `Either` to represent our variable assignments, because we interpreted that variables could be assigned boolean or arithmetic values. 

Then, we defined the `compA` and `compB` functions, respectively, to compile lists of the previously defined arithmetic and boolean _data_ into lists of machine instructions.

```haskell
compA :: Aexp -> Code
compA (AddLit a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MultLit a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SubLit a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (NumLit a1) = [Push a1]
compA (VarLit a1) = [Fetch a1]

compB :: Bexp -> Code
compB (IntEqLit a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BoolEqLit b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LessEqLit a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AndLit b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (NegLit b1) = compB b1 ++ [Neg]
compB TrueLit = [Tru]
compB FalseLit = [Fals]
```

Here, we made sure to place in the instruction list operations that required multiple arguments after the output of compiling said arguments.

Besides these mandatory functions, we also defined an auxiliary function to compile statements:

```haskell
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
```
This function mainly deals with the different types of expressions each statement requires and calls the appropriate compiling functions to construct the final instruction list.

Finally, the function `compile` applies the previous function to the entirety of the parsed program, concatenating all parts.

```haskell
compile :: Program -> Code
compile = concatMap compileStm
```

We then started to construct the `parser` function. To achieve this, we divided it into two main parts: the lexer and the parser itself.

The lexer takes the raw program string and tokenizes it into our chosen _data_.

```haskell
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
```

To achieve this, the lexer is divided into four auxiliary functions:

```haskell
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
```
To successfully tokenize the input, we made use of the `span` function together with different `Char` categorization functions, like `isDigit` and `isAlpha`, which allowed us to easily group characters belonging to the same token.

Then, the `lexer` function forwards the execution to one of the previous functions according to the characteristics of the first character read:

```haskell
lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
  | isAlpha c = lexWord (c : cs)
  | isDigit c = lexNum (c : cs)
  | isSpace c = lexer cs
  | otherwise = lexSymbol (c : cs)
```

This function is tolerant to (but doesn't require) white space between tokens and is also able to tokenize negative numbers when presented between parentheses.

The output of this function is then passed on to the `parse` function. To develop this function, we divided it into three main parts: the parsing of arithmetic expressions, the parsing of boolean expressions, and the parsing of statements. 

The functions to parse expressions all function according to the same strategy. The first function to be called is the function in charge of parsing the lowest-priority operation. However, this function only parses after calling the function for the next higher-priority operation, and verifying there is a leftover token corresponding to the operation it parses. This makes it so the execution always reaches the parser function for the "base-case", the higher priority operations or tokens, first. In our case, these higher-priority tokens are integers, variables, and parentheses for the case of arithmetic expressions, and integer comparisons or boolean literals, for the case of boolean expressions.

To parse arithmetic expressions, we used the following functions:

```haskell
parseIntVarPar :: [Token] -> Maybe (Aexp, [Token])
parseIntVarPar (IntTok n : restTokens) = Just (NumLit n, restTokens)
parseIntVarPar (VarTok x : restTokens) = Just (VarLit x, restTokens)
parseIntVarPar (OpenParTok : restTokens1) =
  case parseSum restTokens1 of
    Just (expr, CloseParTok : restTokens2) ->
      Just (expr, restTokens2)
    Just x -> error "Syntax Error: Missing closing parenthesis"
    Nothing -> Nothing
parseIntVarPar tokens = trace (show tokens) Nothing

parseProd :: [Token] -> Maybe (Aexp, [Token])
parseProd tokens =
  case parseIntVarPar tokens of
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
    Just (expr1, MinusTok : restTokens1) ->
      case parseSum restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SubLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result
```
Whereas to parse boolean expressions, we used these functions:

```haskell
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

parseNeg :: [Token] -> Maybe (Bexp, [Token])
parseNeg (NegTok : restTokens) =
  case parseLessEqBools restTokens of
    Just (expr1, restTokens1) ->
      Just (NegLit expr1, restTokens1)
    result -> result
parseNeg tokens = parseLessEqBools tokens

parseBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNeg tokens of
    Just (expr1, BoolEqTok : restTokens1) ->
      case parseBoolEq restTokens1 of
        Just (expr2, restTokens2) ->
          Just (BoolEqLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAnd restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AndLit expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result
```

Finally, to parse statements we defined the `parseStatement` function.

```haskell
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
```
This function verifies the tokens of each type of statement, as well as its appropriate delimiters. We chose to do these verifications here instead of in a more general function to get comprehensive error messages indicating in which statement there was a missing delimiter. In the case of if statements, we considered that blocks with multiple lines of code required parentheses around them, and a delimiter afterwards if they were the last block of the if statement. The same multiple-statement rule applies to do statements.

To verify this, we used the following auxiliary functions:

```haskell
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

parseStatementsAux :: [Token] -> [Stm] -> Maybe ([Stm], [Token])
parseStatementsAux tokens currStms =
  case parseStatement tokens of
    Just (stms, CloseParTok : restTokens) ->
      -- Found the closing parenthesis delimiting the block's end
      Just (currStms ++ [stms], restTokens)
    Just (stms1, restTokens1) -> parseStatementsAux restTokens1 (currStms ++ [stms1])
    Nothing -> Nothing

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
```

Finally, we combined these functions with the lexer to obtain the final `parse` function.

```haskell
parse :: String -> Program
parse input = parseTokens (lexer input) []

parseTokens :: [Token] -> Program -> Program
parseTokens [] currProgram = currProgram
parseTokens tokens currProgram =
  case parseStatement tokens of
    Just (lits, rest) -> parseTokens rest (currProgram ++ [lits])
    _ -> error "Parse error"
```
