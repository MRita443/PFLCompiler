# Compiler
## Functional and Logic Programming - Practical Assignment 2
### T05_G05

- Diogo Filipe Couto Monteiro: 50%
- Maria Rita Nogueira Lopes: 50%

## Part 1

## Part 2

To tackle this part of the assignment, we started by defining the _data_ we would use to represent arithmetic or boolean expressions, and the language's statements, as well as a type synonym to represent a program:

```haskell
data Aexp = AddLit Aexp Aexp | MultLit Aexp Aexp | SubLit Aexp Aexp | NumLit Integer | VarLit String deriving (Show)

data Bexp = IntEqLit Aexp Aexp | BoolEqLit Bexp Bexp | LessEqLit Aexp Aexp | AndLit Bexp Bexp | NegLit Bexp | TrueLit | FalseLit deriving (Show)

data Stm = WhileLit Bexp [Stm] | IfLit Bexp [Stm] (Maybe [Stm]) | AtrLit String (Either Aexp Bexp) deriving (Show)

type Program = [Stm]
```