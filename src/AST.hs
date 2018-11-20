module AST
    ( Value
    , Identifier
    , Exp
        ( Constant
        , Variable
        , UnaryMinus
        , Plus
        , Minus
        , Times
        , Div
        , Pow
        , Complement
        , And
        , Or
        , Xor
        , Not
        , Equal
        , NotEqual
        , Less
        , LessEqual
        , Greater
        , GreaterEqual
        )
    , Com
        ( Declare
        , Assign
        , Cond
        , While
        , Seq
        , Print
        )
    ) where

        type Value = Int

        type Identifier = String

        data Exp = Constant Value
                 | Variable Identifier
                 | UnaryMinus Exp
                 | Plus Exp Exp
                 | Minus Exp Exp
                 | Times Exp Exp
                 | Div Exp Exp
                 | Pow Exp Exp
                 | Complement Exp
                 | And Exp Exp
                 | Or Exp Exp
                 | Xor Exp Exp
                 | Not Exp
                 | Equal Exp Exp
                 | NotEqual Exp Exp
                 | Less Exp Exp
                 | LessEqual Exp Exp
                 | Greater Exp Exp
                 | GreaterEqual Exp Exp
                 deriving (Eq, Show)

        data Com = Declare Identifier Exp Com
                 | Assign Identifier Exp
                 | Cond Exp Com Com
                 | While Exp Com
                 | Seq Com Com
                 | Print Exp
                 deriving (Eq, Show)
