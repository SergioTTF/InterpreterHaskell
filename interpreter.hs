{-# LANGUAGE InstanceSigs, LambdaCase, TupleSections #-}

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

data Exp = Constant Int | Variable String | Plus Exp Exp | Minus Exp Exp | Greater Exp Exp | Less Exp Exp | Equal Exp Exp | Times Exp Exp | Div Exp Exp deriving Show

data Com = Assign String Exp | Seq Com Com | Cond Exp Com Com | While Exp Com | Declare String Exp Com | Print Exp deriving Show

type Identifier = String
type Value = Int
type Stack = Map.Map Identifier Value
type Output = String
newtype Interpreter a = StateOutput {
    interpret :: Stack -> (Stack, Output, a)
}

instance Monad Interpreter where
    return x = StateOutput (, "", x)
    m >>= f  = StateOutput (\s -> let (s1, o1, a1) = interpret m s
                                      (s2, o2, a2) = interpret (f a1) s1
                                   in (s2, o1 ++ o2, a2))

instance Applicative Interpreter where
    pure  = return
    (<*>) = ap

instance Functor Interpreter where
    fmap = liftM

eval :: Exp -> Interpreter Value
eval exp = case exp of
    Constant n -> return n
    Variable x -> var x
        where var id = StateOutput (\stack -> (stack, "", fromJust (Map.lookup id stack)))
    Plus x y -> do
        a <- eval x
        b <- eval y
        return (a + b)
    Minus x y -> do
        a <- eval x
        b <- eval y
        return (a - b)
    Times x y -> do
        a <- eval x
        b <- eval y
        return (a * b)
    Div x y -> do
        a <- eval x
        b <- eval y
        return (a `div` b)
    Less x y -> do
        a <- eval x
        b <- eval y
        return $ fromBool (a < b)
    Greater x y -> do
        a <- eval x
        b <- eval y
        return $ fromBool (a > b)
    Equal x y -> do
        a <- eval x
        b <- eval y
        return $ fromBool (a == b)

exec :: Com -> Interpreter () 
exec stmt = case stmt of
    Declare id exp s -> do
        v <- eval exp
        declare id v
        exec s
        destroy id
        where declare id val = StateOutput (\stack -> (Map.insert id val stack, "", ()))
              destroy id = StateOutput (\stack -> (Map.delete id stack, "", ()))
    Assign id val -> do
        v <- eval val
        assign id v
        where assign id val = StateOutput (\stack -> (Map.adjust (const val) id stack, "", ()))
    Cond exp s1 s2 -> do
        v <- eval exp
        if v == 1
            then exec s1
            else exec s2
    a@(While exp s) -> do
        v <- eval exp
        if v == 0
            then return ()
            else do
                exec s
                exec a
    Print exp -> do
        v <- eval exp
        output v
        where output v = StateOutput (, show v, ())
    Seq s1 s2 -> do
        exec s1
        exec s2

token :: ReadP a -> ReadP a
token p = do
    a <- p
    skipSpaces
    return a

symbol :: String -> ReadP String
symbol cs = token (string cs)

apply :: ReadP a -> String -> [(a, String)]
apply p = readP_to_S $ do
    skipSpaces
    p

ident :: ReadP String
ident = do
    c <- satisfy isAlpha
    cs <- many (satisfy (\a -> isAlpha a || isDigit a))
    return (c : cs)

identif :: ReadP String
identif = token ident

var :: ReadP Exp
var = Variable <$> identif

digit :: ReadP Exp
digit = do
    x <- token (satisfy isDigit)
    return (Constant (ord x - ord '0'))

digiti :: ReadP Exp
digiti = do
    p <- digit
    l <- many digit
    return (foldl (\a b -> let Constant a' = a
                               Constant b' = b
                            in Constant (10 * a' + b')) (Constant 0) (p : l))

rexp :: ReadP Exp
rexp = chainl1 expr relop

expr :: ReadP Exp
expr = chainl1 term addop

term :: ReadP Exp
term = chainl1 factor mulop

factor :: ReadP Exp
factor = var +++ digiti +++ do
    symbol "("
    n <- rexp
    symbol ")"
    return n

addop :: ReadP (Exp -> Exp -> Exp)
addop = do
    symbol "-"
    return Minus +++ do
        symbol "+"
        return Plus

mulop :: ReadP (Exp -> Exp -> Exp)
mulop = do
    symbol "*"
    return Times +++ do
        symbol "/"
        return Div

relop :: ReadP (Exp -> Exp -> Exp)
relop = do
    symbol ">"
    return Greater +++ do
        symbol "<"
        return Less +++ do
            symbol "="
            return Equal
  
printe :: ReadP Com
printe = do
    symbol "print"
    Print <$> rexp

assign :: ReadP Com
assign = do
    x <- identif
    symbol ":="
    Assign x <$> rexp

seqv :: ReadP Com
seqv = do
    symbol "{"
    c <- com
    symbol ";" 
    d <- com
    symbol "}"
    return (Seq c d)

cond :: ReadP Com
cond = do
    symbol "if"
    e <- rexp
    symbol "then"
    c <- com
    symbol "else"
    Cond e c <$> com

while :: ReadP Com
while = do
    symbol "while"
    e <- rexp
    symbol "do"
    While e <$> com

declare :: ReadP Com
declare = do
    symbol "declare"
    x <- identif
    symbol "="
    e <- rexp 
    symbol "in"
    Declare x e <$> com

com :: ReadP Com
com = assign +++ seqv +++ cond +++ while +++ declare +++ printe

s :: Com
s = Declare "x" (Constant 150) $
    Declare "y" (Constant 200) $
    Seq
        (While
            (Greater (Variable "x") (Constant 0))
            (Seq
                (Assign "x" (Minus (Variable "x") (Constant 1)))
                (Assign "y" (Minus (Variable "y") (Constant 1)))
            )
        )
        (Print (Variable "y"))

s' = apply com "declare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\ndeclare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\n"

test :: (Stack, Output, ())
test = interpret (exec $ fst $ head s') Map.empty
