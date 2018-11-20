{-# LANGUAGE InstanceSigs, LambdaCase, TupleSections #-}

import Control.Monad
import qualified Control.Applicative as Applicative
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char

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

newtype Parser a = Parser {
        parse :: String -> [(a, String)]
}

instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)]) 
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

instance Applicative.Alternative Parser where
    (<|>) = mplus
    empty = mzero 

item :: Parser Char
item = Parser (\case
    "" -> []
    (c : cs) -> [(c, cs)])

instance MonadPlus Parser where
    mzero       = Parser (const [])
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
    [] -> []
    (x : xs) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c
        then return c
        else mzero

(?) :: Parser a -> (a -> Bool) -> Parser a
p ? test = do
    b <- p
    if test b
        then return b
        else mzero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string s@(c : cs) = do
    char c
    string cs
    return s

many :: Parser a -> Parser [a]
many p = many' p +++ return []
    where many' p = do { c <- p
                       ; cs <- many p
                       ; return (c : cs) }

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do
    a <- p
    space
    return a

symbol :: String -> Parser String
symbol cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do
    space
    p)

ident :: Parser String
ident = do
    c <- sat isAlpha
    cs <- many (sat (\a -> isAlpha a || isDigit a))
    return (c : cs)

identif :: Parser String
identif = token ident

var :: Parser Exp
var = Variable <$> identif

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl'` op) +++ return a

chainl' :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl'` op = do
    a <- p
    rest a
    where
        rest a = (do
            f <- op
            b <-p
            rest (f a b)) +++ return a

digit :: Parser Exp
digit = do
    x <- token (sat isDigit)
    return (Constant (ord x - ord '0'))

digiti :: Parser Exp
digiti = do
    p <- digit
    l <- many digit
    return (foldl (\a b -> let Constant a' = a
                               Constant b' = b
                            in Constant (10 * a' + b')) (Constant 0) (p : l))

rexp :: Parser Exp
rexp = expr `chainl'` relop

expr :: Parser Exp
expr = term `chainl'` addop

term :: Parser Exp
term = factor `chainl'` mulop

factor :: Parser Exp
factor = var +++ digiti +++ do
    symbol "("
    n <- rexp
    symbol ")"
    return n

addop :: Parser (Exp -> Exp -> Exp)
addop = do
    symbol "-"
    return Minus +++ do
        symbol "+"
        return Plus

mulop :: Parser (Exp -> Exp -> Exp)
mulop = do
    symbol "*"
    return Times +++ do
        symbol "/"
        return Div

relop :: Parser (Exp -> Exp -> Exp)
relop = do
    symbol ">"
    return Greater +++ do
        symbol "<"
        return Less +++ do
            symbol "="
            return Equal
  
printe :: Parser Com
printe = do
    symbol "print"
    Print <$> rexp

assign :: Parser Com
assign = do
    x <- identif
    symbol ":="
    Assign x <$> rexp

seqv :: Parser Com
seqv = do
    symbol "{"
    c <- com
    symbol ";" 
    d <- com
    symbol "}"
    return (Seq c d)

cond :: Parser Com
cond = do
    symbol "if"
    e <- rexp
    symbol "then"
    c <- com
    symbol "else"
    Cond e c <$> com

while :: Parser Com
while = do
    symbol "while"
    e <- rexp
    symbol "do"
    While e <$> com

declare :: Parser Com
declare = do
    symbol "declare"
    x <- identif
    symbol "="
    e <- rexp 
    symbol "in"
    Declare x e <$> com

com :: Parser Com
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

test :: (Stack, Output, ())
test = interpret (exec $ fst $ head $ parse com "declare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\ndeclare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\n") Map.empty
