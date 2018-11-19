{-# LANGUAGE InstanceSigs, TupleSections #-}

import Control.Applicative
import Control.Monad (liftM, ap)
import qualified Data.Map.Strict as Map
import Data.Maybe

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

data Exp = Constant Int | Variable String | Minus Exp Exp | Greater Exp Exp | Times Exp Exp deriving Show

data Com = Assign String Exp | Seq Com Com | Cond Exp Com Com | While Exp Com | Declare String Exp Com | Print Exp deriving Show

type Identifier = String
type Value = Int
type Stack = Map.Map Identifier Value

type Output = String
newtype M a = StateOutput (Stack -> (Stack, Output, a))

unStateOutput :: M a -> (Stack -> (Stack, Output, a))
unStateOutput (StateOutput f) = f

instance Monad M where
    (>>=) :: M a -> (a -> M b) -> M b
    m >>= f = StateOutput (\s -> let (s1, o1, a1) = unStateOutput m s
                                     (s2, o2, a2) = unStateOutput (f a1) s1
                                 in (s2, o1 ++ o2, a2))
    return :: a -> M a
    return x = StateOutput (, "", x)
  
instance Applicative M where
    pure  = return
    (<*>) = ap

instance Functor M where
    fmap = liftM

var :: Identifier -> M Value
var id = StateOutput (\stack -> (stack, "", fromJust (Map.lookup id stack)))

eval :: Exp -> M Value
eval exp = case exp of
    Constant n -> return n
    Variable x -> var x
    Minus x y -> do
        a <- eval x
        b <- eval y
        return (a - b)
    Greater x y -> do
        a <- eval x
        b <- eval y
        return $ fromBool (a > b)
    Times x y -> do
        a <- eval x
        b <- eval y
        return (a * b)

f = unStateOutput (eval e) s
    where e = Minus (Constant 1) (Constant 2)
          s = Map.empty
          
assign :: Identifier -> Value -> M ()
assign id val = StateOutput (\stack -> (Map.adjust (const val) id stack, "", ()))

declare :: Identifier -> Value -> M ()
declare id val = StateOutput (\stack -> (Map.insert id val stack, "", ()))

destroy :: Identifier -> M ()
destroy id = StateOutput (\stack -> (Map.delete id stack, "", ()))

output :: Show a => a -> M ()
output v = StateOutput (, show v, ())

exec :: Com -> M () 
exec stmt = case stmt of
    Assign id val -> do
        v <- eval val
        assign id v
    Seq s1 s2 -> do
        exec s1
        exec s2
    Declare id exp s -> do
        v <- eval exp
        declare id v
        exec s
        destroy id
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
        output exp
