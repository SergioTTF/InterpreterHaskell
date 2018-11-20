{-# LANGUAGE InstanceSigs, TupleSections #-}

module Interpreter
    ( Interpreter
    , interpret
    , eval
    , exec
    ) where

        import Control.Applicative (Alternative, empty, (<|>))
        import Control.Monad
        import Data.Bits
        import qualified Data.Map.Strict as Map
        import Data.Maybe

        import AST
        import Lib

        type Stack = Map.Map Identifier Value
        type Output = String

        newtype Interpreter a = StateOutput {
            interpret :: Stack -> (Stack, Output, a)
        }

        instance Monad Interpreter where
            return x = StateOutput (, "", x)
            m >>= f  = StateOutput (\ s -> let (s1, o1, a1) = interpret m s
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
                where var id = StateOutput (\ stack -> (stack, "", fromJust (Map.lookup id stack)))
            UnaryMinus x -> do
                a <- eval x
                return (negate a)
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
            Pow x y -> do
                a <- eval x
                b <- eval y
                return (a ^ b)
            Complement x -> do
                a <- eval x
                return (complement a)
            And x y -> do
                a <- eval x
                b <- eval y
                return (a .&. b)
            Or x y -> do
                a <- eval x
                b <- eval y
                return (a .|. b)
            Xor x y -> do
                a <- eval x
                b <- eval y
                return (a `xor` b)
            Not x -> do
                a <- eval x
                return $ fromBool (a == 0)
            Equal x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a == b)
            NotEqual x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a /= b)
            Less x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a < b)
            LessEqual x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a <= b)
            Greater x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a > b)
            GreaterEqual x y -> do
                a <- eval x
                b <- eval y
                return $ fromBool (a >= b)

        exec :: Com -> Interpreter () 
        exec stmt = case stmt of
            Declare id exp s -> do
                v <- eval exp
                declare id v
                exec s
                destroy id
                where declare id val = StateOutput (\ stack -> (Map.insert id val stack, "", ()))
                      destroy id = StateOutput (\ stack -> (Map.delete id stack, "", ()))
            Assign id val -> do
                v <- eval val
                assign id v
                where assign id val = StateOutput (\ stack -> (Map.adjust (const val) id stack, "", ()))
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
                where output v = StateOutput (, show v ++ "\n", ())
            Seq s1 s2 -> do
                exec s1
                exec s2
