{-# LANGUAGE InstanceSigs #-}

module Parser
    ( token
    , symbol
    , ident
    , identif
    , rexp
    , expr
    , bin
    , term
    , pow
    , factor
    , var
    , digiti
    , digit
    , relop
    , addop
    , binop
    , mulop
    , powop
    , unaryop
    , com
    , assign
    , seqv
    , cond
    , while
    , declare
    , printe
    , apply
    , applyAll
    ) where

        import Data.Char
        import Data.Maybe
        import Text.ParserCombinators.ReadP

        import AST

        token :: ReadP a -> ReadP a
        token p = do
            a <- p
            skipSpaces
            return a

        symbol :: String -> ReadP String
        symbol cs = token (string cs)

        ident :: ReadP String
        ident = do
            c <- satisfy isAlpha
            cs <- many (satisfy (\ a -> isAlpha a || isDigit a))
            return (c : cs)

        identif :: ReadP String
        identif = token ident

        rexp :: ReadP Exp
        rexp = chainl1 expr relop

        expr :: ReadP Exp
        expr = chainl1 bin addop

        bin :: ReadP Exp
        bin = chainl1 term binop

        term :: ReadP Exp
        term = chainl1 pow mulop

        pow :: ReadP Exp
        pow = chainl1 factor powop

        factor :: ReadP Exp
        factor = var +++ digiti +++ do
            symbol "("
            n <- rexp
            symbol ")"
            return n

        var :: ReadP Exp
        var = Variable <$> identif

        digiti :: ReadP Exp
        digiti = (do
            op <- unaryop
            op <$> digiti) +++ do
            p <- digit
            l <- many digit
            return (foldl (\ a b -> let Constant a' = a
                                        Constant b' = b
                                     in Constant (10 * a' + b')) (Constant 0) (p : l))

        digit :: ReadP Exp
        digit = do
            x <- token (satisfy isDigit)
            return (Constant (ord x - ord '0'))

        relop :: ReadP (Exp -> Exp -> Exp)
        relop = eq +++ ne +++ lt +++ lte +++ gt +++ gte
            where eq = do
                      symbol "="
                      return Equal
                  ne = do
                      symbol "!="
                      return NotEqual
                  lt = do
                      symbol "<"
                      return Less
                  lte = do
                      symbol "<="
                      return LessEqual
                  gt = do
                      symbol ">"
                      return Greater
                  gte = do
                      symbol ">="
                      return GreaterEqual

        addop :: ReadP (Exp -> Exp -> Exp)
        addop = plus +++ minus
            where plus = do
                      symbol "+"
                      return Plus
                  minus = do
                      symbol "-"
                      return Minus

        binop :: ReadP (Exp -> Exp -> Exp)
        binop = and +++ or +++ xor
            where and = do
                      symbol "&"
                      return And
                  or = do
                      symbol "|"
                      return Or
                  xor = do
                      symbol "^"
                      return Xor

        mulop :: ReadP (Exp -> Exp -> Exp)
        mulop = mult +++ div
            where mult = do
                      symbol "*"
                      return Times
                  div = do
                      symbol "/"
                      return Div

        powop :: ReadP (Exp -> Exp -> Exp)
        powop = do
            symbol "**"
            return Pow
        
        unaryop :: ReadP (Exp -> Exp)
        unaryop = unaryMinus +++ complement +++ not
            where unaryMinus = do
                      symbol "-"
                      return UnaryMinus
                  complement = do
                      symbol "~"
                      return Complement
                  not = do
                      symbol "!"
                      return Not

        com :: ReadP Com
        com = assign +++ seqv +++ cond +++ while +++ declare +++ printe

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
        
        printe :: ReadP Com
        printe = do
            symbol "print"
            Print <$> rexp

        apply :: ReadP a -> String -> Maybe (a, String)
        apply p s = toMaybe $ readP_to_S apply' s
            where toMaybe []      = Nothing
                  toMaybe (x : _) = Just x
                  apply' = do
                      skipSpaces
                      p

        applyAll :: ReadP a -> String -> [a]
        applyAll p s = let maybe = apply p s
                        in if isNothing maybe
                            then []
                            else let (a, unparsed) = fromJust maybe
                                  in if null unparsed
                                      then [a]
                                      else a : applyAll p unparsed
