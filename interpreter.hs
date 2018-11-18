
data Exp = Constant Int | Variable String
| Minus Exp Exp
| Greater Exp Exp | Times Exp Exp deriving Show

data Com = Assign String Exp | Seq Com Com
| Cond Exp Com Com
| While Exp Com
| Declare String Exp Com | Print Exp
deriving Show

type Location = Int 
type Index = [ String ] 
type Stack = [Int]

position :: String −> Index −> Location
position name index = 
    let pos n (nm:nms) = 
        if name == nm 
        then n
        else pos (n+1) nms in pos 1 index


fetch :: Location −> Stack −> Int
fetch n (v:vs) = if n == 1 the v else fetch (n−1) vs

put :: Location −> Int −> Stack −> Stack

newtype M a = StOut (Stack −> (a, Stack, String))

--instance Monad M where


unStOut :: ( StOut f ) -> f

getfrom :: Location −> M Int

write :: Location −> Int −> M ()

push :: Int−> M ()

eval1 :: Exp−> Index−>MInt eval1 exp index = case exp of
    Constant n −> return n
    Variable x −>let loc = position x index
    in getfrom loc
    -- incompleta

exec :: Com −> Index −> M () 
exec stmt index = case stmt of
    Assign name e −> let loc = position name index in do
        { v <−eval1 e index;
        write loc v } 
    Seq s1 s2 −> do 
        { x <− exec s1 index; y <− exec s2 index;
        return () }
    Declare nm e stmt −> do 
        { v <− eval1 e index; push v;
        exec stmt (nm:index); pop }
-- incompleta

output :: Show a => a −> M ()
output v = StOut (\n −> ((), n, show v))
