module Main where

    import qualified Data.Map.Strict as Map

    import Interpreter
    import Parser

    main :: IO ()
    main = do
        sourceCode <- getContents
        print $ map ((\ (_, out, _) -> out) . (\ ast -> interpret (exec ast) Map.empty)) $ applyAll com sourceCode
        where print []       = return ()
              print (x : xs) = do
                  putStr x
                  print xs
