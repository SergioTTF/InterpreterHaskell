import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map

import Test.Hspec
import Test.QuickCheck

import AST
import Lib
import Interpreter
import Parser

sourceCode :: String
sourceCode = "declare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\ndeclare x = 150 in\n declare y = 200 in\n {while x > 0 do { x:=x-1; y:=y-1 };\n print y\n }\n"

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

main :: IO ()
main = hspec $ do
    describe "Smoke Test" $ do
        it "parsers source code" $
            let Just (ast, _) = apply com sourceCode
             in ast `shouldBe` s
        it "parsers source codes" $
            let as = applyAll com sourceCode
             in as `shouldBe` [s, s]
        it "evaluates ast" $
            let Just (ast, _) = apply com sourceCode
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "50\n"
        it "respects arithmetic precedence" $
            let Just (ast, _) = apply com "print (-6 + 4 * 2 ** 3 / 5 - 7)"
                (_, out, _) = interpret (exec ast) Map.empty
            in out `shouldBe` "-7\n"
        it "respects relational precedence" $
            let Just (ast, _) = apply com "print (-6 + 4 * 2 <= 3 / 5 - 7)"
                (_, out, _) = interpret (exec ast) Map.empty
            in out `shouldBe` "0\n"
    describe "Interpreter" $ do
        it "negates" $
            let Just (ast, _) = apply com "print -1"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "-1\n"
        it "adds" $
            let Just (ast, _) = apply com "print (2 + 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "5\n"
        it "subs" $
            let Just (ast, _) = apply com "print (2 - 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "-1\n"
        it "mults" $
            let Just (ast, _) = apply com "print (2 * 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "6\n"
        it "divs" $
            let Just (ast, _) = apply com "print (2 / 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "0\n"
        it "pows" $
            let Just (ast, _) = apply com "print (2 ** 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "8\n"
        it "complements" $
            let Just (ast, _) = apply com "print ~0"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "-1\n"
        it "ands" $
            let Just (ast, _) = apply com "print (2 & 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "2\n"
        it "ors" $
            let Just (ast, _) = apply com "print (2 | 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "3\n"
        it "xors" $
            let Just (ast, _) = apply com "print (2 ^ 3)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "1\n"
        it "eqs" $
            let Just (ast, _) = apply com "print !0"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "1\n"
        it "eqs" $
            let Just (ast, _) = apply com "print (1 = 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "1\n"
        it "nes" $
            let Just (ast, _) = apply com "print (1 != 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "0\n"
        it "lts" $
            let Just (ast, _) = apply com "print (1 < 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "0\n"
        it "ltes" $
            let Just (ast, _) = apply com "print (1 <= 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "1\n"
        it "gts" $
            let Just (ast, _) = apply com "print (1 > 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "0\n"
        it "gtes" $
            let Just (ast, _) = apply com "print (1 >= 1)"
                (_, out, _) = interpret (exec ast) Map.empty
             in out `shouldBe` "1\n"
