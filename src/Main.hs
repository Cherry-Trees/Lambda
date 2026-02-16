module Main where
import Parser 
    (Val(VInt), pTopExpr, pStmt, parseSrc)
import Interpreter 
    (AST(eval), SymTable)
import qualified Data.Map as M
import System.IO 
    (hFlush, stdout)
import Control.Monad.State 
    (runState)
import Control.Monad.Except 
    (runExceptT)

-- A symbol table with infix operators
prelude :: SymTable
prelude = M.fromList [ ("(||)", vOr)
                     , ("(&&)", vAnd)
                     , ("(>=)", vGTE)
                     , ("(>)", vGT)
                     , ("(<=)", vLTE)
                     , ("(<)", vLT)
                     , ("(==)", vEqual)
                     , ("(!=)", vNotEqual)
                     , ("(+)", vAdd)
                     , ("(-)", vSub)
                     , ("(*)", vMul)
                     , ("(/)", vDiv)
                     , ("(%)", vMod)
                     , ("negate", vNegate)
                     , ("not", vNot)
                     ]
    where
        vOr         = interpret "\\x -> \\y -> x || y"
        vAnd        = interpret "\\x -> \\y -> x && y"
        vGTE        = interpret "\\x -> \\y -> x >= y"
        vGT         = interpret "\\x -> \\y -> x > y"
        vLTE        = interpret "\\x -> \\y -> x <= y"
        vLT         = interpret "\\x -> \\y -> x < y"
        vEqual      = interpret "\\x -> \\y -> x == y"
        vNotEqual   = interpret "\\x -> \\y -> x != y"
        vAdd        = interpret "\\x -> \\y -> x + y"
        vSub        = interpret "\\x -> \\y -> x - y"
        vMul        = interpret "\\x -> \\y -> x * y"
        vDiv        = interpret "\\x -> \\y -> x / y"
        vMod        = interpret "\\x -> \\y -> x % y"
        vNegate     = interpret "\\x -> -x"
        vNot        = interpret "\\x -> !x"

        -- Interpret an expression.
        interpret src = case parseSrc pTopExpr src of
            Right expr -> case runState (runExceptT $ eval expr) M.empty of
                (Right val, _) -> val
                _ -> VInt 0 -- This should never happen.
            _ -> VInt 0     -- Neither should this.

-- REPL
main :: IO ()
main = do
    putStrLn "Lambda REPL, :q to exit"
    repl prelude
    where
        repl :: SymTable -> IO ()
        repl symTable = do
            putStr "> "
            hFlush stdout
            line <- getLine
            case line of
                "" -> repl symTable
                ":q" -> putStrLn "Leaving Lambda REPL."
                src -> case parseSrc pStmt src of
                        Left err -> do
                            print err
                            repl symTable
                        Right stmt -> do
                            let (result, symTable') = runState (runExceptT $ eval stmt) symTable
                            case result of
                                Left err -> putStrLn err
                                Right val -> print val
                            repl symTable'