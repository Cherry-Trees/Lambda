{-# LANGUAGE LambdaCase #-}

module Interpreter where
import Parser
    ( Stmt(..),
      Expr(..),
      Term(..),
      Val(..),
      UnaryOp(ULogic, UArith),
      BinaryOp(BLogic, BArith, BCompare) )
import qualified Data.Map as M
import Control.Monad.State 
    (MonadState(get), modify, State)
import Control.Monad.Except 
    (ExceptT, MonadError (throwError))

-- Symbol table mapping variable names to values
type SymTable = M.Map String Val

-- The interpreter environment captures a symbol table state with exception capabilities.
type Env = ExceptT String (State SymTable) 

class AST a where

    -- Evaluate an AST.
    eval :: a -> Env Val

    -- Increase the index of all variables matching the given name.
    shift :: String 
          -- ^ The variable name
          -> Int 
          -- ^ The amount to increase by
          -> Int 
          -- ^ The minimum index
          -> a 
          -- ^ The AST to shift
          -> a

    -- Substitute a value in place of a variable matching the given name and index.
    subst :: String 
          -- ^ The variable name
          -> Int 
          -- ^ The variable index
          -> Val 
          -- ^ The value to substitute in place of a variable
          -> a 
          -- ^ The AST to substitute into
          -> a

instance AST Val where
    eval = return

    subst name index new (Lambda name' expr) = Lambda name' expr'
        where
            index'
                | name == name' = index + 1
                | otherwise = index
            shifted = shift name' 1 0 new
            expr' = subst name index' shifted expr  
    subst _ _ _ val = val
    
    shift name offset minIndex (Lambda name' expr) = Lambda name' expr'
        where
            minIndex'
                | name == name' = minIndex + 1
                | otherwise = minIndex
            expr' = shift name offset minIndex' expr
    shift _ _ _ val = val

instance AST a => AST (Expr a) where
    eval (Binary op lExpr rExpr) = do
        lVal <- eval lExpr
        rVal <- eval rExpr
        case (lVal, rVal) of
            (VInt lInt, VInt rInt) -> case op of
                BArith fun -> return . VInt $ lInt `fun` rInt
                BCompare fun -> return . VBool $ lInt `fun` rInt
                _ -> throwError $ errorMessage op lVal rVal
            (VBool lBool, VBool rBool) -> case op of
                BLogic fun -> return . VBool $ lBool `fun` rBool
                _ -> throwError $ errorMessage op lVal rVal
            _ -> throwError $ errorMessage op lVal rVal
        where
            errorMessage _ lVal rVal = "Could not evaluate binary operator between " ++ show lVal ++ " and " ++ show rVal
    eval (Unary op expr) = do
        val <- eval expr
        case (op, val) of
            (UArith fun, VInt int) -> return . VInt $ fun int
            (ULogic fun, VBool bool) -> return . VBool $ fun bool
            _ -> throwError $ "Could not evaluate unary operator on " ++ show val
    eval (Single expr) = eval expr

    subst name index new = fmap $ subst name index new

    shift name offset minIndex = fmap $ shift name offset minIndex

instance AST Term where
    eval (Val val) = eval val
    eval (Var name _) = do
        symTable <- get
        case M.lookup name symTable of
            Just val -> eval val
            _ -> throwError $ "Undeclared variable: " ++ name
    eval (Parens expr) = eval expr
    eval (App fun arg) = do
        lVal <- eval fun
        rVal <- eval arg
        case lVal of
            Lambda name expr -> eval unshiftedExpr
                where
                    shiftedArg = shift name 1 0 rVal
                    substExpr = subst name 0 shiftedArg expr
                    unshiftedExpr = shift name (-1) 0 substExpr
            _ -> throwError $ "Could not apply " ++ show rVal ++ " to " ++ show lVal
    eval (If condExpr thenExpr elseExpr) = do
        condVal <- eval condExpr
        case condVal of
            VBool True -> eval thenExpr
            VBool False -> eval elseExpr
            _ -> throwError $ "If condition " ++ show condVal ++ " must be a boolean value"

    subst name index new = \case
        (Val val) -> Val val'
            where
                val' = subst' val
        (Var name' index')
            | name == name' && index == index' -> Val new
            | otherwise -> Var name' index'
        (Parens expr) -> Parens expr'
            where
                expr' = subst' expr
        (App fun arg) -> App fun' arg'
            where
                fun' = subst' fun
                arg' = subst' arg
        (If condExpr thenExpr elseExpr) -> If condExpr' thenExpr' elseExpr'
            where
                condExpr' = subst' condExpr
                thenExpr' = subst' thenExpr
                elseExpr' = subst' elseExpr
        where
            subst' :: AST a => a -> a
            subst' = subst name index new

    shift name offset minIndex = \case
        (Val val) -> Val val'
            where
                val' = shift' val
        (Var name' index) -> Var name' index'
            where
                index'
                    | name == name' && minIndex <= index = index + offset
                    | otherwise = index
        (Parens expr) -> Parens expr'
            where
                expr' = shift' expr
        (App fun arg) -> App fun' arg'
            where
                fun' = shift' fun
                arg' = shift' arg
        (If condExpr thenExpr elseExpr) -> If condExpr' thenExpr' elseExpr'
            where
                condExpr' = shift' condExpr
                thenExpr' = shift' thenExpr
                elseExpr' = shift' elseExpr
        where
            shift' :: AST a => a -> a
            shift' = shift name offset minIndex

instance AST Stmt where
    eval (Expr expr) = eval expr
    eval (Assign name expr) = do
        val <- eval expr
        modify $ M.insert name val
        return val

    subst _ _ _ stmt = stmt

    shift _ _ _ stmt = stmt