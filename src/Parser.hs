module Parser where
import Text.Parsec.Text 
    (Parser)
import Text.Parsec.Char
    ( spaces, char, string, lower, alphaNum, digit )
import Text.Parsec 
    (many, try, parse, ParseError, (<?>), between, chainl1, many1)
import Control.Monad 
    (void)
import Control.Applicative 
    (Alternative((<|>)))
import Data.Text 
    (pack)

data BinaryOp = BArith (Integer -> Integer -> Integer)
              | BCompare (Integer -> Integer -> Bool)
              | BLogic (Bool -> Bool -> Bool)

instance Show BinaryOp where
    show _ = "<op>"

data UnaryOp = UArith (Integer -> Integer)
             | ULogic (Bool -> Bool)

instance Show UnaryOp where
    show _ = "<op>"

-- The data that can be bound to a top-level name
-- Three data types are implemented: integers, booleans, and lambda abstractions.
data Val = VInt Integer
         | VBool Bool
         | Lambda String TopExpr

instance Show Val where
    show (VInt int) = show int
    show (VBool bool) = show bool
    show (Lambda name expr) = '\\' : name ++ " -> " ++ show expr

-- The bottom-level of expressions
data Term = Var String Int  -- Variables store their name and De Bruijn index.
          | App Term Term
          | Val Val
          | Parens TopExpr
          | If TopExpr TopExpr TopExpr

instance Show Term where
    show (Var name _) = name
    show (App fun arg) = show fun ++ " " ++ show arg
    show (Val val) = show val
    show (Parens expr) = '(' : show expr ++ ")"
    show (If condExpr thenExpr elseExpr) = "if " ++ show condExpr ++ " then " ++ show thenExpr ++ " else " ++ show elseExpr

-- Expression tree
data Expr a = Binary BinaryOp (Expr a) (Expr a)
            | Unary UnaryOp (Expr a)
            | Single a

instance Show a => Show (Expr a) where
    show (Binary op expr1 expr2) = show expr1 ++ " " ++ show op ++ " " ++ show expr2
    show (Unary op expr) = show op ++ " " ++ show expr
    show (Single x) = show x

instance Functor Expr where
    fmap f (Binary op expr1 expr2) = Binary op (fmap f expr1) $ fmap f expr2
    fmap f (Unary op expr) = Unary op $ fmap f expr
    fmap f (Single a) = Single $ f a

-- Aliases for precedence levels
type TopExpr = Or
type Or = Expr And
type And = Expr Equal
type Equal = Expr Rel
type Rel = Expr Add
type Add = Expr Factor
type Factor = Expr Term

-- Top level declaration
data Stmt = Assign String TopExpr
          | Expr TopExpr

pLex :: Parser a -> Parser a
pLex p = p <* spaces

pChar :: Char -> Parser Char
pChar = pLex . char

pCharV :: Char -> Parser ()
pCharV = void . pChar

pString :: String -> Parser String
pString = try . pLex . string

pStringV :: String -> Parser ()
pStringV = void . pString

pParens :: Parser a -> Parser a
pParens = between (pChar '(') (pChar ')')

-- Parse just a name.
pName' :: Parser String
pName' = do
    c <- lower <|> char '_'
    cs <- pLex $ many $ alphaNum <|> char '_'
    let name = c : cs
    if name `elem` ["if", "then", "else"]
        then fail $ show name ++ "is reserved"
        else return name
    <?> "name"

-- Parse a name or an infix binary operator.
pName :: Parser String
pName = pInfixOp <|> pName' <?> "name"
    where
        pInfixOp = foldl1 (<|>) $ pString <$> infixOpStrings
        infixOpStrings = [ "(||)"
                         , "(&&)"
                         , "(<=)"
                         , "(<)"
                         , "(>=)"
                         , "(>)"
                         , "(==)"
                         , "(!=)"
                         , "(+)"
                         , "(-)"
                         , "(*)"
                         , "(/)"
                         , "(%)"
                         ]

pOrOp, pAndOp, pEqualOp, pRelOp, pAddOp, pMulOp :: Parser BinaryOp
pOrOp         = BLogic <$> do { pStringV "||"; return (||) }
pAndOp        = BLogic <$> do { pStringV "&&"; return (&&) }
pEqualOp = fmap BCompare $ do { pStringV "=="; return (==) }
                       <|> do { pStringV "!="; return (/=) }
pRelOp   = fmap BCompare $ do { pCharV '<'; return (<) }
                       <|> do { pCharV '>'; return (>) }
                       <|> do { pStringV "<="; return (<=) }
                       <|> do { pStringV ">="; return (>=) }
pAddOp     = fmap BArith $ do { pCharV '+'; return (+) } 
                       <|> do { pCharV '-'; return (-) }
pMulOp     = fmap BArith $ do { pCharV '*'; return (*) } 
                       <|> do { pCharV '/'; return div }
                       <|> do { pCharV '%'; return mod }

pNotOp, pNegateOp :: Parser UnaryOp
pNotOp    = ULogic <$> do { pCharV '!'; return not }
pNegateOp = UArith <$> do { pCharV '-'; return negate }

-- Parse an expression with no unary operator.
pExpr :: Parser a -> Parser BinaryOp -> Parser (Expr a)
pExpr p op = pBinary <|> pSingle <?> "expression"
    where
        pSingle = Single <$> p
        pBinary = pSingle `chainl1` (Binary <$> op)

-- Parse an expression with a unary operator.
pExpr' :: Parser a -> Parser BinaryOp -> Parser UnaryOp -> Parser (Expr a)
pExpr' p pBinOp pUnOp = pUnary <|> pExpr p pBinOp <?> "expression"
    where
        pUnary = Unary <$> pUnOp <*> pExpr' p pBinOp pUnOp

pTopExpr :: Parser TopExpr
pTopExpr = pOr 

pOr :: Parser Or
pOr = pExpr pAnd pOrOp

pAnd :: Parser And
pAnd = pExpr pEqual pAndOp

pEqual :: Parser Equal
pEqual = pExpr' pRel pEqualOp pNotOp

pRel :: Parser Rel
pRel = pExpr pAdd pRelOp

pAdd :: Parser Add
pAdd = pExpr pFactor pAddOp

pFactor :: Parser Factor
pFactor = pExpr' pTerm pMulOp pNegateOp

pTerm :: Parser Term
pTerm = pVInt <|> pVBool <|> pIf <|> pApp <|> pLambda <|> pVar
    where
        pVInt = pLex $ Val . VInt . read <$> many1 digit
        pVBool = pLex $ Val . VBool . read <$> (pString "True" <|> pString "False")
        pVar = Var <$> pName <*> (do { pCharV '@'; read <$> many1 digit } <|> pure 0)
        pLambda = Val <$> (Lambda <$> do { pCharV '\\'; pName } <*> do { pStringV "->"; pTopExpr })
        pApp = pSubExpr `chainl1` pure App
        pSubExpr = try (Parens <$> pParens pTopExpr) <|> pLambda <|> try pVar <|> pVInt <|> pVBool <|> pIf
        pIf = If <$> do { pStringV "if"; pTopExpr } 
                 <*> do { pStringV "then"; pTopExpr } 
                 <*> do { pStringV "else"; pTopExpr }

pStmt :: Parser Stmt
pStmt = try pAssign <|> (Expr <$> pTopExpr)
    where 
        pAssign = (Assign <$> pName' <*> (fun <$> many pName' <*> do { pCharV '='; pTopExpr })) <?> "assignment"
        fun [] expr = expr
        fun (param : params) expr = wrap $ Lambda param (fun params expr)
        wrap = Single . Single . Single . Single . Single . Single . Val

-- Run a parser on source code.
parseSrc :: Parser a -> String -> Either ParseError a
parseSrc p =  parse p "" . pack