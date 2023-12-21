import System.Environment (getArgs)
import Data.Char (isAlpha)
import qualified Data.Map as Map

data Token = TInt Int | TOp Char | TId String | TAssign | TSemicolon | TLParen | TRParen
    deriving (Show, Eq)

data Expr = Literal Int 
          | Var String 
          | Assign String Expr 
          | BinaryOp Char Expr Expr
          | UnaryOp Char Expr
          | Seq [Expr]
    deriving (Show)

type SymTab = Map.Map String Int

--main function
main :: IO ()
main = do
    let filename = "test.txt"
    contents <- readFile filename
    let tokens = tokenize contents
    let symTab = interpret tokens Map.empty
    mapM_ print (Map.toList symTab)

isOperator :: Char -> Bool
isOperator c = c `elem` ['=', '+', '-', '*', ';']

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\r', '\t', '\n']

isDigit :: Char -> Bool
isDigit c = c `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

--Token
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isWhiteSpace c = tokenize cs
    | isOperator c = TOp c : tokenize cs
    | isDigit c = let (literal, rest) = tokenizeLiteral (c:cs) in TInt literal : tokenize rest
    | isAlpha c || c == '_' = let (ident, rest) = tokenizeId (c:cs) in TId ident : tokenize rest
    | otherwise = error $ "Unrecognized character: " ++ [c]

tokenizeLiteral :: String -> (Int, String)
tokenizeLiteral cs = let (numStr, rest) = span isDigit cs in (read numStr, rest)

tokenizeId :: String -> (String, String)
tokenizeId cs = span (\c -> isAlpha c || isDigit c || c == '_') cs

--Interpreter
interpret :: [Token] -> SymTab -> SymTab
interpret [] symTab = symTab
interpret tokens symTab = 
    let (restTokens, updatedSymTab) = assignment tokens symTab
    in interpret restTokens updatedSymTab

--Evaluate the expression
evaluate :: Expr -> SymTab -> Int
evaluate (Literal n) _ = n
evaluate (Var x) symTab = 
    case Map.lookup x symTab of
        Just val -> val
        Nothing  -> error $ "Undefined variable: " ++ x
evaluate (Assign x expr) symTab = 
    let val = evaluate expr symTab
    in val
evaluate (BinaryOp op expr1 expr2) symTab = 
    let val1 = evaluate expr1 symTab
        val2 = evaluate expr2 symTab
    in case op of
        '+' -> val1 + val2
        '-' -> val1 - val2
        '*' -> val1 * val2
        _   -> error "Unsupported operator"
evaluate (UnaryOp op expr) symTab = 
    let val = evaluate expr symTab
    in case op of
        '-' -> -val
        '+' -> val
        _   -> error "Unsupported unary operator"
evaluate (Seq []) symTab = 0
evaluate (Seq exprs) symTab = evaluateSeq exprs symTab
  where
    evaluateSeq :: [Expr] -> SymTab -> Int
    evaluateSeq [] _ = 0
    evaluateSeq [e] symTab = evaluate e symTab
    evaluateSeq (e:es) symTab = 
        let _ = evaluate e symTab 
        in evaluateSeq es symTab

--Assignment: Identifier = Exp;
assignment :: [Token] -> SymTab -> ([Token], SymTab)
assignment (TId lhs : TAssign : tokens) symTab =
    let (expr, TSemicolon : restTokens) = parseExp tokens symTab
        val = evaluate expr symTab
    in (restTokens, Map.insert lhs val symTab)
assignment _ _ = error "Invalid assignment syntax"

--Exp: Exp + Term | Exp - Term | Term
parseExp :: [Token] -> SymTab -> (Expr, [Token])
parseExp tokens symTab = 
    let (expr1, restTokens1) = parseTerm tokens symTab
    in case restTokens1 of
        (TOp op : rest) | op == '+' || op == '-' ->
            let (expr2, restTokens2) = parseTerm rest symTab
            in (BinaryOp op expr1 expr2, restTokens2)
        _ -> (expr1, restTokens1)

--Term: Term * Fact  | Fact
parseTerm :: [Token] -> SymTab -> (Expr, [Token])
parseTerm tokens symTab = 
    let (expr1, restTokens1) = parseFactor tokens symTab
    in case restTokens1 of
        (TOp op : rest) | op == '*' ->
            let (expr2, restTokens2) = parseFactor rest symTab
            in (BinaryOp op expr1 expr2, restTokens2)
        _ -> (expr1, restTokens1)

--Fact: ( Exp ) | - Fact | + Fact | Literal | Identifier
parseFactor :: [Token] -> SymTab -> (Expr, [Token])
parseFactor (TLParen : tokens) symTab =
    let (expr, TRParen : restTokens) = parseExp tokens symTab
    in (expr, restTokens)
parseFactor (TOp op : tokens) symTab | op == '-' || op == '+' =
    let (expr, restTokens) = parseFactor tokens symTab
    in case op of
         '-' -> (UnaryOp '-' expr, restTokens)
         '+' -> (expr, restTokens)
parseFactor (TId id : restTokens) symTab =
    case Map.lookup id symTab of
        Just val -> (Literal val, restTokens)
        Nothing  -> error $ "Undefined identifier: " ++ id
parseFactor (TInt num : restTokens) _ =
    (Literal num, restTokens)
parseFactor _ _ = error "Invalid factor"
