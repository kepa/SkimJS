import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e
evalExpr env (ArrayLit []) = return $ List []
evalExpr env (ArrayLit (expr:exprs)) = do
    l <- evalExpr env expr
    List ls <- evalExpr env (ArrayLit exprs)
    return $ List (l:ls)
evalExpr env (StringLit str) = return $ String str
evalExpr env (FuncExpr (Nothing) args stmt) = return $ Function (Id "") args stmt
evalExpr env (FuncExpr (Just id) args stmt) = return $ Function id args stmt
-- BracketRef Expression {- container -} Expression {- key -}
evalExpr env (BracketRef expr index) = do
    ls <- evalExpr env expr
    i <- evalExpr env index
    case ls of
        List l -> do
            getElement env ls i
        _ -> error $ "Object must be a list"
evalExpr env (CallExpr (VarRef (Id name)) params) = do
    Function nameFunct args stmt <- stateLookup env name
    declareArgs env args params
    evalStmt env (BlockStmt stmt)
evalExpr env (CallExpr (DotRef expr (Id name)) params) = do
    e <- evalExpr env expr
    case name of
        "head" -> head' env e
        "tail" -> tail' env e
        "concat" -> concat' env e params
        "length" -> intToST env (lengthInt e)
        -- "equals" -> equalsToST env (equalsBool env e params)

-- getElement from list
getElement :: StateT -> Value -> Value -> StateTransformer Value
getElement env (List []) i = error $ "Out of bounds"
getElement env (List (a:as)) i = do
    case i of
        Int index -> do
            if index < 0 then error $ "Index cannot be negative"
            else if index == 0 then return a
            else getElement env (List as) (Int (index-1))
        _ -> error $ "Index must be an integer"

-- função para declarar as variáveis do parâmetro da função
declareArgs :: StateT -> [Id] -> [Expression] -> StateTransformer Value
-- declareArgs env args params
declareArgs env [] [] = return Nil
declareArgs env ((Id a):as) (p:ps) = do
    var <- evalExpr env p
    setVar a var
    declareArgs env as ps
declareArgs env _ _ = return Nil

-- Funções da lista
-- equals' env list expr
-- expr = list2

-- Tá dando erro porque a é um Value e b é uma Expression

{-equalsBool :: StateT -> Value -> [Expression] -> Bool
equalsBool env (List []) [] = True
equalsBool env (List (a:as)) (b:bs)
   | lengthInt (List(a:as)) /= length (b:bs) = False
   | a /= b = False
   | otherwise = equalsBool env (List as) bs-}

equalsToST :: StateT -> Bool -> StateTransformer Value
equalsToST env val = return $ (Bool val)

-- head' env list
head' :: StateT -> Value -> StateTransformer Value
head' env (List []) = return Nil
head' env (List (a:as)) = return $ a

-- tail' env list
tail' :: StateT -> Value -> StateTransformer Value
tail' env (List []) = return Nil
tail' env (List (a:as)) = return $ (List as)

-- concat' env list expr
-- [expr: list2]
-- não funciona
concat' :: StateT -> Value -> [Expression] -> StateTransformer Value
concat' env (List ls) [] = return $ List ls
concat' env (List ls) (a1:as1) = do
    a <- evalExpr env a1
--    List as1 <- evalExpr env as1
    concat' env (List (ls ++ [a])) as1

-- lengthInt env list -> Int
lengthInt :: Value -> Int
lengthInt (List []) = 0
lengthInt (List (a:as)) = 1 + lengthInt (List as)

-- intToST
intToST :: StateT -> Int -> StateTransformer Value
intToST env val = return $ (Int val)

{-evalExpr env (CallExpr name params) = do
    func <- evalExpr env name
    case func of
        Function nameFunct args stmt -> do
            b <- evalStmt env (BlockStmt stmt)
            case b of
                Break -> return Break
                Return r -> return r
                _ -> return Nil
        _ -> return Nil -- var "name" is not a function-}


evalStmt :: StateT -> Statement -> StateTransformer Value
-- Block
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt [stm]) = evalStmt env stm
evalStmt env (BlockStmt (stm:stmts)) = do
    s <- evalStmt env stm
    case s of
        Break -> return Nil
        Return a -> return (Return a)
        _ -> evalStmt env (BlockStmt stmts)
-- Empty
evalStmt env EmptyStmt = return Nil
--VarDecl
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
-- Expr
evalStmt env (ExprStmt expr) = evalExpr env expr
-- IFSingle
evalStmt env (IfSingleStmt expr stm) = do
    Bool e <- evalExpr env expr
    if e then evalStmt env stm else return Nil
-- IFStmt
evalStmt env (IfStmt expr stm1 stm2) = do
    Bool e <- evalExpr env expr
    if e then evalStmt env stm1 else evalStmt env stm2
-- While
evalStmt env (WhileStmt expr stmt) = do
    Bool e <- evalExpr env expr
    if e then do
        s <- evalStmt env stmt
        case s of
            Break -> return Nil
            Return a -> return (Return a)
            _ -> evalStmt env (WhileStmt expr stmt)
    else return Nil
-- Function
evalStmt env (FunctionStmt (Id name) args stmt) = setVar name (Function (Id name) args stmt)
-- Return
evalStmt env (ReturnStmt Nothing) = return $ Return Nil
evalStmt env (ReturnStmt (Just expr)) = do
    e <- evalExpr env expr
    return $ Return e

--Inicio Break
evalStmt env (BreakStmt _) = return Break
--Fim Break

--FOR Não sei se esse é o lugar correto.

evalStmt env (ForStmt initi condi itera action) = ST $ \s ->
    let
        (ST a) = evalStmt env EmptyStmt
        (ignore, newS) = a s
        (ST g) = do
            evalFor env initi        
            case condi of
                (Just (a)) -> do 
                    Bool tf <- evalExpr env a
                    if tf then do
                        r1 <- evalStmt env action
                        case r1 of 
                            Break -> return Nil
                            (Return a) -> return (Return a)
                            _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
                    else return Nil
                Nothing -> do 
                    r1 <- evalStmt env action
                    case r1 of
                        Break -> return Nil
                        (Return a) -> return (Return a)
                        _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
        (resp,ign) = g newS
    in (resp,ign)   

evalFor :: StateT -> ForInit -> StateTransformer Value
evalFor env (VarInit a) = do
    evalStmt env (VarDeclStmt a)
evalFor env NoInit = return Nil
evalFor env (ExprInit b) = evalExpr env b
--fim for

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Int  v1) (Int  v2) = return $ Bool $ v1 /= v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defined."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
