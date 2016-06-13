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

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (IfSingleStmt stm expr) = do
    Bool s <- evalExpr env stm
    if s then evalStmt env expr else return Nil
evalStmt env (IfStmt stm expr1 expr2) = do
    Bool s <- evalExpr env stm
    if s then evalStmt env expr1 else evalStmt env expr2
--Editei aqui(Parte do Break)
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt1:stmt2)) = do
    case stmt1 of
        BreakStmt _ -> return break
        ReturnStmt a -> do
            case a of
                (Just expr) ->
                    ST $ \s ->
                        let
                            respos = let
                                        (ST f) = evalExpr env expr
                                        (resp,ign) = f s
                                        resposta = (return resp)
                                     in resposta
                        in (respos,s)
                (Nothing) -> return Nil
        _ -> do
            e <- evalStmt env stmt1
            case e of
                (break) -> return break
                (ReturnStmt v) -> return v
                _ -> evalStmt env (BlockStmt stmt2)
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
                            break -> return Nil
                            (ReturnStmt a) -> return (ReturnStmt a)
                            _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
                    else return Nil
                Nothing -> do 
                    r1 <- evalStmt env action
                    case r1 of
                        break -> return Nil
                        (ReturnStmt a) -> return (ReturnStmt a)
                        _ -> do
                                case itera of 
                                    (Just (b)) -> evalExpr env b
                                    Nothing -> return Nil
                                evalStmt env (ForStmt NoInit condi itera action)
        (resp,ign) = g newS
    in (resp,ign)
--FIM FOR
--Inicio Break
evalStmt env (BreakStmt _ ) = return break
--Fim Break


--FOR Não sei se esse é o lugar correto.
evalFor :: StateT -> ForInit -> StateTransformer Value
evalFor env (VarInit a) = do
    evalStmt env (VarDeclStmt a)
evalFor env NoInit = return Nil
evalFor env (ExprInit b) = evalExpr env b


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
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
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
