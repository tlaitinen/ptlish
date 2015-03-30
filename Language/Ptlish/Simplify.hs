module Language.Ptlish.Simplify (simplify) where

import Language.Ptlish.AST
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe 
type SimplifyM = State SimplifyState

data SimplifyState = SimplifyState {
    stNextExprId :: Int,
    stExtra      :: [(Name,Expr)],                 
    stExprMap    :: Map.Map Expr Name,
    stScope      :: Map.Map Name Expr      
    }
initState :: SimplifyState
initState = SimplifyState 1 [] Map.empty Map.empty

simplify :: Ptlish -> Ptlish
simplify ptl = let (v, s) = runState (simplify' ptl) initState in
    [ (Def n e) | (n,e) <- reverse $ stExtra s ] ++ v

simplify' :: [Stmt] -> SimplifyM [Stmt]
simplify' = (fmap catMaybes) . mapM simplifyStmt

notLambda :: Expr -> Bool
notLambda (LambdaExpr _ _) = False
notLambda _ = True

simplifyStmt :: Stmt -> SimplifyM (Maybe Stmt)
simplifyStmt (Def n e) = do
    e' <- simplifyExpr e
    s <- get
    put $ s { stScope = Map.insert n e (stScope s) }
    return $ if notLambda e'
        then Just $ (Def n e')
        else Nothing
simplifyStmt (Trigger n as) = do
    as' <- mapM simplifyAction as                             
    return $ Just $ Trigger n as'
    
simplifyAction :: Action -> SimplifyM Action
simplifyAction (Action n exprs) = do
    exprs' <- mapM simplifyExpr exprs
    return $ Action n exprs'

simplifyExpr :: Expr -> SimplifyM Expr
simplifyExpr (UnExpr op e) = do
    e' <- simplifyExpr e
    matchExpr (UnExpr op e')
simplifyExpr (BinExpr op e1 e2) = do
    e1' <- simplifyExpr e1
    e2' <- simplifyExpr e2
    matchExpr (BinExpr op e1' e2')
simplifyExpr ce@(ConstExpr _) = do
    return ce
simplifyExpr (IfExpr i t e) = do
    i' <- simplifyExpr i
    t' <- simplifyExpr t
    e' <- simplifyExpr e
    matchExpr (IfExpr i' t' e')
simplifyExpr ve@(VarExpr v e) = do
    e' <- simplifyExpr e
    return $ VarExpr v e'
simplifyExpr re@(RefExpr _) = do
    e <- normalize re
    simplifyExpr e
simplifyExpr ue@(UnboundExpr _) = return ue
simplifyExpr ae@(ApplyExpr _ _) = do
    ae' <- normalize ae
    simplifyExpr ae'
simplifyExpr le@(LambdaExpr n e) = do
    e' <- normalize e
    matchExpr (LambdaExpr n e')        
    
matchExpr :: Expr -> SimplifyM Expr
matchExpr e = do
    s <- get
    case Map.lookup e (stExprMap s) of
        Just n -> return $ RefExpr n
        Nothing -> if notLambda e 
            then do
                let n = "_" ++ (show $ stNextExprId s)
                put $ s { 
                    stExprMap = Map.insert e n (stExprMap s),
                    stScope = Map.insert n e (stScope s),
                    stNextExprId = stNextExprId s + 1, 
                    stExtra = (n,e):stExtra s
                }
                return $ RefExpr n
            else return e    
normalize :: Expr -> SimplifyM Expr
normalize (UnExpr op e) = do
    e' <- normalize e
    return $ UnExpr op e'
normalize (BinExpr op e1 e2) = do
    e1' <- normalize e1
    e2' <- normalize e2
    return $ BinExpr op e1' e2'
normalize (ConstExpr i) = return $ ConstExpr i
normalize (IfExpr i t e) = do
    i' <- normalize i
    t' <- normalize t
    e' <- normalize e
    return $ IfExpr i' t' e'
normalize (VarExpr n e) = do
    e' <- normalize e
    return $ VarExpr n e'
normalize (RefExpr n) = do
    s <- get
    case Map.lookup n (stScope s) of
        Just e -> normalize e
        Nothing -> fail $ "Reference to undeclared identifier: " ++ n        
normalize u@(UnboundExpr n) = return u
normalize (ApplyExpr e1 e2) = do
    e1' <- normalize e1
    e2' <- normalize e2
    apply e1' e2'
normalize (LambdaExpr n e) = do
    e' <- normalize e
    return $ LambdaExpr n e'
apply :: Expr -> Expr -> SimplifyM Expr
apply (LambdaExpr n e1) e2 = bind n e2 e1
apply e _ = fail $ "Cannot apply:" ++ (show e)

bind :: Name -> Expr -> Expr -> SimplifyM Expr
bind n be (UnExpr op e) = do
    e' <- bind n be e
    return $ UnExpr op e'
bind n be (BinExpr op e1 e2) = do
    e1' <- bind n be e1
    e2' <- bind n be e2
    return $ BinExpr op e1' e2'
bind _ _ ce@(ConstExpr _) = return $ ce
bind n be (IfExpr i t e) = do
    i' <- bind n be i
    t' <- bind n be t
    e' <- bind n be e
    return $ IfExpr i' t' e'
bind n be (VarExpr vn e) = do
    e' <- bind n be e
    return $ VarExpr vn e'
bind n be re@(RefExpr en) = return $ re
bind n be ue@(UnboundExpr n') = return $ if n == n' then be else ue
bind n be ae@(ApplyExpr _ _) = fail $ "Apply not allowed when binding: " ++ (show ae)
bind n be le@(LambdaExpr n' e) = do
    e' <- bind n be e
    return $ if n == n' then le else LambdaExpr n' e'
