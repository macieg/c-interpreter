module Interpreter where

import Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Absgrammar

type Var = String
data Fun = A ([VarVal] -> Semantics (Maybe VarVal))

type Loc = Int

data Array = LOCLIST [Loc] | ARRAYLIST [Array]
data LocType = LOC Loc | ARRAY Array

data VarVal = INT Int | ARRAYVAL Array

type Env = (M.Map Var LocType, M.Map Var Fun)
type St = M.Map Loc Int

emptyEnv :: Env
emptyEnv = (M.empty, M.empty)

--na zerze zapisany numer kolejnej wolnej lokacji
initialSt :: St
initialSt = M.singleton 0 1

type Semantics = ReaderT Env (StateT St (ExceptT String IO))

--STATEMENT
--compound_stm
stmt :: Stm -> Semantics (Maybe VarVal)
stmt (CompS ScompOne) = return Nothing

stmt (CompS (ScompTwo stms)) = stmts stms

stmt (CompS (ScompFour dec stms)) = do
	env' <- decls dec
	local (const env') (stmts stms)

--Exp
stmt (ExprS expr) = do
	eval expr
	return Nothing

--selection_stm
stmt (SelS (SselOne expr stm)) = do
	Just (INT val) <- eval expr
	if val == 1 then stmt stm else return Nothing

stmt (SelS (SselTwo expr stm1 stm2)) = do
	Just (INT val) <- eval expr
	if val == 1 then stmt stm1 else stmt stm2

--iter_stm
stmt while@(IterS (SiterOne expr stm)) = do
	Just (INT val) <- eval expr
	if val == 1 then do
		stmt stm
		stmt while 
	else return Nothing

stmt (IterS (SiterFour exp1 exp2 exp3 stm)) = do
	eval exp1
	stmtForLoop exp2 exp3 stm

--return
stmt (JumpS expr) = eval expr

--print
stmt (PrintS expr) = do
	Just (INT val) <- eval expr
	liftIO $ print val
	return Nothing

stmtForLoop :: Exp -> Exp -> Stm -> Semantics (Maybe VarVal)
stmtForLoop bexp iexp stm = do
	Just (INT cond) <- eval bexp
	if cond == 1 then do
		stmt stm
		eval iexp
		stmtForLoop bexp iexp stm
	else return Nothing

stmts :: [Stm] -> Semantics (Maybe VarVal)
stmts [] = return Nothing
stmts (s:ss) = do
	mval <- stmt s
	case mval of
		Just val -> return $ Just val --byl return
		Nothing -> stmts ss

evalStmt :: Stm -> IO ()
evalStmt stm = do
	finalState <- runExceptT (execStateT(runReaderT (stmt stm) emptyEnv) initialSt)
	case finalState of
		Right a -> return ()--TODO tutaj mozemy wypisac stan print a, albo return ()
		Left b -> print b

--DECLARATIONS

decl :: Dec -> Semantics Env
--zwykła deklaracja zmiennej
decl (Variable (OnlyDecl t (Ident var))) = do
	Just newLoc <- gets (M.lookup 0) 
	modify (M.insert newLoc 0) --domyslnie wartosc 0
	modify (M.insert 0 (newLoc+1))
	(envv, envf) <- ask	
	return (M.insert var (LOC newLoc) envv, envf)

--deklaracja z inicjalizacją
decl (Variable (InitDecl t (Ident var) expr)) = do
	Just newLoc <- gets (M.lookup 0)
	mval <- eval expr
	case mval of
		Just (INT val) -> do
			modify (M.insert newLoc val)
			modify (M.insert 0 (newLoc+1))
			(envv, envf) <- ask
			return (M.insert var (LOC newLoc) envv, envf)
		_ -> throwError $ "Cannot initialize variable " ++ var ++ " with Nothing"

decl (Variable (ArrayDecl t dims (Ident var))) = do
	(envv, envf) <- ask	
	let
		f :: Int -> Semantics Loc
		f b = do
			Just newLoc <- gets (M.lookup 0)
			modify (M.insert newLoc 0) --tablice wypelniamy zerami
			modify (M.insert 0 (newLoc+1))
			return newLoc
		declOneDimArray (ArrayDec exp) = do
			Just (INT n) <- eval exp
			mapM f [1..n]
		dfs :: [ArrayDim] -> Semantics Array
		dfs (d:[]) = do
			locs <- declOneDimArray d
			return $ LOCLIST locs
		dfs ((ArrayDec exp):ds) = do
			Just (INT n) <- eval exp
			ls <- (mapM (\x -> dfs ds) [1..n])
			return $ ARRAYLIST ls
	array <- dfs dims
	return (M.insert var (ARRAY array) envv, envf)

decl (Function (FunNoParams t (Ident x) stm)) = declf x stm
decl (Function (FunNoParamsArray t a (Ident x) stm)) = declf x stm
decl (Function (FunWithParams t (Ident x) params stm)) = declfp x params stm
decl (Function (FunWithParamsArray t a (Ident x) params stm)) = declfp x params stm
--decl x = error $ show x

declfp x params stm = do
	(envv, envf) <- ask
	let
		f paramVals = do
			(envv', envf') <- local (const (envv, envf)) (decls_ (getDecs paramVals))
			local (const (envv', M.insert x (A f) envf')) (stmt (CompS stm))
	return (envv, M.insert x (A f) envf)
	where
		getDecs parVals = zipWith zipper parVals params
		zipper x (Param t (Ident y)) = (x, y)
		zipper x (ParamArray t a (Ident y)) = (x, y)

declf x stm = do
	(envv, envf) <- ask
	let 
		f [] = local (const (envv, M.insert x (A f) envf)) (stmt (CompS stm))
	return (envv, M.insert x (A f) envf)

decl_ :: VarVal -> Var -> Semantics Env --nie ma znaczenia jaki typ
decl_ (INT val) var = decl (Variable (InitDecl Tint (Ident var) (Econst (Eint $ toInteger val))))
decl_ (ARRAYVAL val) var = do
	(envv, envf) <- ask
	return (M.insert var (ARRAY val) envv, envf)

decls_ :: [(VarVal, Var)] -> Semantics Env
decls_ [] = ask
decls_ ((varVal, var):ds) = do
	env <- decl_ varVal var
	local (const env) (decls_ ds)

decls :: [Dec] -> Semantics Env 
decls [] = ask
decls (d:ds) = do
	env' <- decl d
	local (const env') (decls ds)
	
--evalDecl :: [Dec] -> Env
--evalDecl ds = 
--	let sem = decls ds in
--	let state = runReaderT sem (empty, empty) in
--	execStateT state initialSt

--EXPRESSIONS

eval :: Exp -> Semantics (Maybe VarVal)
eval (Econst (Eint c)) = return $ Just $ INT $ fromInteger c
eval (Econst (Eboolean Etrue)) = return $ Just $ INT 1
eval (Econst (Eboolean Efalse)) = return $ Just $ INT 0

eval (Evar (Ident var)) = do
	(env,_) <- ask
	case M.lookup var env of 
		Just (LOC loc) -> do
			Just val <- gets (M.lookup loc)
			return $ Just $ INT val
		Just (ARRAY a) -> return $ Just $ ARRAYVAL a

eval (Earray (Ident var) ads) = do
	(env,_) <- ask
	let
		f (ArrayDec exp) = do 
			Just (INT n) <- eval exp
			return n
	case M.lookup var env of
		Just (ARRAY a) -> do
			indexes <- (mapM f ads)
			case getVal indexes a of
				LOC l -> do
					Just i <- gets $ M.lookup l
					return $ Just $ INT i
				ARRAY a -> return $ Just $ ARRAYVAL a

eval (Eassign (Evar (Ident var)) Assign exp) = do --zgadzamy sie na przypisywanie tylko do zmiennych
	(env,_) <- ask
	case M.lookup var env of 
		Just (LOC loc) -> do
			Just (INT newVal) <- eval exp
			modify (M.insert loc newVal)
			return $ Just $ INT newVal
		Just (ARRAY a) -> do
			Just (ARRAYVAL a2) <- eval exp
			v2 <- mapM (\x -> do val <- gets (M.lookup x); return val) (getLocList a2)
			mapM (\(x,Just y) -> modify (M.insert x y)) (zipWith (\x y -> (x,y)) (getLocList a) v2)
			return $ Just $ ARRAYVAL a

eval (Eassign (Evar (Ident var)) op exp2) = do --zgadzamy sie na przypisywanie tylko do zmiennych
	(env,_) <- ask
	case M.lookup var env of
		Just (LOC loc) -> do
			Just oldVal <- gets (M.lookup loc)
			Just (INT rval) <- eval exp2
			let newVal = (assignValue oldVal rval op)
			modify (M.insert loc newVal) 
			return $ Just $ INT newVal

eval (Eassign (Earray (Ident var) ads) Assign exp) = do
	(envv,envf) <- ask
	let
		f (ArrayDec exp) = do
			Just (INT n) <- eval exp
			return n
	case M.lookup var envv of
		Just (ARRAY a) -> do
			indexes <- (mapM f ads)
			case getVal indexes a of
				LOC l -> do
					Just (INT val) <- eval exp
					modify (M.insert l val)
					return $ Just $ INT val
				ARRAY a -> do
					Just (ARRAYVAL a2) <- eval exp
					v2 <- mapM (\x -> do val <- gets (M.lookup x); return val) (getLocList a2)
					mapM (\(x,Just y) -> modify (M.insert x y)) (zipWith (\x y -> (x,y)) (getLocList a) v2)
					return $ Just $ ARRAYVAL a

eval (Eassign v o e) = throwError $ "Can't assign expression to expression"

eval (Epreinc (Evar (Ident var))) = do
	(env,_) <- ask
	case M.lookup var env of
		Just (LOC loc) -> do
			Just val <- gets (M.lookup loc)
			let newVal = val + 1
			modify (M.insert loc newVal)
			return $ Just $ INT newVal

eval (Epredec (Evar (Ident var))) = do
	(env,_) <- ask
	case M.lookup var env of
		Just (LOC loc) -> do
			Just val <- gets (M.lookup loc)
			let newVal = val - 1
			modify (M.insert loc newVal)
			return $ Just $ INT newVal

eval (Epreop op expr) = do
	Just (INT val) <- eval expr
	case op of
		Negative -> return $ Just $ INT (-val)
		Logicalneg -> if val == 0 then return $ Just $ INT 1 else return $ Just $ INT 0

eval (Efunk (Evar (Ident var))) = do
	(_,env) <- ask
	case M.lookup var env of
		Just (A fun) -> fun []

eval (Efunkpar (Evar (Ident var)) exps) = do
	(_,env) <- ask
	case M.lookup var env of
		Just (A fun) -> do
				vals <- Prelude.mapM eval exps
				fun (Prelude.map fromJust vals)
		
eval (Epostinc (Evar (Ident var))) = do
	(env,_) <- ask
	case M.lookup var env of 
		Just (LOC loc) -> do
			Just val <- gets (M.lookup loc)
			modify (M.insert loc (val + 1))
			return $ Just $ INT val

eval (Epostdec (Evar (Ident var))) = do
	(env,_) <- ask
	case M.lookup var env of
		Just (LOC loc) -> do
			Just val <- gets (M.lookup loc)
			modify (M.insert loc (val + 1))
			return $ Just $ INT val
--intowe
eval (Eplus exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ Just $ INT (val1 + val2)

eval (Eminus exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ Just $ INT (val1 - val2)

eval (Etimes exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ Just $ INT (val1 * val2)

eval (Ediv exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	case val2 of
		0 -> throwError $ "Division by zero!"
		v2 -> return $ Just $ INT (val1 `div` v2)

eval (Emod exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ Just $ INT (val1 `mod` val2)

--boolowe
eval (Elor exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	return $ bti ((itb val1) || (itb val2)) 

eval (Eland exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	return $ bti ((itb val1) && (itb val2)) 

eval (Eeq exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 == val2

eval (Eneq exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 /= val2

eval (Elthen exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 < val2

eval (Egrthen exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 > val2

eval (Ele exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 <= val2

eval (Ege exp1 exp2) = do
	Just (INT val1) <- eval exp1
	Just (INT val2) <- eval exp2
	return $ bti $ val1 >= val2

evalExp :: Exp -> IO ()
evalExp expr = do 
		Right (Just (INT val)) <- runExceptT (evalStateT (runReaderT (eval expr) emptyEnv) initialSt)
		print val

runProgram :: Program -> IO ()
runProgram (Progr decs stms) = evalStmt $ CompS (ScompFour decs stms)
 
--pomocnicze

assignValue :: Int -> Int -> Assignment_op -> Int
assignValue val1 val2 AssignMul = val1 * val2;
assignValue val1 val2 AssignDiv = val1 `div` val2;
assignValue val1 val2 AssignMod = val1 `mod` val2;
assignValue val1 val2 AssignAdd = val1 + val2;
assignValue val1 val2 AssignSub = val1 - val2;

getVal :: [Int] -> Array -> LocType
getVal (i:is) (LOCLIST l) = LOC $ l !! i
getVal (i:[]) (ARRAYLIST a) = ARRAY $ a !! i
getVal (i:is) (ARRAYLIST a) = getVal is (a !! i)

getLocList :: Array -> [Loc]
getLocList (LOCLIST ls) = ls
getLocList (ARRAYLIST as) = Prelude.foldr (\a b -> (getLocList a) ++ b) [] as


toBoolean :: Bool -> Constant
toBoolean a = if a then Eboolean Etrue else Eboolean Efalse

toBool :: Boolean -> Bool
toBool a = if a == Etrue then True else False

itb :: Maybe VarVal -> Bool
itb (Just (INT a)) = if a == 1 then True else False

bti :: Bool -> Maybe VarVal
bti a = if a == True then Just $ INT 1 else Just $ INT 0
