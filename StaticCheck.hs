module StaticCheck where

import Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Data.Maybe

import Absgrammar

type Var = String
data Fun = FUN Type [Type] --typ zwracany przez funkcję i liczba argumentow
data Type = VOID | INT | BOOLEAN | ARRAY Type Int deriving (Eq, Show)

type Env = (M.Map Var Type, M.Map Var Fun, Type) --srodowisko zmiennych, funkcji, oczekiwany typ returna

emptyEnv :: Env
emptyEnv = (M.empty, M.empty, VOID)

type Semantics = ReaderT Env (ExceptT String Identity)

--STATEMENT
--compound_stm
stmt :: Stm -> Semantics Bool
stmt (CompS ScompOne) = return False

stmt (CompS (ScompTwo stms)) = stmts stms

stmt (CompS (ScompFour dec stms)) = do
	env' <- decls dec
	local (const env') (stmts stms)

----Exp
stmt (ExprS expr) = do
	eval expr
	return False
--
----selection_stm
stmt (SelS (SselOne expr stm)) = do
	stmtCond expr
	stmt stm
	return False

stmt (SelS (SselTwo expr stm1 stm2)) = do
	stmtCond expr
	b1 <- stmt stm1
	b2 <- stmt stm2
	return $ b1 && b2

----iter_stm
stmt while@(IterS (SiterOne expr stm)) = do
	stmtCond expr
	stmt stm
	return False

stmt (IterS (SiterFour exp1 exp2 exp3 stm)) = do
	eval exp1
	stmtForLoop exp2 exp3 stm
	return False

--return
stmt (JumpS expr) = do
	(_,_,t) <- ask
	t2 <- eval expr
	if (t == VOID) then
		throwError $ "return statement unexpected here"
	else if t == t2 then
		return True
	else
		throwError $ "return statement has type " ++ (show t2) ++ ", but expected " ++ (show t)

stmt (PrintS expr) = do
	x <- eval expr
	return False

stmtForLoop :: Exp -> Exp -> Stm -> Semantics (Maybe Type)
stmtForLoop bexp iexp stm = stmtCond bexp

stmts :: [Stm] -> Semantics Bool
stmts (s:[]) = stmt s
stmts (s:ss) = do
	wasReturn <- stmt s
	if wasReturn then do
		stmts ss
		return True
	else
		stmts ss

evalStmt :: Stm -> Maybe String
evalStmt stm =
	case
	runExceptT (runReaderT (stmt stm) emptyEnv) of
		(Identity (Left x)) -> Just x
		(Identity (Right x)) -> Nothing

stmtCond :: Exp -> Semantics (Maybe Type)
stmtCond exp = do
	t <- eval exp
	case t of
		BOOLEAN -> return Nothing
		_ -> conditionMustBeBoolean

--DECLARATIONS

decl :: Dec -> Semantics Env
--zwykła deklaracja zmiennej
decl (Variable (OnlyDecl Tint (Ident var))) = do
	(envv, envf, envt) <- ask
	return (M.insert var INT envv, envf, envt)

decl (Variable (OnlyDecl Tbool (Ident var))) = do
	(envv, envf, envt) <- ask
	return (M.insert var BOOLEAN envv, envf, envt)

decl (Variable (OnlyDecl Tvoid (Ident var))) = variableCantBeVoid var

----deklaracja z inicjalizacją
decl (Variable (InitDecl t (Ident var) expr)) = do
	(envv, envf, envt) <- ask
	t2 <- eval expr
	if convertType t == t2 then return (M.insert var t2 envv, envf, envt) else assignError t t2

decl (Variable (ArrayDecl t dims (Ident var))) = do
	(envv, envf, envt) <- ask
	case convertType t of
		VOID -> variableCantBeVoid var
		t2 -> return (M.insert var ((ARRAY t2) $ length dims) envv, envf, envt)

decl (Function (FunNoParams Tvoid (Ident x) stm)) = do
	(envv, envf, envt) <- ask
	properReturn <- local (const (envv, M.insert x (FUN VOID []) envf, envt)) (stmt (CompS stm))
	case properReturn of
		True -> throwError $ "void function " ++ x ++ "should not return anything"
		False -> return (envv, M.insert x (FUN VOID []) envf, VOID)

decl (Function (FunNoParams t (Ident x) stm)) = do
	(envv, envf, envt) <- ask
	let tc = convertType t
	properReturn <- local (const (envv, M.insert x (FUN tc []) envf, tc)) (stmt (CompS stm))
	case properReturn of
		False -> throwError $ "missing return statement in function " ++ x
		True -> return (envv, M.insert x (FUN tc []) envf, envt)

decl (Function (FunNoParamsArray Tvoid a (Ident x) stm)) =
	throwError $ "function " ++ x ++ "cant have array void return type"

decl (Function (FunNoParamsArray t a (Ident x) stm)) = do
	(envv, envf, envt) <- ask
	let arrt = (ARRAY (convertType t) (length a))
	properReturn <- local (const (envv, M.insert x (FUN arrt []) envf, arrt)) (stmt (CompS stm))
	case properReturn of
		False -> throwError $ "missing return statement in function " ++ x
		True -> return (envv, M.insert x (FUN arrt []) envf, envt)

decl (Function (FunWithParams Tvoid (Ident x) params stm)) = do
	(envv, envf, envt) <- ask
	ps <- Prelude.mapM convertParam params
	(envv', envf', _) <- local (const (envv, envf, envt)) (decls_ ps)
	let f = FUN VOID (Prelude.map fst ps)
	properReturn <- local (const (envv', M.insert x f envf', VOID)) (stmt (CompS stm))
	case properReturn of
		False -> return (envv, M.insert x f envf, envt)
		--True -> throwError $ "void function " ++ x ++ "should not return anything"

decl (Function (FunWithParams t (Ident x) params stm)) = do
	(envv, envf, envt) <- ask
	ps <- Prelude.mapM convertParam params
	(envv', envf', _) <- local (const (envv, envf, envt)) (decls_ ps)
	let 
		tc = (convertType t)
		f = (FUN tc (Prelude.map fst ps))
	properReturn <- local (const (envv', M.insert x f envf', tc)) (stmt (CompS stm))
	case properReturn of
		False -> throwError $ "missing return statement in function " ++ x
		True -> return (envv, M.insert x f envf, envt)

decl (Function (FunWithParamsArray Tvoid a (Ident x) params stm)) =
	throwError $ "function " ++ x ++ "cant have array void return type"

decl (Function (FunWithParamsArray t a (Ident x) params stm)) = do
	(envv, envf, envt) <- ask
	ps <- Prelude.mapM convertParam params
	let 
		tc = (ARRAY (convertType t) (length a))
		f = (FUN tc (Prelude.map fst ps))
	(envv', envf', _) <- local (const (envv, envf, tc)) (decls_ ps)
	properReturn <- local (const (envv', M.insert x f envf', tc)) (stmt (CompS stm))
	case properReturn of
		False -> throwError $ "missing return statement in function " ++ x
		True -> return (envv, M.insert x f envf, envt)

decl_ :: Type -> Var -> Semantics Env
decl_ t var = do
	(envv, envf, envt) <- ask
	return (M.insert var t envv, envf, envt)

decls_ :: [(Type, Var)] -> Semantics Env
decls_ [] = ask
decls_ ((t, var):ds) = do
	env <- decl_ t var
	local (const env) (decls_ ds)

decls :: [Dec] -> Semantics Env
decls [] = ask
decls (d:ds) = do
	env' <- decl d
	local (const env') (decls ds)

convertParam :: Parameter -> Semantics (Type, Var)
convertParam (Param t (Ident var)) =
	case convertType t of
		VOID -> variableCantBeVoid var
		t2 -> return (t2, var)

convertParam (ParamArray t as (Ident var)) =
	case convertType t of
		VOID -> variableCantBeVoid var
		t2 -> return $ (ARRAY t2 (length as), var)

--EXPRESSIONS

eval :: Exp -> Semantics Type
eval (Econst (Eint c)) = return INT
eval (Econst (Eboolean c)) = return BOOLEAN

eval (Evar (Ident var)) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just t -> return t
		Nothing -> variableUndeclared var

eval (Earray (Ident var) ads) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just (ARRAY t ds) ->
			let
				lads = length ads
				l = lads - ds
			in
				if l > 0 then
					return $ ARRAY t l
				else if l == 0 then
					return t
				else
					improperDimension lads ds var
		Nothing -> variableUndeclared var
		_ -> notArray var

eval (Eassign (Evar (Ident var)) Assign exp) = do --zgadzamy sie na przypisywanie tylko do zmiennych
	(env,_,_) <- ask
	case M.lookup var env of
		Just t -> do
			t2 <- eval exp
			if t == t2 then return t else assignError t t2
		_ -> variableUndeclared var

eval (Eassign (Evar (Ident var)) op exp2) = do --zgadzamy sie na przypisywanie tylko do zmiennych
	(env,_,_) <- ask
	case M.lookup var env of
		Just t -> do
			t2 <- eval exp2
			if t == t2 then return t else assignError t t2
		_ -> variableUndeclared var

eval (Eassign (Earray (Ident var) ads) Assign exp) = do
	(env,_,_) <- ask
    	case M.lookup var env of
    		Just (ARRAY t a) -> do
    			t2 <- eval exp
    			let
    			    l = length ads
    			    lads = a - l
    			if (lads < 0) then improperDimension a l var
    			else if (ARRAY t lads) == t2 || lads == 0 then return t2
    			else assignError t2 (ARRAY t lads)
    		Just x -> do
    		    throwError $ var ++ " is not an array "
    		_ -> variableUndeclared var

eval (Eassign v o e) = throwError $ "Can't assign expression to expression"

eval (Epreinc (Evar (Ident var))) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just INT -> return INT
		Just x -> throwError $ "cant do preincrementation on " ++ (show x)
		_ -> variableUndeclared var

eval (Epredec (Evar (Ident var))) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just INT -> return INT
		Just x -> throwError $ "cant do predecrementation on " ++ (show x)
		_ -> variableUndeclared var

eval (Epreop op expr) = do
	t <- eval expr
	case (t, op) of
		(INT, Negative) -> return INT
		(BOOLEAN, Logicalneg) -> return BOOLEAN
		_ -> throwError $ "cant use " ++ (show op) ++ " operator on " ++ (show t)

eval (Efunk (Evar (Ident var))) = do
	(_,env,_) <- ask
	case M.lookup var env of
		Just (FUN t []) -> return t
		Just (FUN t x) -> throwError $ "lack of parameters in invocation of function " ++ var
		_ -> functionUndeclared var

eval (Efunkpar (Evar (Ident var)) exps) = do
	(_,env,_) <- ask
	case M.lookup var env of
		Just (FUN t []) -> throwError $ "function " ++ var ++ " takes no parameters"
		Just (FUN t ps) -> do
				vals <- Prelude.mapM eval exps
				if (length vals) /= (length ps) then
					throwError $ "improper number of parameters in invocation of function " ++ var
				else do
					x <- zipWithM zipper ps vals
					return t
		_ -> functionUndeclared var
	where
		zipper :: Type -> Type -> Semantics (Type, Type)
		zipper x y = if x == y then return (x, y)
		else impTypes var x y

eval (Epostinc (Evar (Ident var))) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just INT -> return INT
		Just x -> throwError $ "cant do postincrementation on " ++ (show x)
		_ -> variableUndeclared var

eval (Epostdec (Evar (Ident var))) = do
	(env,_,_) <- ask
	case M.lookup var env of
		Just INT -> return INT
		Just x -> throwError $ "cant do postdecrementation on " ++ (show x)
		_ -> variableUndeclared var


----intowe
eval (Eplus exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return INT
		(t1, t2) -> throwError $ "cant perform addition on " ++ (show t1) ++ " and " ++ (show t2)

eval (Eminus exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return INT
		(t1, t2) -> throwError $ "cant perform subtraction on " ++ (show t1) ++ " and " ++ (show t2)

eval (Etimes exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return INT
		(t1, t2) -> throwError $ "cant perform multiplication on " ++ (show t1) ++ " and " ++ (show t2)

eval (Ediv exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return INT
		(t1, t2) -> throwError $ "cant perform division on " ++ (show t1) ++ " and " ++ (show t2)

eval (Emod exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return INT
		(t1, t2) -> throwError $ "cant perform modulo on " ++ (show t1) ++ " and " ++ (show t2)

----boolowe
eval (Elor exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(BOOLEAN, BOOLEAN) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform logical or on " ++ (show t1) ++ " and " ++ (show t2)

eval (Eland exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(BOOLEAN, BOOLEAN) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform logical and on " ++ (show t1) ++ " and " ++ (show t2)

eval (Eeq exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(BOOLEAN, BOOLEAN) -> return BOOLEAN
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform equals on " ++ (show t1) ++ " and " ++ (show t2)

eval (Eneq exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(BOOLEAN, BOOLEAN) -> return BOOLEAN
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform not equals on " ++ (show t1) ++ " and " ++ (show t2)

eval (Elthen exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform less then on " ++ (show t1) ++ " and " ++ (show t2)

eval (Egrthen exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform greater then on " ++ (show t1) ++ " and " ++ (show t2)

eval (Ele exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform less then or equals on " ++ (show t1) ++ " and " ++ (show t2)

eval (Ege exp1 exp2) = do
	val1 <- eval exp1
	val2 <- eval exp2
	case (val1, val2) of
		(INT, INT) -> return BOOLEAN
		(t1, t2) -> throwError $ "cant perform greater then or equals on " ++ (show t1) ++ " and " ++ (show t2)

staticCheck :: Program -> Maybe String
staticCheck (Progr decs stms) = evalStmt $ CompS (ScompFour decs stms)
 
--pomocnicze
convertType Tbool = BOOLEAN
convertType Tint = INT
convertType Tvoid = VOID

impTypes var x y = throwError $ "improper types in invocation of function " ++ var ++ " expected " ++ (show x) ++ " but is " ++ (show y)
variableCantBeVoid var = throwError $ "variable " ++ var ++ " cant be void type"
conditionMustBeBoolean = throwError $ "condition in statement must be boolean expression!"
assignError t t2 = throwError $ "cannot assign " ++ (show t) ++ " to " ++ (show t2)
notArray var = throwError $ var ++ " is not array"
variableUndeclared var = throwError $ "Variable undeclared: " ++ var
functionUndeclared var = throwError $ "Function undeclared: " ++ var
improperDimension real req var = throwError $ "Improper dimension size in array " ++ var ++ " real - " ++
	(show real) ++ ", requested " ++ (show req)

