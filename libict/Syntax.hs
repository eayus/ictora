module Syntax where

import Control.Monad.State.Lazy

type Identifier = String

data TType = TInt | TFunc TType TType
    deriving Show


{--data Program = [FuncDef]

data FuncDef = FuncDef [Identifier] Expr--}

data Expr
    = Var Identifier TType
    | Lit Int TType
    | Add Expr Expr TType
    | App Expr Expr TType
    deriving Show

typeofExpr :: Expr -> TType
typeofExpr (Var _ t) = t
typeofExpr (Lit _ t) = t
typeofExpr (Add _ _ t) = t
typeofExpr (App _ _ t) = t

data Atom
    = AtomVar Identifier
    | AtomVal Int
    | AtomClosure Closure
    deriving Show

data Closure = Closure Identifier [Atom]
    deriving Show

data Instruction
    = PushVal Int
    | PushVar Identifier
    | PushClosure Closure
    | AppendInt
    | AppendClosure
    | EnterClosure
    | PopAndAdd
    deriving Show


comp :: Expr -> [Instruction]
comp (Lit val t) = [PushVal val]
comp (Var name (TInt)) = [PushVar name]
comp (Var name (TFunc _ _)) = [PushClosure (Closure name [])]
comp (Add e1 e2 _) = comp e1 ++ comp e2 ++ [PopAndAdd]
comp (App e1 e2 _) = comp e1 ++ comp e2 ++ [appendCommand e2] ++ case typeofExpr e1 of
    (TFunc _ TInt) -> [EnterClosure]
    _ -> []

appendCommand :: Expr -> Instruction
appendCommand e = case typeofExpr e of
    TInt -> AppendInt
    _ -> AppendClosure


-- apply (add 2) 3
testProgram :: Expr
testProgram = App (App (Var "apply" (TFunc (TFunc TInt TInt) (TFunc TInt TInt))) (App (Var "add" (TFunc TInt (TFunc TInt TInt))) (Lit 2 TInt) (TFunc TInt TInt)) (TFunc TInt TInt)) (Lit 3 TInt) TInt


type VarName = Int

newtype NameStore = NameStore VarName

fresh :: State NameStore VarName
fresh = do
    name <- lastVar
    put $ NameStore (name + 1)
    return (name + 1)

lastVar :: State NameStore VarName
lastVar = do
    (NameStore n) <- get
    return n


-- Expr -> Int -> (Int, [Instruction])
exprToAsm :: Expr -> State NameStore [Asm]

exprToAsm (Lit val _) = do
    tmpVar <- fresh
    return [StoreVal tmpVar val]

exprToAsm (Var name TInt) = do
    tmpVar <- fresh
    return [StoreVar tmpVar name]

exprToAsm (Var name (TFunc _ _)) = do
    tmpVar <- fresh
    return [StoreClosure tmpVar name]

exprToAsm (Add e1 e2 _) = do
    asm1 <- exprToAsm e1
    op1var <- lastVar
    asm2 <- exprToAsm e2
    op2var <- lastVar
    tmpVar <- fresh
    return $ asm1 ++ asm2 ++ [PrimAdd tmpVar op1var op2var]

exprToAsm (App e1 e2 typ) = do
    asm1 <- exprToAsm e1
    op1var <- lastVar
    asm2 <- exprToAsm e2
    op2var <- lastVar
    newClosure <- fresh
    case typ of
        TInt -> do
            res <- fresh
            return $ asm1 ++ asm2 ++ [appendAsm e2 newClosure op1var op2var, AsmEnterClosure res newClosure]
        (TFunc _ _) -> return $ asm1 ++ asm2 ++ [appendAsm e2 newClosure op1var op2var]

appendAsm :: Expr -> VarName -> VarName -> VarName -> Asm
appendAsm e = case typeofExpr e of
    TInt -> AsmAppendInt
    (TFunc _ _) -> AsmAppendClosure


{--
PushClosure (Closure "apply" [])
PushClosure (Closure "add" [])
PushVal 2
AppendArg
AppendArg
PushVal 3
AppendArg
EnterClosure--}


data Asm
    = StoreClosure Int String -- Location, FunctionName
    | StoreVal Int Int        -- Location, Value
    | StoreVar Int String     -- Location, Name
    | PrimAdd Int Int Int        -- Result, Arg0, Arg1
    | AsmAppendInt Int Int Int       -- Location, Closure, Arg
    | AsmAppendClosure Int Int Int       -- Location, Closure, Arg
    | AsmEnterClosure Int Int    -- Result Location, Closure to enter
    deriving Show


--type VarState = (Int, [Int]) -- Next varname, stack

{--toAsm :: [Instruction] -> VarState -> [Asm]
toAsm [] _ = []
toAsm (x:xs) vs = let (instr, newVs) = toAsm1 x vs in instr : (toAsm xs newVs)


toAsm1 :: Instruction -> VarState -> (Asm, VarState)
toAsm1 (PushClosure (Closure name args)) (n, stack) = (StoreClosure n name, (n + 1, n : stack))
toAsm1 (PushVal val) (n, stack) = (StoreVal n val, (n + 1, n : stack))
toAsm1 (PushVar name) (n, stack) = (StoreVar n name, (n + 1, n : stack))
toAsm1 PopAndAdd (n, (x : y : xs)) = (PrimAdd n x y, (n + 1, n : xs))
toAsm1 AppendInt (n, (x : y : stack)) = (AsmAppendInt y x, (n, y : stack))
toAsm1 AppendClosure (n, (x : y : stack)) = (AsmAppendClosure y x, (n, y : stack))
toAsm1 EnterClosure (n, x : stack) = (AsmEnterClosure x n, (n + 1, n : stack))
toAsm1 _ _ = error "bad program"--}

















































{--add y x = x + y

f y = map (add y) xs

Const [2], Function("add", y), Function("map"), App2


f x = x + 1
main = f 2


f:	Const 1, Access 1, Add, Ret

main:	Const(2), Closure("f"), App


f:
	push 1
	push arg[0]

	pop reg1
	pop reg2
	add reg1, reg2
	push reg1

	ret

main:
	push 2--}














{--import Util

data IctType = TInt | TFunc IctType IctType

data Expr
    = Var Identifier
    | Lit Int
    | App Expr Expr
    deriving Show

data FuncDef = FuncDef IctType [Identifier] Expr -- function type, params, body

type Program = [FuncDef]


numArgs :: IctType -> Int
numArgs TInt = 0
numArgs TFunc _ t2 = 1 + numArgs t2


rpProgram :: Program -> Program
rpProgram = concat . (map rpFuncDef)

rpFuncDef :: FuncDef -> [FuncDef]
rpFuncDef (FuncDef t params body) = --}



-- make function args (f = \x -> \y -> x + y      TO    f x y = x + y)
-- Lambda Lift
-- Remove Partial Applications (see below) - only occurs when a function is passed to another function (not true? f = add 2)

{--apply f x = f x
add x y = x + y
start = apply add 5 3

op x = match x
    True => add
    False => sub

lol1 = """

struct Thunk {
	func: isize,
	args: Box<[
}


fn start() -> i32 {
  	apply1((&add, 5), 3);
}

fn add(val: i32, val2: i32) -> i32 {
	val + val2
}

fn apply1(f: Thunk, val: i32) -> i32 {
	f.func(i32, i32)
}



struct Closure {
	fn* code;
	int num_args;
	void* args[];
};

*void apply(c: Closure, arg: *int) {
	push arg
	for i in num_args..0
		push c.args[i]

	jmp c.code	
}

*int 


"""--}
