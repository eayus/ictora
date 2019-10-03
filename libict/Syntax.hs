module Syntax where

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
    | AppendArg
    | EnterClosure
    | PopAndAdd
    deriving Show



comp :: Expr -> [Instruction]
comp (Lit val t) = [PushVal val]
comp (Var name (TInt)) = [PushVar name]
comp (Var name (TFunc _ _)) = [PushClosure (Closure name [])]
comp (Add e1 e2 _) = comp e1 ++ comp e2 ++ [PopAndAdd]
comp (App e1 e2 _) = comp e1 ++ comp e2 ++ [AppendArg] ++ case typeofExpr e1 of
    (TFunc _ TInt) -> [EnterClosure]
    _ -> []


-- apply (add 2) 3
testProgram :: Expr
testProgram = App (App (Var "apply" (TFunc (TFunc TInt TInt) (TFunc TInt TInt))) (App (Var "add" (TFunc TInt (TFunc TInt TInt))) (Lit 2 TInt) (TFunc TInt TInt)) (TFunc TInt TInt)) (Lit 3 TInt) TInt

{--
PushClosure (Closure "apply" [])
PushClosure (Closure "add" [])
PushVal 2
AppendArg
AppendArg
PushVal 3
AppendArg
EnterClosure--}
















































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
