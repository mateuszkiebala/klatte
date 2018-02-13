module SkelKlatte where

-- Haskell module generated by the BNF converter

import AbsKlatte
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs  -> failure x


transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type' id args block  -> failure x
  VarDef type' items  -> failure x


transArg :: Arg -> Result
transArg x = case x of
  ValArg type' id  -> failure x
  VarArg type' id  -> failure x
  FunArg funtype id  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Block stmts  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  Empty  -> failure x
  BStmt block  -> failure x
  Decl type' items  -> failure x
  Ass id expr  -> failure x
  Ret expr  -> failure x
  VRet  -> failure x
  Cond expr stmt  -> failure x
  CondElse expr stmt0 stmt  -> failure x
  While expr stmt  -> failure x
  ForTo type' id expr0 expr block  -> failure x
  ForDownTo type' id expr0 expr block  -> failure x
  Print expr  -> failure x
  SExp expr  -> failure x


transItem :: Item -> Result
transItem x = case x of
  Init id expr  -> failure x


transType :: Type -> Result
transType x = case x of
  Int  -> failure x
  Bool  -> failure x
  Void  -> failure x


transFunPartType :: FunPartType -> Result
transFunPartType x = case x of
  ValType type'  -> failure x
  VarType type'  -> failure x
  FType funtype  -> failure x


transFunType :: FunType -> Result
transFunType x = case x of
  Fun type' funparttypes  -> failure x


transGeneralExpr :: GeneralExpr -> Result
transGeneralExpr x = case x of
  LExpr lamexpr  -> failure x
  NExpr expr  -> failure x


transLamExpr :: LamExpr -> Result
transLamExpr x = case x of
  ELam type' args block  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  EVar id  -> failure x
  ELitInt n  -> failure x
  ELitTrue  -> failure x
  ELitFalse  -> failure x
  EApp id generalexprs  -> failure x
  LApp lamexpr generalexprs  -> failure x
  Neg expr  -> failure x
  Not expr  -> failure x
  EMul expr0 mulop expr  -> failure x
  EAdd expr0 addop expr  -> failure x
  ERel expr0 relop expr  -> failure x
  EAnd expr0 expr  -> failure x
  EOr expr0 expr  -> failure x


transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus  -> failure x
  Minus  -> failure x


transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times  -> failure x
  Div  -> failure x
  Mod  -> failure x


transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH  -> failure x
  LE  -> failure x
  GTH  -> failure x
  GE  -> failure x
  EQU  -> failure x
  NE  -> failure x


