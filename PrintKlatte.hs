{-# OPTIONS -fno-warn-incomplete-patterns #-}
module PrintKlatte where

-- pretty-printer generated by the BNF converter

import AbsKlatte
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
   Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])


instance Print TopDef where
  prt i e = case e of
   FnDef type' id args block -> prPrec i 0 (concatD [prt 0 type' , prt 0 id , doc (showString "(") , prt 0 args , doc (showString ")") , prt 0 block])
   VarDef type' items -> prPrec i 0 (concatD [prt 0 type' , prt 0 items , doc (showString ";")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Arg where
  prt i e = case e of
   ValArg type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])
   VarArg type' id -> prPrec i 0 (concatD [prt 0 type' , doc (showString "&") , prt 0 id])
   FunArg funtype id -> prPrec i 0 (concatD [prt 0 funtype , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Block where
  prt i e = case e of
   Block stmts -> prPrec i 0 (concatD [doc (showString "{") , prt 0 stmts , doc (showString "}")])


instance Print Stmt where
  prt i e = case e of
   Empty  -> prPrec i 0 (concatD [doc (showString ";")])
   BStmt block -> prPrec i 0 (concatD [prt 0 block])
   Decl type' items -> prPrec i 0 (concatD [prt 0 type' , prt 0 items , doc (showString ";")])
   Ass id expr -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 expr , doc (showString ";")])
   Ret expr -> prPrec i 0 (concatD [doc (showString "return") , prt 0 expr , doc (showString ";")])
   VRet  -> prPrec i 0 (concatD [doc (showString "return") , doc (showString ";")])
   Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt])
   CondElse expr stmt0 stmt -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt0 , doc (showString "else") , prt 0 stmt])
   While expr stmt -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 stmt])
   ForTo type' id expr0 expr block -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString "=") , prt 0 expr0 , doc (showString "to") , prt 0 expr , doc (showString "do") , prt 0 block])
   ForDownTo type' id expr0 expr block -> prPrec i 0 (concatD [doc (showString "for") , prt 0 type' , prt 0 id , doc (showString "=") , prt 0 expr0 , doc (showString "downto") , prt 0 expr , doc (showString "do") , prt 0 block])
   Print expr -> prPrec i 0 (concatD [doc (showString "print") , prt 0 expr , doc (showString ";")])
   SExp expr -> prPrec i 0 (concatD [prt 0 expr , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Item where
  prt i e = case e of
   Init id expr -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 expr])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Type where
  prt i e = case e of
   Int  -> prPrec i 0 (concatD [doc (showString "int")])
   Bool  -> prPrec i 0 (concatD [doc (showString "bool")])
   Void  -> prPrec i 0 (concatD [doc (showString "void")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print FunPartType where
  prt i e = case e of
   ValType type' -> prPrec i 0 (concatD [prt 0 type'])
   VarType type' -> prPrec i 0 (concatD [prt 0 type' , doc (showString "&")])
   FType funtype -> prPrec i 0 (concatD [prt 0 funtype])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print FunType where
  prt i e = case e of
   Fun type' funparttypes -> prPrec i 0 (concatD [prt 0 type' , doc (showString "(") , prt 0 funparttypes , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print GeneralExpr where
  prt i e = case e of
   LExpr lamexpr -> prPrec i 0 (concatD [prt 0 lamexpr])
   NExpr expr -> prPrec i 0 (concatD [prt 0 expr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print LamExpr where
  prt i e = case e of
   ELam type' args block -> prPrec i 0 (concatD [prt 0 type' , doc (showString "(") , prt 0 args , doc (showString ")") , prt 0 block])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Expr where
  prt i e = case e of
   EVar id -> prPrec i 6 (concatD [prt 0 id])
   ELitInt n -> prPrec i 6 (concatD [prt 0 n])
   ELitTrue  -> prPrec i 6 (concatD [doc (showString "true")])
   ELitFalse  -> prPrec i 6 (concatD [doc (showString "false")])
   EApp id generalexprs -> prPrec i 6 (concatD [prt 0 id , doc (showString "(") , prt 0 generalexprs , doc (showString ")")])
   LApp lamexpr generalexprs -> prPrec i 6 (concatD [prt 0 lamexpr , doc (showString "(") , prt 0 generalexprs , doc (showString ")")])
   Neg expr -> prPrec i 5 (concatD [doc (showString "-") , prt 6 expr])
   Not expr -> prPrec i 5 (concatD [doc (showString "!") , prt 6 expr])
   EMul expr0 mulop expr -> prPrec i 4 (concatD [prt 4 expr0 , prt 0 mulop , prt 5 expr])
   EAdd expr0 addop expr -> prPrec i 3 (concatD [prt 3 expr0 , prt 0 addop , prt 4 expr])
   ERel expr0 relop expr -> prPrec i 2 (concatD [prt 2 expr0 , prt 0 relop , prt 3 expr])
   EAnd expr0 expr -> prPrec i 1 (concatD [prt 2 expr0 , doc (showString "&&") , prt 1 expr])
   EOr expr0 expr -> prPrec i 0 (concatD [prt 1 expr0 , doc (showString "||") , prt 0 expr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print AddOp where
  prt i e = case e of
   Plus  -> prPrec i 0 (concatD [doc (showString "+")])
   Minus  -> prPrec i 0 (concatD [doc (showString "-")])


instance Print MulOp where
  prt i e = case e of
   Times  -> prPrec i 0 (concatD [doc (showString "*")])
   Div  -> prPrec i 0 (concatD [doc (showString "/")])
   Mod  -> prPrec i 0 (concatD [doc (showString "%")])


instance Print RelOp where
  prt i e = case e of
   LTH  -> prPrec i 0 (concatD [doc (showString "<")])
   LE  -> prPrec i 0 (concatD [doc (showString "<=")])
   GTH  -> prPrec i 0 (concatD [doc (showString ">")])
   GE  -> prPrec i 0 (concatD [doc (showString ">=")])
   EQU  -> prPrec i 0 (concatD [doc (showString "==")])
   NE  -> prPrec i 0 (concatD [doc (showString "!=")])



