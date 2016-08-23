module Continuation where

import Lib

repl () =
  do putStr "CPS> "
     input <- getLine
     case parseDecl input of
        Right x -> let result = cpsDecl x
                    in do putStrLn "Pretty Result: "
                          putStrLn $ toStr result
                          putStrLn "Details: "
                          putStrLn $ show result
                          putStrLn ""
                          repl ()
        Left x -> do putStrLn $ show x
                     repl ()

-- p1 -- factk

factk :: Integer -> (Integer -> a) -> a
factk 0 k = k 1
factk n k = factk (n-1) (\v -> k $ v * n)

-- p2 -- evenodd

evenoddk :: Integral r => [r] -> (r -> t) -> (r -> t) -> t
evenoddk [x] ke ko | even x    = ke x
                  | otherwise = ko x
evenoddk (x:xs) ke ko | even x    = evenoddk xs (\v -> ke $ v + x) ko
                     | otherwise = evenoddk xs ke (\v -> ko $ v + x)


cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) = Decl (f ++ "k")
                                    (params ++ ["k"])
                                    (fst (cpsExp body (VarExp "k") 1))

isSimple :: Exp -> Bool
isSimple (VarExp _) = True
isSimple (IntExp _) = True
isSimple (AppExp _ _) = False
isSimple (IfExp e1 e2 e3) = all isSimple [e1,e2,e3]
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple (LamExp _ _) = True

gensym :: Integer -> (String,Integer)
gensym i = ("v" ++ show i, i + 1)

cpsExp :: Exp -> Exp -> Integer -> (Exp,Integer)
cpsExp e@(IntExp _) k syms = (AppExp k e, syms)
cpsExp e@(VarExp _) k syms = (AppExp k e, syms)
cpsExp e@(IfExp e1 e2 e3) k syms | isSimple e1 =
         let (ce2,s2) = cpsExp e2 k syms
             (ce3,s3) = cpsExp e3 k s2
          in (IfExp e1 ce2 ce3, s3)
       | otherwise = let (v,s2) = gensym syms
                         (ce2,s3) = cpsExp e2 k s2
                         (ce3,s4) = cpsExp e3 k s3
                      in (cpsExp e1 (LamExp v (IfExp (VarExp v) ce2 ce3)) s4)

cpsExp (AppExp f arg) k syms | isSimple arg = (AppExp (AppExp f arg) k, syms)
                             | otherwise = let (name,s2) = gensym syms
                                            in cpsExp arg (LamExp name (AppExp (AppExp f (VarExp name)) k)) s2
cpsExp e@(OpExp op e1 e2) k syms =
  case (isSimple e1, isSimple e2) of
     (True,True) -> (AppExp k e,syms)
     (True,False) -> cpsExp e2 (LamExp name (AppExp k (OpExp op e1 (VarExp name)))) s2
           where (name,s2) = gensym syms
     (False,True) -> cpsExp e1 (LamExp name (AppExp k (OpExp op (VarExp name) e2))) s2
           where (name,s2) = gensym syms
     (False,False) ->
           let (v1,s2) = gensym syms
               (v2,s3) = gensym s2
               base = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
               (ce2,s4) = cpsExp e2 base s3
            in cpsExp e1 (LamExp v1 ce2) s4

cpsIt str = case parseExp str of
               Right x -> toStr $ fst $ cpsExp x (VarExp "k") 1
               Left x -> show x
