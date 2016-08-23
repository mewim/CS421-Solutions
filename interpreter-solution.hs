{-----------------------------------
 - Interpreter.hs SOLUTION
 - v1.0
 -----------------------------------}

module Interpreter where

-- Language Representation
import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import Control.Monad
import Parser

fixMe = error "fix me!"

liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBool _ _ _ = ExnVal "Cannot lift"

--liftCompOp = fixMe
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ (op x y)
liftCompOp _ _ _ = ExnVal "Cannot lift"

{-----------------------------------
 - eval: The Evaluator 
 -----------------------------------}
eval :: Exp -> Env -> Val
eval (IntExp i) env = IntVal i
eval (BoolExp i) env = BoolVal i
eval (VarExp s) env = 
   case H.lookup s env of
     Just v -> v
     Nothing -> ExnVal "No match in env"
eval (IfExp e1 e2 e3) env =
   case (eval e1 env) of
     BoolVal True -> eval e2 env
     _            -> eval e3 env
eval (CompOpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       Just cop = H.lookup op compOps
    in liftCompOp cop v1 v2
eval (IntOpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       Just iop = H.lookup op intOps
    in liftIntOp iop v1 v2
eval (BoolOpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       Just bop = H.lookup op boolOps
    in liftBoolOp bop v1 v2
eval (FunExp params body) env = CloVal params body env
eval (AppExp e1 args) env =
    let vargs = Prelude.map (\x -> eval x env) args
        CloVal params body cenv = eval e1 env
     in eval body
              (Prelude.foldr (\ (k,v) e -> H.insert k v e) env 
                  $ zip params vargs)
eval (LetExp pairs body) env =
    let vpairs = Prelude.map (\(k,e) -> (k,eval e env)) pairs
     in eval body
              (Prelude.foldr (\ (k,v) e -> H.insert k v e) env 
                 vpairs)

{-----------------------------------
 - exec
 -----------------------------------}
exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
   where val = show $ eval e env
exec (SetStmt var e) penv env = ("", penv, (H.insert var val env))
   where val = eval e env

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)
exec (CallStmt name args) penv env = do
   case H.lookup name penv of
     Nothing -> ("Procedure " ++ name ++ " undefined.", penv,env)
     Just (ProcedureStmt _ params body) ->
          exec body
               penv 
              (Prelude.foldr (\ (k,v) e -> H.insert k (eval v env) e) env 
                  $ zip params args)
exec (IfStmt e1 s1 s2) penv env = if b then (exec s1 penv env) else (exec s2 penv env)
  where (BoolVal b) = eval e1 env
exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (s:ss)) penv env = (out' ++ out'', penv'', env'')
  where (out', penv', env') = exec s penv env
        (out'', penv'', env'') = exec (SeqStmt ss) penv' env'

{-----------------------------------
 - repl
 -----------------------------------}
repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"
