module Main

import Types
import Reader
import Printer

mutual 

  eval_list : List MalType -> Env -> Either String (List MalType, Env)
  eval_list [] e = Right ([], e)
  eval_list (y :: ys) e = do
    (v, e') <- eval y e
    (vs, e'') <- eval_list ys e'
    Right (v :: vs, e'')

  eval_ast : MalType -> Env -> Either String (MalType, Env)
  eval_ast (MalList xs) env = do
      (vs, env') <- eval_list xs env
      Right (MalList vs, env')
  eval_ast n@(MalNumber _) env = Right (n, env)
  eval_ast f@(MalFunction _) env = Right (f, env)
  eval_ast (MalSymbol s) env =
    case symLookup env s of
      Just sym => Right (sym, env)
      Nothing  => Left $ "Symbol " ++ s ++ " not found"

  eval : MalType -> Env -> Either String (MalType, Env)
  eval ast@(MalList []) env = Right (ast, env)
  eval ast@(MalList (x :: [])) env = eval_ast ast env
  eval (MalList (x :: (y :: ys))) env =
    do (f, env') <- eval_function x env
       (args, env'') <- eval_list (y :: ys) env'
       res <- f args
       Right (res, env'')
  eval ast env = eval_ast ast env

  eval_function : MalType -> Env -> Either String (Function, Env)
  eval_function exp env =
    case eval_ast exp env of
      Right (MalFunction f, e) => Right (f, e)
      Right (v, _) => Left (pr_str v ++ " is not a function")
      Left err => Left err



print : MalType -> String
print x = pr_str x

read : String -> Either String MalType
read x = read_str x

rep : String -> Env -> Either String (String, Env)
rep inp env = do
  exp <- read inp
  (res, env') <- eval exp env
  Right (print res, env')

repl : Env -> String -> Maybe (String, Env)
repl _ "" = Nothing
repl env line  =
  case rep line env of
    Right (out, env') => Just $ (out ++ "\n", env')
    Left err  => Just $ (err ++ "\n", env)

main : IO ()
main = replWith empty "user> " repl
