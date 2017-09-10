module Main

import Debug.Trace

import Types
import Reader
import Printer

data Noop = MkNoop

read : String -> Either String MalType
read x = read_str x

eval : MalType -> MalType
eval x = x

print : MalType -> String
print x = pr_str x

rep : String -> Either String String
rep inp = map (print . eval) (read inp)

repl : Noop -> String -> Maybe (String, Noop)
repl _ "" = Nothing
repl n x  =
  case rep x of
    Right out => Just $ (out ++ "\n", n)
    Left err  => Just $ (err ++ "\n", n)

main : IO ()
main = replWith MkNoop "user> " repl
