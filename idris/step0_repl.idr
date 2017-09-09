module Main

data Noop = MkNoop

read : String -> String
read x = x

eval : String -> String
eval x = x

print : String -> String
print x = x

rep : String -> String
rep = print . eval . read

repl : Noop -> String -> Maybe (String, Noop)
repl _ "" = Nothing
repl n x  = Just $ ((rep x) ++ "\n", n)

main : IO ()
main = replWith MkNoop "user> " repl
