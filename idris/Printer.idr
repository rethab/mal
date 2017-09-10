module Printer

import Types

public export
pr_str : MalType -> String
pr_str (MalList xs) = "(" ++ ci (map pr_str xs) ++ ")" 
  where ci = concat . intersperse " "
pr_str (MalNumber x) = show x
pr_str (MalSymbol x) = x
pr_str (MalFunction _) = "<fn>"
pr_str (MalString s) = show s
pr_str MalNil = "nil"
pr_str (MalBool True) = "true"
pr_str (MalBool False) = "false"
