module Printer

import Types

ci : List String -> String
ci = concat . intersperse " "

export
pr_str : MalType -> String
pr_str (MalList xs) = "(" ++ ci (map pr_str xs) ++ ")" 
pr_str (MalVector xs) = "[" ++ ci (map pr_str xs) ++ "]" 
pr_str (MalMap xs) = "{" ++ ci (map pr_v xs) ++ "}"
  where pr_v ((StringKey key), val) = show key ++ " " ++ pr_str val
        pr_v ((KeywordKey key), val) = ":" ++ key ++ " " ++ pr_str val
pr_str (MalNumber x) = show x
pr_str (MalSymbol x) = x
pr_str (MalFunction name _) = name
pr_str (MalString s) = show s
pr_str MalNil = "nil"
pr_str (MalBool True) = "true"
pr_str (MalBool False) = "false"
pr_str (MalKeyword kw) = ":" ++ kw
