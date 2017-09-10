module ReaderTest

import Types
import Reader
import Printer

import Lightyear.Testing

%access export

data Test : Type where
  MkTest : (inp: String) -> (exp: String) -> Test

runTest : Test -> String
runTest (MkTest inp exp) =
  case read_str inp of
    Left(err) => "Failed parsing on '" ++ inp ++ "': " ++ err
    Right(out) => 
      case pr_str out == exp of
        True  => "Success on '" ++ inp ++ "'"
        False => "Failed on '" ++ inp ++ "': got '" ++ pr_str out ++ "', but expected '" ++ exp ++ "'"

runTests : List Test -> IO ()
runTests = traverse_ (putStrLn . runTest)

readerTests : IO ()
readerTests = runTests
  [ MkTest "1" "1"
  , MkTest "7" "7"
  , MkTest "   7" "7"
  , MkTest "-123" "-123"
  , MkTest "+" "+"
  , MkTest "abc " "abc"
  , MkTest "   abc   " "abc"
  , MkTest "abc5 " "abc5"
  , MkTest "abc-def " "abc-def"
  , MkTest "(+ 1 2) " "(+ 1 2)"
  , MkTest "() " "()"
  , MkTest "(nil) " "(nil)"
  , MkTest "((3 4)) " "((3 4))"
  , MkTest "(+ 1 (+ 2 3)) " "(+ 1 (+ 2 3))"
  , MkTest "  ( +   1   (+   2 3   )   )  " "(+ 1 (+ 2 3))"
  , MkTest "(* 1 2) " "(* 1 2)"
  , MkTest "(** 1 2) " "(** 1 2)"
  , MkTest "(* -3 6) " "(* -3 6)"
  , MkTest "(1 2, 3,,,,),, " "(1 2 3)"
  , MkTest "nil " "nil"
  , MkTest "true " "true"
  , MkTest "false " "false"
  , MkTest "\"abc\" " "\"abc\""
  , MkTest "   \"abc\"   " "\"abc\""
  , MkTest "\"abc (with parens)\" " "\"abc (with parens)\""
  , MkTest "\"abc\\\"def" " \"\"abc\\\"def\""
  , MkTest ";;;\"abc\\ndef\" ;;;" "\"abc\\ndef\""
  , MkTest "\"\" " "\"\""
  , MkTest "'1 " "(quote 1)"
  , MkTest "'(1 2 3) " "(quote (1 2 3))"
  , MkTest "`1 " "(quasiquote 1)"
  , MkTest "`(1 2 3) " "(quasiquote (1 2 3))"
  , MkTest "~1 " "(unquote 1)"
  , MkTest "~(1 2 3) " "(unquote (1 2 3))"
  , MkTest "`(1 ~a 3) " "(quasiquote (1 (unquote a) 3))"
  , MkTest "~@(1 2 3) " "(splice-unquote (1 2 3))"
  , MkTest ":kw " ":kw"
  , MkTest "(:kw1 :kw2 :kw3) " "(:kw1 :kw2 :kw3)"
  , MkTest "[+ 1 2] " "[+ 1 2]"
  , MkTest "[] " "[]"
  , MkTest "[[3 4]] " "[[3 4]]"
  , MkTest "[+ 1 [+ 2 3]] " "[+ 1 [+ 2 3]]"
  , MkTest "  [ +   1   [+   2 3   ]   ]  " "[+ 1 [+ 2 3]]"
  , MkTest "{\"abc\" 1} " "{\"abc\" 1}"
  , MkTest "{\"a\" {\"b\" 2}} " "{\"a\" {\"b\" 2}}"
  , MkTest "{\"a\" {\"b\" {\"c\" 3}}} " "{\"a\" {\"b\" {\"c\" 3}}}"
  , MkTest "{  \"a\"  {\"b\"   {  \"cde\"     3   }  }} " "{\"a\" {\"b\" {\"cde\" 3}}}"
  , MkTest "{  :a  {:b   {  :cde     3   }  }} " "{:a {:b {:cde 3}}}"
  , MkTest "1 ; comment after expression " "1"
  , MkTest "1; comment after expression " "1"
  , MkTest "^{\"a\" 1} [1 2 3] " "(with-meta [1 2 3] {\"a\" 1})"
  , MkTest "@a " "(deref a)"
  ]
