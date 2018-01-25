{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.MoreParserTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Course.MoreParser
       (between, betweenCharTok, charTok, commaTok, digits1, hex, noneof,
        oneof, option, quote, sepby1, spaces, string, stringTok, tok)
import Course.Parser (ParseResult(..), character, digit, is, parse)

import Course.ParserTest (checkError)

test_MoreParser :: TestTree
test_MoreParser =
  testGroup
    "MoreParser"
    [ spacesTest
    , tokTest
    , charTokTest
    , commaTokTest
    , parseQuoteTest
    , stringTest
    , stringTokTest
    , optionTest
    , digits1Test
    , oneofTest
    , noneOfTest
    , betweenTest
    , betweenCharTokTest
    , hexTest
    , sepBy1Test
    ]

spacesTest :: TestTree
spacesTest =
  testGroup
    "spaces"
    [ testCase "leading spaces" $ parse spaces " abc" @?= Result "abc" " "
    , testCase "noleading spaces" $ parse spaces "abc" @?= Result "abc" ""
    ]

tokTest :: TestTree
tokTest =
  testGroup
    "tok"
    [ testCase "parse a token" $ parse (tok (is 'a')) "abc" @?= Result "bc" 'a'
    , testCase "parse a token with extra spaces" $ parse (tok (is 'a')) "a bc" @?= Result "bc" 'a'
    ]

charTokTest :: TestTree
charTokTest =
  testGroup
    "charTok"
    [ testCase "valid character" $ parse (charTok 'a') "abc" @?= Result "bc" 'a'
    , testCase "invalid leading character " $ checkError $ parse (charTok 'a') "dabc"
    ]

commaTokTest :: TestTree
commaTokTest =
  testGroup
    "commaTok"
    [ testCase "leading comma" $ parse commaTok ",123" @?= Result "123" ','
    , testCase "nonleading comma" $ checkError $ parse commaTok "1,23"
    ]

parseQuoteTest :: TestTree
parseQuoteTest =
  testGroup
    "parseQuote"
    [ testCase "leading single quote" $ parse quote "'abc" @?= Result "abc" '\''
    , testCase "leading double quote" $ parse quote "\"abc" @?= Result "abc" '"'
    , testCase "nonleading quote" $ checkError $ parse quote "abc"
    ]

stringTest :: TestTree
stringTest =
  testGroup
    "string"
    [ testCase "qualifiedMatches" $ parse (string "abc") "abcdef" @?= Result "def" "abc"
    , testCase "unqualifiedMatches" $ checkError $ parse (string "abc") "bcdef"
    ]

stringTokTest :: TestTree
stringTokTest =
  testGroup
    "stringTok"
    [ testCase "parses a string with spaces" $ parse (stringTok "abc") "abc  " @?= Result "" "abc"
    , testCase "unqualifiedMatches" $ checkError $ parse (stringTok "abc") "bc  "
    ]

optionTest :: TestTree
optionTest =
  testGroup
    "option"
    [ testCase "successful parser" $ parse (option 'x' character) "abc" @?= Result "bc" 'a'
    , testCase "unsuccessful parser" $ parse (option ':' digit) "abc" @?= Result "abc" ':'
    , testCase "empty input with char parser" $ parse (option 'x' character) "" @?= Result "" 'x'
    ]

digits1Test :: TestTree
digits1Test =
  testGroup
    "digits1"
    [ testCase "leads with digits" $ parse digits1 "123" @?= Result "" "123"
    , testCase "does not lead with digits" $ checkError $ parse digits1 "abc123"
    ]

oneofTest :: TestTree
oneofTest =
  testGroup
    "oneof"
    [ testCase "leading character is an eligible character" $ parse (oneof "abc") "bcdef" @?= Result "cdef" 'b'
    , testCase "leading character is an ineligible character" $ checkError $ parse (oneof "abc") "1cdef"
    ]

noneOfTest :: TestTree
noneOfTest =
  testGroup
    "noneOf"
    [ testCase "leading character is not in the list of disallowed characters" $
      parse (noneof "bcd") "abc" @?= Result "bc" 'a'
    , testCase "leading character is in the list of disallowed characters" $ checkError $ parse (noneof "abcd") "abc"
    ]

betweenTest :: TestTree
betweenTest =
  testGroup
    "between"
    [ testCase "successful" $ parse (between (is '[') (is ']') character) "[a]" @?= Result "" 'a'
    , testCase "terminated too early" $ checkError $ parse (between (is '[') (is ']') character) "[abc]"
    , testCase "could not parse ending" $ checkError $ parse (between (is '[') (is ']') character) "[a"
    , testCase "could not parse beginning" $ checkError $ parse (between (is '[') (is ']') character) "a]"
    ]

betweenCharTokTest :: TestTree
betweenCharTokTest =
  testGroup
    "between"
    [ testCase "successful" $ parse (betweenCharTok '[' ']' character) "[a]" @?= Result "" 'a'
    , testCase "terminated too early" $ checkError $ parse (betweenCharTok '[' ']' character) "[abc]"
    , testCase "could not parse ending" $ checkError $ parse (betweenCharTok '[' ']' character) "[a"
    , testCase "could not parse beginning" $ checkError $ parse (betweenCharTok '[' ']' character) "a]"
    ]

hexTest :: TestTree
hexTest =
  testGroup
    "hex"
    [ testCase "success1" $ parse hex "0010" @?= Result "" '\DLE'
    , testCase "success2" $ parse hex "0a1f" @?= Result "" '\2591'
    , testCase "shortString" $ checkError $ parse hex "001"
    , testCase "contains nonhexadecimal digit" $ checkError $ parse hex "0axf"
    ]

sepBy1Test :: TestTree
sepBy1Test =
  testGroup
    "sepBy1"
    [ testCase "parse 1 value no seperator" $ parse (sepby1 character (is ',')) "a" @?= Result "" "a"
    , testCase "parse multiple values seperated multiple times" $
      parse (sepby1 character (is ',')) "a,b,c" @?= Result "" "abc"
    , testCase "parse multiple values with an extra (,)" $
      parse (sepby1 character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
    , testCase "empty string" $ checkError $ parse (sepby1 character (is ',')) ""
    ]
