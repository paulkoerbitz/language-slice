{-# LANGUAGE OverloadedStrings #-}
import           System.Exit (exitFailure)
import qualified Test.HUnit                           as HU
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Language.Slice.Syntax.Parser
import           Language.Slice.Syntax.AST

testCases :: HU.Test
testCases = HU.TestList
  [ " include quotes"  HU.~: Right (IncludeDecl Quotes "some/relative/path.ice") HU.@=? parseSlice "#include \"some/relative/path.ice\""
  , " include backets" HU.~: Right (IncludeDecl AngleBrackets "some/relative/path.ice") HU.@=? parseSlice "#include <some/relative/path.ice>"
  , " enum 1"          HU.~: Right (EnumDecl "plain" []) HU.@=? parseSlice "enum plain {};"
  , " enum 2"          HU.~: Right (EnumDecl "plain" ["MyVal"]) HU.@=? parseSlice "enum plain { MyVal };"
  , " plain module"    HU.~: Right (ModuleDecl "plain" []) HU.@=? parseSlice "module plain {};"
  , " nested module"   HU.~: Right (ModuleDecl "nested" [ModuleDecl "nested2" []]) HU.@=? parseSlice "module nested { module nested2 {}; };"
  ]
            
tests :: [Test]
tests = [testGroup "Basic declaration parsing" $ hUnitTestToTests testCases]

main :: IO ()
main = defaultMain tests