{-# LANGUAGE OverloadedStrings #-}
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
  , " enum 3"          HU.~: Right (EnumDecl "MyEnumWith_Underscores" ["Val_1","Val_2"]) HU.@=? parseSlice "enum MyEnumWith_Underscores \n{\n\tVal_1,\n\tVal_2\n};"
  , " plain module"    HU.~: Right (ModuleDecl "plain" []) HU.@=? parseSlice "module plain {};"
  , " nested module"   HU.~: Right (ModuleDecl "nested" [ModuleDecl "nested2" []]) HU.@=? parseSlice "module nested { module nested2 {}; };"
  , " interface 1"     HU.~: Right (InterfaceDecl "MyInterface" ["Interface1"] [MethodDecl STVoid "method1" [] []]) HU.@=? parseSlice "interface MyInterface extends Interface1 { void method1(); };"
  , " interface 2"     
    HU.~:  Right (InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [], MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam"] []]) 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\tbool method2(MyType myParam);\n};"
  , " interface 3"
    HU.~:  Right (InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [], MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam"] ["MyException", "MyOtherException"]]) 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\n\tbool method2(MyType myParam) throws MyException, MyOtherException;\n};"
  ]
            
tests :: [Test]
tests = [testGroup "Basic declaration parsing" $ hUnitTestToTests testCases]

main :: IO ()
main = defaultMain tests