{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString                      as BS
import qualified Test.HUnit                           as HU
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Language.Slice.Syntax.Parser
import           Language.Slice.Syntax.AST

testCases :: HU.Test
testCases = HU.TestList
  [ " include quotes"  HU.~: Right [IncludeDecl Quotes "some/relative/path.ice"] HU.@=? parseSlice "#include \"some/relative/path.ice\""
  , " include backets" HU.~: Right [IncludeDecl AngleBrackets "some/relative/path.ice"] HU.@=? parseSlice "#include <some/relative/path.ice>"
  , " enum 1"          HU.~: Right [EnumDecl "plain" []] HU.@=? parseSlice "enum plain {};"
  , " enum 2"          HU.~: Right [EnumDecl "plain" ["MyVal"]] HU.@=? parseSlice "enum plain { MyVal };"
  , " enum 3"          HU.~: Right [EnumDecl "MyEnumWith_Underscores" ["Val_1","Val_2"]] HU.@=? parseSlice "enum MyEnumWith_Underscores \n{\n\tVal_1,\n\tVal_2\n};"
  , " enum 4"          HU.~: Right [EnumDecl "MyEnumWith_Underscores" ["Val_1","Val_2"]] HU.@=? parseSlice "enum MyEnumWith_Underscores \n{\n\tVal_1,\n\tVal_2\n};;"      
  , " plain module"    HU.~: Right [ModuleDecl "plain" []] HU.@=? parseSlice "module plain {};"
  , " nested module"   HU.~: Right [ModuleDecl "nested" [ModuleDecl "nested2" []]] HU.@=? parseSlice "module nested { module nested2 {}; };"
  , " method 1"        HU.~: Right (MethodDecl STVoid "myMethod" [] []) HU.@=? parseOnly parseMethod "void myMethod();"
  , " method 2"        HU.~: Right (MethodDecl STVoid "myMethod" [] ["MyException"]) HU.@=? parseOnly parseMethod "void myMethod() throws MyException;"
  , " method 3"        HU.~: Right (MethodDecl STVoid "myMethod" [FieldDecl STBool "myBool"] []) HU.@=? parseOnly parseMethod "void myMethod(bool myBool);"
  , " method 4"        HU.~: Right (MethodDecl STString "myMethod" [FieldDecl STBool "myBool", FieldDecl STDouble "My_Double"] ["MyException1","my_exception_1"]) HU.@=? parseOnly parseMethod "string myMethod(bool myBool , double My_Double) throws MyException1, my_exception_1;"
  , " method list 1"   HU.~: Right [] HU.@=? parseOnly (parseList parseMethod) ""
  , " method list 2"   HU.~: Right [MethodDecl STString "myMethod" [] [], MethodDecl STString "myMethod" [] []] HU.@=? parseOnly (parseList parseMethod) "string myMethod(); string myMethod();"
  , " interface 1"     HU.~: Right [InterfaceDecl "MyInterface" [] []] HU.@=? parseSlice "interface MyInterface {};"
  , " interface 2"     HU.~: Right [InterfaceDecl "MyInterface" ["Interface1"] []] HU.@=? parseSlice "interface MyInterface extends Interface1 {};"
  , " interface 3"     HU.~: Right [InterfaceDecl "MyInterface" ["Interface1"] [MethodDecl STVoid "method1" [] []]] HU.@=? parseSlice "interface MyInterface extends Interface1 { void method1(); };"
  , " interface 4"     
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [], MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam"] []]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\tbool method2(MyType myParam);\n};"
  , " interface 5"
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [], MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam"] ["MyException", "MyOtherException"]]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\n\tbool method2(MyType myParam) throws MyException, MyOtherException;\n};"
  , " interface 6"
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [], MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam"] ["MyException", "MyOtherException"]]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();;\n\tbool method2(MyType myParam) throws MyException, MyOtherException;\n};"
  , " module list"     HU.~: Right [ModuleDecl "plain" [], ModuleDecl "plain" []] HU.@=? parseSlice "module plain {};\nmodule plain {};"
  , " ifndef 1"        HU.~: Right [ModuleDecl "plain" []] HU.@=? parseSlice "#ifndef GUARD_1\n#define GUARD_1\n\nmodule plain {};\n#endif"
  ]

fileTestData :: [(String,String)]
fileTestData = [("test/testSlice01.ice","test/testSlice01.ast")]

tests :: [Test]
tests = [testGroup "Basic declaration parsing" $ hUnitTestToTests testCases]

main :: IO ()
main = do
  slcData <- mapM (BS.readFile . fst) fileTestData
  astData <- mapM (readFile . snd) fileTestData
  let fileTests = [testGroup "File tests" $ hUnitTestToTests $ HU.TestList $ 
                   zipWith3 (\(nm,_) str ast -> nm HU.~: (read ast :: Either String [SliceDecl]) HU.@=? parseSlice str) fileTestData slcData astData]
      allTests  = tests ++ fileTests
  defaultMain allTests