{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString                      as BS
import qualified Test.HUnit                           as HU
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Language.Slice.Syntax.Parser
import           Language.Slice.Syntax.AST

testCases :: HU.Test
testCases = HU.TestList
  [ " type 1"          HU.~: Right (STUserDefinedPrx "MyType") HU.@=? parseOnly parseType "MyType*"
  , " field 1"         HU.~: Right (FieldDecl (STUserDefinedPrx "MyType") "MyIdentifier" Nothing) HU.@=? parseOnly parseSemTermField "MyType* MyIdentifier;"
  , " const 1"         HU.~: Right (ConstDecl STBool "MyBool" (SliceBool True)) HU.@=? parseOnly parseConst "const bool MyBool = true;"
  , " include quotes"  HU.~: Right [IncludeDecl Quotes "some/relative/path.ice"] HU.@=? parseSlice "#include \"some/relative/path.ice\""
  , " include backets" HU.~: Right [IncludeDecl AngleBrackets "some/relative/path.ice"] HU.@=? parseSlice "#include <some/relative/path.ice>"
  , " enum 1"          HU.~: Right [EnumDecl "plain" []] HU.@=? parseSlice "enum plain {};"
  , " enum 2"          HU.~: Right [EnumDecl "plain" ["MyVal"]] HU.@=? parseSlice "enum plain { MyVal };"
  , " enum 3"          HU.~: Right [EnumDecl "MyEnumWith_Underscores" ["Val_1","Val_2"]] HU.@=? parseSlice "enum MyEnumWith_Underscores \n{\n\tVal_1,\n\tVal_2\n};"
  , " enum 4"          HU.~: Right [EnumDecl "MyEnumWith_Underscores" ["Val_1","Val_2"]] HU.@=? parseSlice "enum MyEnumWith_Underscores \n{\n\tVal_1,\n\tVal_2\n};;"      
  , " struct 1"        HU.~: Right [StructDecl "plain" []] HU.@=? parseSlice "struct plain {};"
  , " struct 2"        HU.~: Right [StructDecl "plain" [FieldDecl STBool "myBool" Nothing]] HU.@=? parseSlice "struct plain { bool myBool; };"
  , " struct 3"        HU.~: Right [StructDecl "plain" [FieldDecl STBool "myBool" (Just (SliceBool True))]] HU.@=? parseSlice "struct plain { bool myBool = true; };"
  , " struct 4"        HU.~: Right [StructDecl "plain" [FieldDecl (STUserDefined "MyType") "status" (Just (SliceIdentifier "MyIdentifier"))]] HU.@=? parseSlice "struct plain { MyType status = MyIdentifier; };"
  , " struct 5"        HU.~: Right [StructDecl "plain" [FieldDecl (STUserDefinedPrx "MyType") "status" Nothing]] HU.@=? parseSlice "struct plain { MyType* status; };"
  , " plain module"    HU.~: Right [ModuleDecl "plain" []] HU.@=? parseSlice "module plain {};"
  , " nested module"   HU.~: Right [ModuleDecl "nested" [ModuleDecl "nested2" []]] HU.@=? parseSlice "module nested { module nested2 {}; };"
  , " method 1"        HU.~: Right (MethodDecl STVoid "myMethod" [] [] Nothing) HU.@=? parseOnly parseMethod "void myMethod();"
  , " method 2"        HU.~: Right (MethodDecl STVoid "myMethod" [] ["MyException"] Nothing) HU.@=? parseOnly parseMethod "void myMethod() throws MyException;"
  , " method 3"        HU.~: Right (MethodDecl STVoid "myMethod" [FieldDecl STBool "myBool" Nothing] [] Nothing) HU.@=? parseOnly parseMethod "void myMethod(bool myBool);"
  , " method 4"        HU.~: Right (MethodDecl STString "myMethod" [FieldDecl STBool "myBool" Nothing, FieldDecl STDouble "My_Double" Nothing] ["MyException1","my_exception_1"] Nothing) HU.@=? parseOnly parseMethod "string myMethod(bool myBool , double My_Double) throws MyException1, my_exception_1;"
  , " method 5"        HU.~: Right (MethodDecl STVoid "myMethod" [] [] (Just Idempotent)) HU.@=? parseOnly parseMethod "idempotent void myMethod();"
  , " method list 1"   HU.~: Right [] HU.@=? parseOnly (parseList parseMethod) ""
  , " method list 2"   HU.~: Right [MethodDecl STString "myMethod" [] [] Nothing, MethodDecl STString "myMethod" [] [] Nothing] HU.@=? parseOnly (parseList parseMethod) "string myMethod(); string myMethod();"
  , " interface 1"     HU.~: Right [InterfaceDecl "MyInterface" [] []] HU.@=? parseSlice "interface MyInterface {};"
  , " interface 2"     HU.~: Right [InterfaceDecl "MyInterface" ["Interface1"] []] HU.@=? parseSlice "interface MyInterface extends Interface1 {};"
  , " interface 3"     HU.~: Right [InterfaceDecl "MyInterface" ["Interface1"] [MethodDecl STVoid "method1" [] [] Nothing]] HU.@=? parseSlice "interface MyInterface extends Interface1 { void method1(); };"
  , " interface 4"     
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [] Nothing, MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam" Nothing] [] Nothing]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\tbool method2(MyType myParam);\n};"
  , " interface 5"
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [] Nothing, MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam" Nothing] ["MyException", "MyOtherException"] Nothing]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();\n\tbool method2(MyType myParam) throws MyException, MyOtherException;\n};"
  , " interface 6"
    HU.~:  Right [InterfaceDecl "MyInterface" ["Interface1","Interface_2"] [MethodDecl STVoid "method1" [] [] Nothing, MethodDecl STBool "method2" [FieldDecl (STUserDefined "MyType") "myParam" Nothing] ["MyException", "MyOtherException"] Nothing]] 
    HU.@=? parseSlice "interface MyInterface extends Interface1, Interface_2\n{\n\tvoid method1();;\n\tbool method2(MyType myParam) throws MyException, MyOtherException;\n};"
  , " module list"     HU.~: Right [ModuleDecl "plain" [], ModuleDecl "plain" []] HU.@=? parseSlice "module plain {};\nmodule plain {};"
  , " ifndef 1"        HU.~: Right [ModuleDecl "plain" []] HU.@=? parseSlice "#ifndef GUARD_1\n#define GUARD_1\n\nmodule plain {};\n#endif"
  ]

fileTestData :: [(String,String)]
fileTestData = [("test/testSlice01.ice","test/testSlice01.ast")
               ,("test/testSlice02.ice","test/testSlice02.ast")]

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