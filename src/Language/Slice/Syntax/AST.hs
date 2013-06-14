module Language.Slice.Syntax.AST
       ( IncludeDelimiters(..)
       , Ident(..)
       , NsQualIdent(..)
       , SliceType(..)
       , SliceVal(..)
       , SliceDecl(..)
       -- , DefaultValue(..)
       , Annotation(..)
       , FieldDecl(..)
       , MethodDecl(..)
       , MethodOrFieldDecl(..)
       ) where


data IncludeDelimiters = AngleBrackets | Quotes deriving (Show, Read, Eq)

newtype Ident = Ident String deriving (Show, Read, Eq)

data NsQualIdent = NsQualIdent { name :: String, ns :: [String] }
                 deriving (Show, Read, Eq)

data SliceType = STVoid 
               | STBool | STByte | STShort | STInt | STLong 
               | STFloat | STDouble 
               | STString 
               | STUserDefined NsQualIdent
               | STUserDefinedPrx NsQualIdent
               deriving (Show, Read, Eq)
                 
data SliceVal = SliceBool Bool
              | SliceStr String
              | SliceInteger Integer
              | SliceDouble Double
              | SliceIdentifier NsQualIdent
              deriving (Show, Read, Eq)

data SliceDecl = ModuleDecl Ident [SliceDecl]
               | IncludeDecl IncludeDelimiters String
               | EnumDecl Ident [Ident]
               | StructDecl Ident [FieldDecl]
               | ClassDecl Ident (Maybe NsQualIdent) [MethodOrFieldDecl]
               | InterfaceDecl Ident [NsQualIdent] [MethodDecl]
               | InterfaceFDecl NsQualIdent
               | SequenceDecl SliceType Ident
               | DictionaryDecl SliceType SliceType Ident
               | ExceptionDecl Ident [NsQualIdent] [FieldDecl]
               | ConstDecl SliceType Ident SliceVal
               deriving (Show, Read, Eq)

data Annotation = Idempotent deriving (Show, Read, Eq)

data FieldDecl = FieldDecl SliceType Ident (Maybe SliceVal) deriving (Show, Read, Eq)

data MethodDecl = MethodDecl SliceType Ident [FieldDecl] [NsQualIdent] (Maybe Annotation) deriving (Show, Read, Eq)

data MethodOrFieldDecl = MDecl MethodDecl | FDecl FieldDecl deriving (Show, Read, Eq)
