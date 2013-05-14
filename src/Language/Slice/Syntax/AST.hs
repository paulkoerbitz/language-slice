module Language.Slice.Syntax.AST
       ( IncludeDelimiters(..)
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

data SliceType = STVoid 
               | STBool | STByte | STShort | STInt | STLong 
               | STFloat | STDouble 
               | STString 
               | STUserDefined String
               | STUserDefinedPrx String
               deriving (Show, Read, Eq)
                 
data SliceVal = SliceBool Bool
              | SliceStr String
              | SliceInteger Integer
              | SliceDouble Double
              | SliceIdentifier String
              deriving (Show, Read, Eq)

data SliceDecl = ModuleDecl String [SliceDecl]
               | IncludeDecl IncludeDelimiters String
               | EnumDecl String [String]
               | StructDecl String [FieldDecl]
               | ClassDecl String (Maybe String) [MethodOrFieldDecl]
               | InterfaceDecl String [String] [MethodDecl]
               | InterfaceFDecl String
               | SequenceDecl SliceType String
               | DictionaryDecl SliceType SliceType String
               | ExceptionDecl String [String] [FieldDecl]
               | ConstDecl SliceType String SliceVal
               deriving (Show, Read, Eq)

data Annotation = Idempotent deriving (Show, Read, Eq)

data FieldDecl = FieldDecl SliceType String (Maybe SliceVal) deriving (Show, Read, Eq)

data MethodDecl = MethodDecl SliceType String [FieldDecl] [String] (Maybe Annotation) deriving (Show, Read, Eq)

data MethodOrFieldDecl = MDecl MethodDecl | FDecl FieldDecl deriving (Show, Read, Eq)
