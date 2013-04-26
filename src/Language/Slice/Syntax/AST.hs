module Language.Slice.Syntax.AST
       ( IncludeDelimiters(..)
       , SliceType(..)
       , SliceDecl(..)
       , FieldDecl(..)
       , MethodDecl(..)
       , MethodOrFieldDecl(..)
       ) where

data IncludeDelimiters = AngleBrackets | Quotes deriving (Show, Eq)

data SliceType = STVoid 
               | STBool | STByte | STShort | STInt | STLong 
               | STFloat | STDouble 
               | STString 
               | STUserDefined String
               | STUserDefinedPrx String
               deriving (Show, Eq)
                 
data SliceExpr = SliceStr String
               | SliceInt Int
               | SliceDouble Double
               deriving (Show, Eq)

data SliceDecl = ModuleDecl String [SliceDecl]
               | IncludeDecl IncludeDelimiters String
               | EnumDecl String [String]
               | StructDecl String [FieldDecl]
               | ClassDecl String (Maybe String) [MethodOrFieldDecl]
               | InterfaceDecl String [String] [MethodDecl]
               | SequenceDecl SliceType String
               | DictionaryDecl SliceType SliceType String
               | ExceptionDecl String [String] [FieldDecl]
               | ConstDecl SliceType String SliceExpr
               deriving (Show, Eq)

data FieldDecl = FieldDecl SliceType String deriving (Show, Eq)

data MethodDecl = MethodDecl SliceType String [FieldDecl] [String] deriving (Show, Eq)

data MethodOrFieldDecl = MDecl MethodDecl | FDecl FieldDecl deriving (Show, Eq)
