{-# LANGUAGE OverloadedStrings #-}
module Language.Slice.Syntax.Parser
       ( parseSlice
       ) where

import           Control.Applicative ((<|>))
import           Language.Slice.Syntax.AST
import           Data.Attoparsec as AT
import           Data.ByteString.Char8 
import           Data.Char (ord, chr)
import           Data.Word (Word8)

-- Final parser
parseSlice :: ByteString -> Either String SliceDecl
parseSlice = parseOnly parseSliceInternal

parseSliceInternal :: Parser SliceDecl
parseSliceInternal = skipWS >> do
  parseModule
  <|> parseInclude
  <|> parseEnum
  <|> parseStruct
  <|> parseClass
  <|> parseInterface
  <|> parseSequence
  <|> parseDictionary
  <|> parseException
  
c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral

char :: Char -> Parser Word8
char c = word8 . c2w $ c

isWS :: Word8 -> Bool
isWS w = w == 32 || w == 9 || w == 10 || w == 12

skipWS :: Parser ()
skipWS = skipWhile isWS

skipComment :: Parser ()
skipComment = (string "/*" >> scan False starSearcher >> string "*/" >> return ()) 
              <|> (string "//" >> skip (/= 10) >> char '\n' >> return ())
  where
    starSearcher True  47 = Nothing    -- /
    starSearcher _     42 = Just True  -- *
    starSearcher _     _  = Just False
    
identifier :: Parser String
identifier = AT.takeWhile1 (inClass "a-zA-Z0-9_") >>= return . unpack

parseType :: Parser SliceType
parseType = (string "bool" >> return STBool)
            <|> (string "byte" >> return STByte)
            <|> (string "int" >> return STInt)
            <|> (string "long" >> return STLong)
            <|> (string "float" >> return STFloat)
            <|> (string "double" >> return STDouble)
            <|> (string "string" >> return STString)
            <|> do tn <- identifier
                   (char '*' >> return (STUserDefinedPrx tn)) <|> return (STUserDefined tn)
                   
parseSepList :: Parser a -> Parser b -> Parser [b]
parseSepList sep parser = go []
  where
    go lst = (parseItem >>= \i -> (sep >> go (i:lst)) <|> return (Prelude.reverse $ i:lst)) 
             <|> if Prelude.null lst then return [] else fail " parseSepList: extra seperator"
    parseItem = skipWS >> parser >>= \i -> skipWS >> return i

                   
parseBlock :: ByteString -> Parser a -> Parser (String, a)
parseBlock kw parser = do
  string kw >> skipWS
  name <- identifier
  skipWS >> char '{'
  decls <- parser
  skipWS >> char '}' >> skipWS >> char ';'
  return (name,decls)
  
parseExtBlock :: ByteString -> Parser a -> Parser (String, [String], a)
parseExtBlock kw parser = do
  string kw >> skipWS
  name <- identifier
  skipWS
  exts <- parseExtensions
  skipWS >> char '{'
  decls <- parser
  skipWS >> string "};"
  return (name, exts, decls)
  where
    parseExtensions = do
      string "extends" >> skipWS
      parseSepList (char ',') identifier

parseModule :: Parser SliceDecl
parseModule = do
  (name,decls) <- parseBlock "module" (parseSepList skipWS parseSliceInternal)
  return (ModuleDecl name decls)
  
parseInclude :: Parser SliceDecl
parseInclude = do
  string "#include" >> skipWS
  c <- char '"' <|> char '<'
  path <- AT.takeWhile $ inClass "-_a-zA-Z0-9./"
  char (closingChar (w2c c))
  case (w2c c) of
    '"' -> return $ IncludeDecl Quotes (unpack path)
    '<' -> return $ IncludeDecl AngleBrackets (unpack path)
  where
    closingChar '"' = '"'
    closingChar '<' = '>'

parseEnum :: Parser SliceDecl
parseEnum = do
  (name,decls) <- parseBlock "enum" (parseSepList (char ',') identifier)
  return (EnumDecl name decls)

parseStruct :: Parser SliceDecl
parseStruct = do
  (name,decls) <- parseBlock "struct" (parseSepList (char ';') parseField)
  return (StructDecl name decls)

parseClass :: Parser SliceDecl
parseClass = do
  (name,exts,decls) <- parseExtBlock "class" (parseSepList (char ';') parseMethodOrField)
  return $ ClassDecl name (safeHead exts) decls
  where
    safeHead []     = Nothing
    safeHead (x:xs) = Just x

parseInterface :: Parser SliceDecl
parseInterface = do
  (name,exts,decls) <- parseExtBlock "interface" (parseSepList (char ';') parseMethod)
  return $ InterfaceDecl name exts decls

parseException :: Parser SliceDecl
parseException =  do
  (name,exts,decls) <- parseExtBlock "exception" (parseSepList (char ';') parseField)
  return $ ExceptionDecl name exts decls

parseSequence :: Parser SliceDecl
parseSequence = do
  string "sequence<"
  type' <- parseType
  char '>' >> skipWS
  name <- identifier
  skipWS >> char ';'
  return $ SequenceDecl type' name

parseDictionary :: Parser SliceDecl
parseDictionary = do
  string "dictionary<"
  type1 <- parseType
  skipWS >> char ',' >> skipWS
  type2 <- parseType
  char '>' >> skipWS
  name <- identifier
  skipWS >> char ';'
  return $ DictionaryDecl type1 type2 name

parseField :: Parser FieldDecl
parseField = do
  type' <- parseType
  skipWS
  name <- identifier
  skipWS >> char ';'
  return $ FieldDecl type' name
  
parseMethod :: Parser MethodDecl
parseMethod = do
  rType <- parseType
  skipWS
  name <- identifier
  skipWS >> char '('
  fields <- parseSepList (char ',') parseField
  skipWS >> char ')' >> skipWS >> char ';'
  return $ MethodDecl rType name fields
  
parseMethodOrField :: Parser MethodOrFieldDecl
parseMethodOrField = (parseMethod >>= return . MDecl) <|> (parseField >>= return . FDecl)
  