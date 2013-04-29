{-# LANGUAGE OverloadedStrings #-}
module Language.Slice.Syntax.Parser
       ( parseSlice,
         parseMethod,
         parseField,
         parseList,
         parseOnly
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
parseSliceInternal = skipWs >> do
  parseModule
  <|> parseInclude
  <|> parseEnum
  <|> parseStruct
  <|> parseClass
  <|> parseInterface
  <|> parseSequence
  <|> parseDictionary
  <|> parseException
  <?> "top level" 
  
c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral

char :: Char -> Parser Word8
char c = word8 . c2w $ c

isWs :: Word8 -> Bool
isWs w = w == 32 || w == 9 || w == 10 || w == 12

isWsOrSem :: Word8 -> Bool
isWsOrSem w = w == 32 || w == 9 || w == 10 || w == 12 || w == 59 

skipWs :: Parser ()
skipWs = skipWhile isWs

skipWsOrSem :: Parser ()
skipWsOrSem = skipWhile isWsOrSem

skipComment :: Parser ()
skipComment = (string "/*" >> scan False starSearcher >> string "*/" >> return ()) 
              <|> (string "//" >> skip (/= 10) >> char '\n' >> return ())
              <?> "comment"
  where
    starSearcher True  47 = Nothing    -- /
    starSearcher _     42 = Just True  -- *
    starSearcher _     _  = Just False
    
identifier :: Parser String
identifier = AT.takeWhile1 (inClass "a-zA-Z0-9_") >>= return . unpack

parseType :: Parser SliceType
parseType = ((string "void" >> return STVoid)
             <|> (string "bool" >> return STBool)
             <|> (string "byte" >> return STByte)
             <|> (string "int" >> return STInt)
             <|> (string "long" >> return STLong)
             <|> (string "float" >> return STFloat)
             <|> (string "double" >> return STDouble)
             <|> (string "string" >> return STString)
             <|> do tn <- identifier
                    (char '*' >> return (STUserDefinedPrx tn)) <|> return (STUserDefined tn))
            <?> "type"
            
liftWs :: Parser a -> Parser a
liftWs parser = skipWs >> parser >>= \i -> skipWs >> return i
                   
parseSepList :: Parser a -> Parser b -> Parser [b]
parseSepList sep parser = go [] <?> "sep list"
  where
    go lst = do i <- liftWs parser 
                (sep >> go (i:lst)) <|> (return (Prelude.reverse $ i:lst))
             <|> if Prelude.null lst then return [] else fail " parseSepList: extra seperator"

parseList :: Parser b -> Parser [b]
parseList parser = go [] <?> "list"
  where
    go lst = do i <- liftWs parser
                go (i:lst)
             <|>
             (return $ Prelude.reverse lst)
                   
parseBlock :: ByteString -> Parser a -> Parser (String, a)
parseBlock kw parser = do
    string kw >> skipWs
    name <- identifier
    skipWs >> char '{'
    decls <- parser
    skipWs >> char '}' >> skipWsOrSem
    return (name,decls)
  <?> "block"
  
parseExtBlock :: ByteString -> Parser a -> Parser (String, [String], a)
parseExtBlock kw parser = 
  do string kw >> skipWs
     name <- identifier
     skipWs
     exts <- parseExtensions
     skipWs >> char '{'
     decls <- parser
     skipWs >> char '}' >> skipWs >> char ';' >> skipWs
     return (name, exts, decls)
  <?> "ext block"
  where
    parseExtensions = 
      do string "extends" >> skipWs
         parseSepList (char ',') identifier
      <|> return []

parseModule :: Parser SliceDecl
parseModule = do
    (name,decls) <- parseBlock "module" (parseSepList skipWs parseSliceInternal)
    return (ModuleDecl name decls)
  <?> "module"
  
parseInclude :: Parser SliceDecl
parseInclude = do
    string "#include" >> skipWs
    c <- char '"' <|> char '<'
    path <- AT.takeWhile $ inClass "-_a-zA-Z0-9./"
    char (closingChar (w2c c))
    skipWsOrSem
    case (w2c c) of
      '"' -> return $ IncludeDecl Quotes (unpack path)
      '<' -> return $ IncludeDecl AngleBrackets (unpack path)
  <?> "include"
  where
    closingChar '"' = '"'
    closingChar '<' = '>'

parseEnum :: Parser SliceDecl
parseEnum = do
    (name,decls) <- parseBlock "enum" (parseSepList (char ',') identifier)
    return (EnumDecl name decls)
  <?> "enum"
  

parseStruct :: Parser SliceDecl
parseStruct = do
    (name,decls) <- parseBlock "struct" (parseList parseSemTermField)
    return (StructDecl name decls)
  <?> "struct"

parseClass :: Parser SliceDecl
parseClass = do
    (name,exts,decls) <- parseExtBlock "class" (parseList parseMethodOrField)
    return $ ClassDecl name (safeHead exts) decls
  <?> "class"
  where
    safeHead []     = Nothing
    safeHead (x:xs) = Just x

parseInterface :: Parser SliceDecl
parseInterface = 
  do (name,exts,decls) <- parseExtBlock "interface" (parseList parseMethod)
     return $ InterfaceDecl name exts decls
  <?> "interface"

parseException :: Parser SliceDecl
parseException =  do
    (name,exts,decls) <- parseExtBlock "exception" (parseList parseSemTermField)
    return $ ExceptionDecl name exts decls
  <?> "interface"

parseSequence :: Parser SliceDecl
parseSequence = do
  string "sequence<"
  type' <- parseType
  char '>' >> skipWs
  name <- identifier
  skipWs >> char ';' >> skipWsOrSem
  return $ SequenceDecl type' name

parseDictionary :: Parser SliceDecl
parseDictionary = do
  string "dictionary<"
  type1 <- parseType
  skipWs >> char ',' >> skipWs
  type2 <- parseType
  char '>' >> skipWs
  name <- identifier
  skipWs >> char ';' >> skipWsOrSem
  return $ DictionaryDecl type1 type2 name

parseField :: Parser FieldDecl
parseField = do
  type' <- parseType
  skipWs
  name <- identifier
  skipWs
  return $ FieldDecl type' name
  
parseSemTermField :: Parser FieldDecl
parseSemTermField = do
  field <- parseField
  skipWs >> char ';' >> skipWsOrSem
  return field
  
parseMethod :: Parser MethodDecl
parseMethod = do
  rType <- parseType
  skipWs
  name <- identifier
  skipWs >> char '('
  fields <- parseSepList (char ',') parseField
  skipWs >> char ')' 
  excepts <- (skipWs >> string "throws" >> skipWs >> parseSepList (char ',') identifier) <|> return []
  skipWs >> char ';' >> skipWsOrSem
  return $ MethodDecl rType name fields excepts
  
parseMethodOrField :: Parser MethodOrFieldDecl
parseMethodOrField = (parseMethod >>= return . MDecl) <|> (parseSemTermField >>= return . FDecl)