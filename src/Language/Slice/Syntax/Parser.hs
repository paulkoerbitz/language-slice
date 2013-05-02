{-# LANGUAGE OverloadedStrings #-}
module Language.Slice.Syntax.Parser
       ( parseSlice,
         parseMethod,
         parseField,
         parseList,
         parseOnly,
         parseType,
         parseSemTermField
--          module Language.Slice.Syntax.AST
       ) where

import           Control.Applicative ((<|>),(<$>),(<*>),(<*),(*>))
import           Control.Monad (liftM)
import           Language.Slice.Syntax.AST
import           Data.Attoparsec as AT
import           Data.Attoparsec.Char8 as ATC8 ((<*.),(.*>), Number(..), number)
import           Data.ByteString.Char8
import qualified Data.ByteString as BS
import           Data.Char (ord, chr)
import           Data.Word (Word8)
import           Data.Monoid

-- Final parser
parseSlice :: ByteString -> Either String [SliceDecl]
parseSlice = parseOnly $ parseIfDef <|> parseList parseSliceInternal

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

(<+>) :: Monoid a => Parser a -> Parser a -> Parser a
p1 <+> p2 = (<>) <$> p1 <*> p2

parseEither :: Monoid a => Parser a -> Parser a -> Parser a
parseEither p1 p2 = ((p1 <|> p2) <+> parseEither p1 p2) <|> return mempty

parseAny :: Monoid a => [Parser a] -> Parser a
parseAny ps = choice ps <+> parseAny ps <|> return mempty

--parseEither p1 p2 = (p1 <|> p2) <|> return mempty
--parseEither p1 p2 = do r1 <- (p1 <|> p2) 
--                       r2 <- parseEither p1 p2
--                       return (r1 <> r2)
--                    <|> return mempty

parseWs :: Parser ByteString
parseWs = takeWhile1 isWs

parseComment :: Parser ByteString
parseComment = BS.pack <$> 
               (("/*" .*> manyTill anyWord8 (string "*/")) <|> 
                ("//" .*> manyTill anyWord8 (string "\n")))
                                               
parseWsOrComment :: Parser ByteString
parseWsOrComment = parseEither parseComment parseWs

parseWsOrCommentOrSem :: Parser ByteString
parseWsOrCommentOrSem = parseAny [parseWs, parseComment, (string ";")]

skipWsOrComment :: Parser ()
skipWsOrComment = parseWsOrComment >> return ()
    
skipWsOrCommentOrSem :: Parser ()
skipWsOrCommentOrSem = parseWsOrCommentOrSem >> return ()

identifier :: Parser String
identifier = AT.takeWhile1 (inClass "a-zA-Z0-9_:") >>= return . unpack

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
                    skipWsOrComment
                    (char '*' >> return (STUserDefinedPrx tn)) <|> return (STUserDefined tn))
            <?> "type"
            
liftWs :: Parser a -> Parser a
liftWs parser = skipWsOrComment >> parser >>= \i -> skipWsOrComment >> return i
                   
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
    string kw >> skipWsOrComment
    name <- identifier
    skipWsOrComment >> char '{'
    decls <- parser
    skipWsOrComment >> char '}' >> skipWsOrSem
    return (name,decls)
  <?> "block"

parseExtBlock :: ByteString -> Parser a -> Parser (String, [String], a)
parseExtBlock kw parser = 
  do string kw >> skipWsOrComment
     name <- identifier
     skipWsOrComment
     exts <- parseExtensions
     skipWsOrComment >> char '{'
     decls <- parser
     skipWsOrComment >> char '}' >> skipWsOrComment >> char ';' >> skipWsOrComment
     return (name, exts, decls)
  <?> "ext block"
  where
    parseExtensions = 
      do string "extends" >> skipWsOrComment
         parseSepList (char ',') identifier
      <|> return []

parseModule :: Parser SliceDecl
parseModule = do
    (name,decls) <- parseBlock "module" (parseSepList skipWsOrComment parseSliceInternal)
    return (ModuleDecl name decls)
  <?> "module"

parseInclude :: Parser SliceDecl
parseInclude = 
  do string "#include" >> skipWsOrComment
     c <- char '"' <|> char '<'
     path <- AT.takeWhile $ inClass "-_a-zA-Z0-9./"
     char (closingChar (w2c c))
     skipWsOrCommentOrSem
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
  char '>' >> skipWsOrComment
  name <- identifier
  skipWsOrComment >> char ';' >> skipWsOrCommentOrSem
  return $ SequenceDecl type' name

parseDictionary :: Parser SliceDecl
parseDictionary = do
  string "dictionary<"
  type1 <- parseType
  skipWsOrComment >> char ',' >> skipWsOrComment
  type2 <- parseType
  char '>' >> skipWsOrComment
  name <- identifier
  skipWsOrComment >> char ';' >> skipWsOrCommentOrSem
  return $ DictionaryDecl type1 type2 name

parseField :: Parser FieldDecl
parseField = do
  type' <- parseType
  skipWsOrComment
  name <- identifier
  skipWsOrComment
  return $ FieldDecl type' name Nothing
  
parseDefValue :: Parser (Maybe DefaultValue)
parseDefValue = do
  (string "=" >> skipWsOrComment *>
   ((Just . DefaultBool <$> parseBool)
    <|> do num <- number
           case num of
             (D dbl) -> return . Just . DefaultDouble $ dbl
             (I int) -> return . Just . DefaultInteger $ int
    <|> (Just . DefaultString . unpack . BS.pack <$> parseString)
    <|> (Just . DefaultIdentifier <$> identifier))
   <* skipWsOrComment)
  <|> return Nothing
  where
    parseBool   = (string "true" >> return True) <|> (string "false" >> return False)
    parseString = "\"" .*> manyTill anyWord8 (string "\"")

parseSemTermField :: Parser FieldDecl
parseSemTermField = do
  (FieldDecl type' name _) <- parseField
  skipWsOrComment
  mDefVal <- parseDefValue
  skipWsOrComment >> char ';' >> skipWsOrCommentOrSem
  return (FieldDecl type' name mDefVal)

parseMethod :: Parser MethodDecl
parseMethod = do
  annot <- (string "idempotent" >> skipWsOrComment >> return (Just Idempotent)) <|> return Nothing
  rType <- parseType
  skipWsOrComment
  name <- identifier
  skipWsOrComment >> char '('
  fields <- parseSepList (char ',') parseField
  skipWsOrComment >> char ')' 
  excepts <- (skipWsOrComment >> string "throws" >> skipWsOrComment >> parseSepList (char ',') identifier) <|> return []
  skipWsOrComment >> char ';' >> skipWsOrCommentOrSem
  return $ MethodDecl rType name fields excepts annot

parseMethodOrField :: Parser MethodOrFieldDecl
parseMethodOrField = (parseMethod >>= return . MDecl) <|> (parseSemTermField >>= return . FDecl)

parseIfDef :: Parser [SliceDecl]
parseIfDef = do
  skipWsOrComment >> string "#ifndef" >> skipWsOrComment
  guard <- identifier
  skipWsOrComment >> string "#define" >> skipWsOrComment >> string (pack guard) >> skipWsOrComment
  result <- parseList parseSliceInternal
  skipWsOrComment >> string "#endif" >> skipWsOrComment
  return result
