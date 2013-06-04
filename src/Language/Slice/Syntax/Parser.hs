{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Language.Slice.Syntax.Parser
       ( parse
       , parseFile  
       , parseMethod
       , parseField
       , parseList
       , parseType
       , parseSemTermField
       , parseConst
       , parseIfDef
       , parseSlice
       , parseSlices
       , SyntaxError(..)
       ) where

import           Control.Applicative ((<|>),(<$>),(<*>),(<*),(*>))
import           Data.Functor.Identity (Identity)
import           Data.List (intercalate)
import           Data.Monoid
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as PBS
import qualified Text.Parsec.Error as PE

import qualified Language.Slice.Syntax.AST as AST

type Parser = PBS.Parser

parse :: P.Stream s Identity t => P.Parsec s () a -> P.SourceName -> s -> Either P.ParseError a
parse = P.parse

data SyntaxError = SyntaxError { ctxt :: String, pos :: P.SourcePos, msgs :: [PE.Message] }
                 deriving (Eq)

instance Show SyntaxError where
  show (SyntaxError ln p m) = 
    (P.sourceName p) ++ ":" ++ (show $ P.sourceLine p) ++ ":" ++ (show $ P.sourceColumn p) ++
    ": " ++ PE.showErrorMessages "," "Unknown error" "expected:" "unexpected:" "end of file" m ++
    "\n" ++ ln ++ "\n" ++ genIdnt [] sc ln ++ "^___\n"
    where
      sc    = P.sourceColumn p - 1
      genIdnt res n ('\t':xs) | n>0 = genIdnt ('\t':res) (n-8) xs
      genIdnt res n (x:xs)    | n>0 = genIdnt (' ':res) (n-1) xs
      genIdnt res _ _               = reverse res
        

parseFile :: String -> IO (Either SyntaxError [AST.SliceDecl])
parseFile file = do
  parseResult <- PBS.parseFromFile parseSlices file
  case parseResult of
    Left err      -> do
      fileData <- readFile file
      let pos'  = PE.errorPos err
          msgs' = PE.errorMessages err
          line  = head $ drop (P.sourceLine pos' - 1) $ lines fileData
      return . Left $ SyntaxError line pos' msgs'
    (Right res) -> return $ Right res

parseSlices :: Parser [AST.SliceDecl]
parseSlices = P.try parseIfDef <|> P.many1 parseSlice

parseSlice :: Parser AST.SliceDecl
parseSlice = P.spaces >> (do
  (    P.try parseModule
   <|> P.try parseInclude
   <|> P.try parseEnum
   <|> P.try parseStruct
   <|> P.try parseClass
   <|> P.try parseInterface
   <|> P.try parseInterfaceF
   <|> P.try parseSequence
   <|> P.try parseDictionary
   <|> P.try parseException))

(.*>) :: String -> Parser a -> Parser a
s .*> p = P.string s *> p

(<+>) :: Monoid a => Parser a -> Parser a -> Parser a
p1 <+> p2 = (<>) <$> p1 <*> p2

parseEither :: Monoid a => Parser a -> Parser a -> Parser a
parseEither p1 p2 = ((p1 <|> p2) <+> parseEither p1 p2) <|> return mempty

parseAny :: Monoid a => [Parser a] -> Parser a
parseAny ps = P.choice ps <+> parseAny ps <|> return mempty

parseWs :: Parser String
parseWs = P.many1 P.space

parseComment :: Parser String
parseComment =     P.try ("/*" .*> P.manyTill P.anyToken (P.try $ P.string "*/")) 
               <|> P.try ("//" .*> P.manyTill P.anyToken (P.try $ P.string "\n"))
                                               
parseWsOrComment :: Parser String
parseWsOrComment = parseEither parseComment parseWs

parseWsOrCommentOrSem :: Parser String
parseWsOrCommentOrSem = parseAny [parseWs, parseComment, (P.string ";")]

skipWsOrComment :: Parser ()
skipWsOrComment = parseWsOrComment >> return ()
    
skipWsOrCommentOrSem :: Parser ()
skipWsOrCommentOrSem = parseWsOrCommentOrSem >> return ()

chars ::String
chars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

digits :: String
digits = "0123456789"

identifierStartChars :: String
identifierStartChars = chars ++ digits ++ "_"

identifierChars :: String
identifierChars = chars ++ digits ++ "_:"

identifier :: Parser String
identifier = do c  <- P.oneOf identifierStartChars
                cs <- P.many $ P.oneOf identifierChars
                return (c:cs)

parseType :: Parser AST.SliceType
parseType = (    P.try (P.string "void" >> return AST.STVoid)
             <|> P.try (P.string "bool" >> return AST.STBool)
             <|> P.try (P.string "byte" >> return AST.STByte)
             <|> P.try (P.string "int" >> return AST.STInt)
             <|> P.try (P.string "long" >> return AST.STLong)
             <|> P.try (P.string "float" >> return AST.STFloat)
             <|> P.try (P.string "double" >> return AST.STDouble)
             <|> P.try (P.string "string" >> return AST.STString)
             <|> P.try (do tn <- identifier
                           skipWsOrComment
                           (P.char '*' >> return (AST.STUserDefinedPrx tn)) <|> return (AST.STUserDefined tn)))
            P.<?> "type"
            
liftWs :: Parser a -> Parser a
liftWs parser = skipWsOrComment *> parser <* skipWsOrComment

charWs :: Char -> Parser Char
charWs = liftWs . P.char
                   
parseSepList :: Parser a -> Parser b -> Parser [b]
parseSepList sep parser = go [] 
  where
    go lst = do i <- liftWs parser 
                (sep >> go (i:lst)) <|> (return (Prelude.reverse $ i:lst))
             <|> if Prelude.null lst then return [] else fail " parseSepList: extra seperator"

parseList :: Parser b -> Parser [b]
parseList parser = go []
  where
    go lst = do i <- liftWs parser
                go (i:lst)
             <|>
             (return $ Prelude.reverse lst)

parseBlock :: String -> Parser a -> Parser (String, a)
parseBlock kw parser = do
    P.string kw >> skipWsOrComment
    name <- identifier
    decls <- P.between (charWs '{') (charWs '}') parser <* (liftWs $ P.char ';')
    return (name,decls)
  P.<?> kw

parseExtBlock :: String -> Parser a -> Parser (String, [String], a)
parseExtBlock kw parser = 
  do P.string kw >> skipWsOrComment
     name <- identifier
     skipWsOrComment
     exts <- parseExtensions
     _ <- skipWsOrComment >> P.char '{'
     decls <- parser
     _ <- skipWsOrComment >> P.char '}' >> skipWsOrComment >> P.char ';' >> skipWsOrComment
     return (name, exts, decls)
  where
    parseExtensions = 
      do P.string "extends" >> skipWsOrComment
         parseSepList (P.char ',') identifier
      <|> return []

parseModule :: Parser AST.SliceDecl
parseModule = do
    (name,decls) <- parseBlock "module" (parseSepList skipWsOrComment parseSlice)
    return (AST.ModuleDecl name decls)
  P.<?> "module"

parseInclude :: Parser AST.SliceDecl
parseInclude = 
  do P.string "#include" >> skipWsOrComment
     (do fn <- P.between (P.char '"') (P.char  '"') (P.many1 $ P.oneOf (identifierChars ++ "-_./"))
         return $ AST.IncludeDecl AST.Quotes fn
      <|>
      do fn <- P.between (P.char '<') (P.char  '>') (P.many1 $ P.oneOf (identifierChars ++ "-_./"))
         return $ AST.IncludeDecl AST.AngleBrackets fn)
  P.<?> "include"

parseEnum :: Parser AST.SliceDecl
parseEnum = do
    (name,decls) <- parseBlock "enum" ((liftWs identifier `P.sepBy` (P.char ',')) <* P.optional (P.char ','))
    return (AST.EnumDecl name decls)
  P.<?> "enum"

parseStruct :: Parser AST.SliceDecl
parseStruct = do
    (name,decls) <- parseBlock "struct" (parseList parseSemTermField)
    return (AST.StructDecl name decls)
  P.<?> "struct"

parseClass :: Parser AST.SliceDecl
parseClass = do
    (name,exts,decls) <- parseExtBlock "class" (parseList parseMethodOrField)
    return $ AST.ClassDecl name (safeHead exts) decls
  P.<?> "class"
  where
    safeHead []     = Nothing
    safeHead (x:_)  = Just x

parseInterface :: Parser AST.SliceDecl
parseInterface = 
  do (name,exts,decls) <- parseExtBlock "interface" (parseList parseMethod)
     return $ AST.InterfaceDecl name exts decls
  P.<?> "interface"
  
parseInterfaceF :: Parser AST.SliceDecl
parseInterfaceF = do 
  nm <- P.string "interface " *> identifier
  skipWsOrComment >> P.string ";" >> skipWsOrComment
  return $ AST.InterfaceFDecl nm

parseException :: Parser AST.SliceDecl
parseException =  do
    (name,exts,decls) <- parseExtBlock "exception" (parseList parseSemTermField)
    return $ AST.ExceptionDecl name exts decls
  P.<?> "interface"

parseSequence :: Parser AST.SliceDecl
parseSequence = do
  _ <- P.string "sequence<"
  type' <- parseType
  _ <- P.char '>' >> skipWsOrComment
  name <- identifier
  _ <- skipWsOrComment >> P.char ';' >> skipWsOrCommentOrSem
  return $ AST.SequenceDecl type' name

parseDictionary :: Parser AST.SliceDecl
parseDictionary = do
  _ <- P.string "dictionary<"
  type1 <- parseType
  skipWsOrComment >> P.char ',' >> skipWsOrComment
  type2 <- parseType
  P.char '>' >> skipWsOrComment
  name <- identifier
  skipWsOrComment >> P.char ';' >> skipWsOrCommentOrSem
  return $ AST.DictionaryDecl type1 type2 name

parseField :: Parser AST.FieldDecl
parseField = do
  type' <- parseType
  skipWsOrComment
  name <- identifier
  skipWsOrComment
  return $ AST.FieldDecl type' name Nothing
  
data Number = I Integer
            | D Double

parseNumber :: Parser Number
parseNumber = 
  do preDec <- P.many1 P.digit
     (do decDot <- P.char '.'
         postDec <- P.many1 P.digit
         return (D $ read $ preDec ++ [decDot] ++ postDec)
      <|> 
      return (I $ read $ preDec))
  
parseSliceVal :: Parser AST.SliceVal
parseSliceVal = do
  ((P.string "=" >> skipWsOrComment) *>
   ((AST.SliceBool <$> parseBool)
    <|> (do num <- parseNumber
            case num of
              (D dbl) -> return . AST.SliceDouble $ dbl
              (I int) -> return . AST.SliceInteger $ int)
    <|> (AST.SliceStr <$> parseString)
    <|> (AST.SliceIdentifier <$> identifier))
   <* skipWsOrComment)
  where
    parseBool   = (P.string "true" >> return True) <|> (P.string "false" >> return False)
    parseString = P.string "\"" *> P.manyTill undefined (P.string "\"")

parseSemTermField :: Parser AST.FieldDecl
parseSemTermField = do
  (AST.FieldDecl type' name _) <- parseField
  skipWsOrComment
  mDefVal <- (parseSliceVal >>= return . Just) <|> return Nothing
  skipWsOrComment >> P.char ';' >> skipWsOrCommentOrSem
  return (AST.FieldDecl type' name mDefVal)

parseMethod :: Parser AST.MethodDecl
parseMethod = do
  annot <- (P.string "idempotent" >> skipWsOrComment >> return (Just AST.Idempotent)) <|> return Nothing
  rType <- parseType
  skipWsOrComment
  name <- identifier
  _ <- skipWsOrComment >> P.char '('
  fields <- parseSepList (P.char ',') parseField
  _ <- skipWsOrComment >> P.char ')' 
  excepts <- (skipWsOrComment >> P.string "throws" >> skipWsOrComment >> parseSepList (P.char ',') identifier) <|> return []
  skipWsOrComment >> P.char ';' >> skipWsOrCommentOrSem
  return $ AST.MethodDecl rType name fields excepts annot

parseMethodOrField :: Parser AST.MethodOrFieldDecl
parseMethodOrField = P.try (parseMethod >>= return . AST.MDecl) <|> P.try (parseSemTermField >>= return . AST.FDecl)

parseIfDef :: Parser [AST.SliceDecl]
parseIfDef = do
  skipWsOrComment >> P.string "#ifndef" >> skipWsOrComment
  guard <- identifier
  skipWsOrComment >> P.string "#define" >> skipWsOrComment >> P.string guard >> skipWsOrComment
  result <- parseList parseSlice
  skipWsOrComment >> P.string "#endif" >> skipWsOrComment
  return result
  
parseConst :: Parser AST.SliceDecl
parseConst = do
  tp <- "const" .*> skipWsOrComment >> parseType
  nm <- skipWsOrComment >> identifier
  val <- skipWsOrComment >> parseSliceVal
  return $ AST.ConstDecl tp nm val
