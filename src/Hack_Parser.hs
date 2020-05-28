module Hack_Parser where

import Data.Char (isSpace, GeneralCategory(..))
import Data.Void
import Text.Megaparsec --(Parsec, runParser, (<|>), try, manyTill, skipMany, skipManyTill, anySingle, eof, optional, sepEndBy, some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Error (errorBundlePretty)
import System.Exit (exitFailure)
import Control.Monad (void)

import Hack_AST

type Parser = Parsec Void String

-- General Parser that uses a specific parser to parse a given File
parseFile :: Parser a -> String -> IO a
parseFile parser filename = do
    input <- readFile filename
    case runParser parser filename input of 
        Left err -> putStrLn (errorBundlePretty err) >> exitFailure   -- Als er een parse error is dan print hij de fout en sluit hij het programma af
        Right result -> return result --als hij successvol parsed returned hij het resultaat
        
-- General Parser that uses a specific parser to parse a given string
parseString:: Parser a -> String -> IO a
parseString parser input = do
    case runParser parser "inputstring" input of 
        Left err -> putStrLn (errorBundlePretty err) >> exitFailure   -- Als er een parse error is dan print hij de fout en sluit hij het programma af
        Right result -> return result --als hij successvol parsed returned hij het resultaat        
      
parseAssembly :: Parser [Instruction String]
parseAssembly = customSeperator *> sepEndBy parseCommand separator <* eof
  where
    -- skip eerst 0 of meer whitespace, dan een optional comment, dan eol, dan customseparator (comments en whitespace)
    separator = skipManySpace >> optional parseComment >> eol >> customSeperator

parseCommand :: Parser (Instruction String)
parseCommand = parseLabel <|> parseAddressInstruction <|> parseComputeInstruction

customSeperator :: Parser ()
customSeperator = skipMany (space1 <|> parseComment)
    
parseComment :: Parser ()
parseComment = do
    char '/'
    char '/'
    void $ skipManyTill anySingle (lookAhead eol)

-- skip zero or more occurences of whitespace EXCEPT when the whitespace is newline
skipManySpace :: Parser ()
skipManySpace = skipMany $ satisfy (\c -> isSpace c && c /= '\n' && c /= '\r')
--
--PARSING LABELS
--
parseLabel :: Parser (Instruction a)
parseLabel = do
    char '('
    label <- manyTill anySingle (char ')')
    return $ LabelInstr label

--
--PARSING ADDRESS INSTRUCTIONS
--
parseAddressInstruction :: Parser (Instruction String)
parseAddressInstruction = AddrInstr <$> do
    _ <- char '@'
    parseFixedAddress <|> parseVariableAddress
    
parseFixedAddress :: Parser (Address String)
parseFixedAddress = FixedInt <$> decimal

  -- geeft nu vaste string terug, eerste applicatie werkt zonder symbols     
parseVariableAddress :: Parser (Address String)
parseVariableAddress = Symbol <$> some (alphaNumChar <|> punctuationChar <|> charCategory CurrencySymbol )


--
--PARSING COMPUTE INSTRUCTIONS
--

parseComputeInstruction  :: Parser (Instruction a)
parseComputeInstruction = computeInstruction <|> constJumpInstruction

-- e.g. D=A+1;JGT
computeInstruction :: Parser (Instruction a)
computeInstruction = do
    dest <- try $ parseDestination  <* char '='  
    func <- parseFunction
    jump <- parseJump <|> return NOJ
    return $ ComputeInstr func dest jump

-- e.g. D;JEQ
constJumpInstruction :: Parser (Instruction a)
constJumpInstruction = do
    param <- parseParam
    jump <- parseJump <|> return NOJ
    return $ ComputeInstr (Const param) (Dest False False False) jump
    
  -- alternatieven moeten nog worden toegevoegd, maar als het goed is staan ze steeds in deze volgorde
parseDestination :: Parser (Dest)
parseDestination = amd <|> am <|> ad <|> md <|> a <|> m <|> d
    where
        amd = Dest {regA=True, regM=True, regD=True} <$ string "AMD"
        am = Dest {regA=True, regM=True, regD=False} <$ string "AM"
        ad = Dest {regA=True, regM=False, regD=True} <$ string "AD"
        md = Dest {regA=False, regM=True, regD=True} <$ string "MD"
        a = Dest {regA=True, regM=False, regD=False} <$ char 'A'
        m = Dest {regA=False, regM=True, regD=False} <$ char 'M'
        d = Dest {regA=False, regM=False, regD=True} <$ char 'D'

parseFunction :: Parser (Function)
parseFunction = oneArg <|> twoArgs  <|> constArg

twoArgs :: Parser (Function)
twoArgs = do
    (arg1, func) <- try $ (,) <$> parseParam <*> operator2params
    arg2 <- parseParam
    return $ func arg1 arg2
        where
            operator2params =
                    Add <$ char '+'
                <|> Subst <$ char '-'
                <|> And <$ char '&'
                <|> Or <$ char '|'

oneArg :: Parser (Function)
oneArg = do
    func <- Not <$ char '!' <|> Neg <$ char '-'
    arg <- parseParam
    return $ func arg
    
constArg :: Parser (Function)
constArg = do
    arg <- parseParam
    return $ Const arg

parseParam :: Parser (Param)
parseParam = 
        Zero <$ char '0' 
    <|> One <$ char '1' 
    <|> A <$ char 'A' 
    <|> M <$ char 'M' 
    <|> D <$ char 'D'

parseJump :: Parser (Jump)
parseJump = do
    _ <- char ';'
    jump <- jumps
    return jump
        where
          jumps = 
                  JGT <$ string "JGT"
              <|> JEQ <$ string "JEQ" 
              <|> JGE <$ string "JGE" 
              <|> JLT <$ string "JLT" 
              <|> JNE <$ string "JNE" 
              <|> JLE <$ string "JLE" 
              <|> JMP <$ string "JMP" 
    

