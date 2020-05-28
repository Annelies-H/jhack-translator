module Hack_Assembler where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Hack_AST


defaultSymbols :: M.Map String Integer
defaultSymbols = M.fromList
    [ ("SP", 0)
    , ("LCL", 1)
    , ("ARG", 2)
    , ("THIS", 3)
    , ("THAT", 4)
    , ("R0", 0)
    , ("R1", 1)
    , ("R2", 2)
    , ("R3", 3)
    , ("R4", 4)
    , ("R5", 5)
    , ("R6", 6)
    , ("R7", 7)
    , ("R8", 8)
    , ("R9", 9)
    , ("R10", 10)
    , ("R11", 11)
    , ("R12", 12)
    , ("R13", 13)
    , ("R14", 14)
    , ("R15", 15)
    , ("SCREEN", 16384)
    , ("KBD", 24576)
    ]  

firstFreeAddress :: Integer
firstFreeAddress = 16

-- adds all the labels to the symbol table of the correct program address of the next line
addLabelAddresses :: [Instruction String] -> M.Map String (Either String Integer)
addLabelAddresses instrs = snd $ foldl foo (0, Right <$> defaultSymbols) instrs
  where
    foo :: (Integer, M.Map String (Either String Integer)) -> Instruction String -> (Integer, M.Map String (Either String Integer))
    foo (i, symbolMap) (LabelInstr label) =  (i, M.insertWithKey f label (Right i) symbolMap)
    foo (i, symbolMap) _ = (i+1, symbolMap)
    
    f :: String -> Either String Integer -> Either String Integer -> Either String Integer
    f k x y = Left $ "Duplicate Label: " ++ k

-- add all the other symbols in the address instruction to the address map
addVariableAddresses :: [Instruction String] -> M.Map String Integer -> M.Map String Integer
addVariableAddresses instr addresses = snd $ foldl foo (firstFreeAddress, addresses) instr
  where
    foo :: (Integer, M.Map String Integer) -> Instruction String -> (Integer, M.Map String Integer)
    foo (i, symbols) (AddrInstr (Symbol s)) = case M.member s symbols of
        True -> (i, symbols)
        False -> (i+1, M.insert s i symbols)
    foo (i, symbols) _ = (i, symbols)    

-- convert the symbols in the address instructions to the corresponding addresses
symbolsToAddresses :: [Instruction String] -> M.Map String Integer -> [Instruction Integer]
symbolsToAddresses instr symbols = foo <$> instr
  where
    foo :: Instruction String -> Instruction Integer
    foo (AddrInstr (Symbol s)) = AddrInstr (Symbol $ fromJust (M.lookup s symbols))
    foo (AddrInstr (FixedInt x)) = (AddrInstr (FixedInt x))
    foo (LabelInstr s) = LabelInstr s
    foo (ComputeInstr f d j) = ComputeInstr f d j

-- converting a list of instructions to a list of binary strings (Instructions without binary representation are converted to Nothing)
programToBinary :: [Instruction Integer] -> [Maybe String]
programToBinary instr = foo <$> instr
  where
    foo :: Instruction Integer -> Maybe String
    foo (LabelInstr x) = Nothing
    foo x = Just $ toBinaryInt x
    

--    
-- conversion to binary for each instruction with binary representation
--
toBinaryInt :: Instruction Integer -> String
toBinaryInt (AddrInstr (Symbol x)) = toBinary (AddrInstr (FixedInt x))
toBinaryInt instr = toBinary instr

toBinary :: Instruction a -> String
toBinary (AddrInstr (FixedInt x)) = "0" ++ intToBin15 x
toBinary (ComputeInstr func dest jump) =
    "111" ++ funcToBinary func ++ destToBinary dest ++ jumpToBinary jump

funcToBinary :: Function -> String
funcToBinary arg = case arg of
    Const param -> constToBinary param
    Neg param -> negToBinary param
    Not param -> notToBinary param
    Add x y -> addToBinary x y
    Subst x y -> substToBinary x y
    And M _ -> "1000000"
    And _ M -> "1000000"
    And _ _ -> "0000000"
    Or M _ ->  "1010101"
    Or _ M ->  "1010101"
    Or _ _ ->  "0010101"
    
constToBinary :: Param -> String
constToBinary param = case param of
    Zero -> "0101010"
    One ->  "0111111"
    D ->    "0001100"
    A ->    "0110000"
    M ->    "1110000"

negToBinary :: Param -> String
negToBinary param = case param of
    One -> "0111010"
    D ->   "0001111"
    A ->   "0110011"
    M ->   "1110011"

notToBinary :: Param -> String
notToBinary param = case param of
    D -> "0001101"
    One -> constToBinary Zero
    Zero -> constToBinary One
    A -> "0110001"
    M -> "1110001"

addToBinary :: Param -> Param -> String
addToBinary x y = case (x, y) of
    (D, One) ->  "0011111"
    (One, D) ->  "0011111"
    (A, One) ->  "0110111"
    (One, A) ->  "0110111"
    (M, One) ->  "1110111"
    (One, M) ->  "1110111"
    (D, A) ->    "0000010"
    (A, D) ->    "0000010"
    (D, M) ->    "1000010"
    (M, D) ->    "1000010"
    otherwise -> "0000010"

substToBinary :: Param -> Param -> String
substToBinary x y = case (x, y) of
    (D, One) ->  "0001110"
    (A, One) ->  "0110010"
    (M, One) ->  "1110010"
    (D, A) ->    "0010011"
    (D, M) ->    "1010011"
    (A, D) ->    "0000111"
    (M, D) ->    "1000111"  
    otherwise -> "0010011"

jumpToBinary :: Jump -> String
jumpToBinary NOJ = "000"
jumpToBinary JGT = "001"
jumpToBinary JEQ = "010"
jumpToBinary JGE = "011"
jumpToBinary JLT = "100"
jumpToBinary JNE = "101"
jumpToBinary JLE = "110"
jumpToBinary JMP = "111"

destToBinary :: Dest -> String
destToBinary dest = 
    boolToBinary (regA dest) ++ boolToBinary(regD dest) ++ boolToBinary (regM dest)

boolToBinary :: Bool -> String
boolToBinary True = "1"
boolToBinary False = "0"




-- binary representation of an integer
intToBinary :: (Integral int, Show int) => int -> String
intToBinary x = showIntAtBase 2 intToDigit x ""

-- 15 bit representation of an integer
intToBin15 :: (Integral int, Show int) => int -> String
intToBin15 x = go (intToBinary x)
    where
      go result
        | length result > 15 = go (tail result)
        | length result == 15 = result
        | otherwise = go ("0" ++ result)