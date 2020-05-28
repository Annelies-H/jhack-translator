module Main where

import Control.Monad (forM_)
import System.Environment
import System.IO
import Text.Megaparsec.Char
import Data.Maybe (fromJust, catMaybes)

import Hack_Parser
import Hack_AST
import Hack_Assembler

inputFile :: String
inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/add"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/addL"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/max"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/maxS"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/maxL"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/rect"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/rectL"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/pong"
--inputFile = "D:/nand2tetris/nand2tetris_projects/06/providedTests/pongL"
  
main :: IO ()
main = writeHackToBinary

writeHackToBinary :: IO ()
writeHackToBinary = do
  assembly <- parseFile parseAssembly (inputFile ++ ".asm")
  case sequence (addLabelAddresses assembly) of
    Left message -> print message
    Right symbols -> do
      let program = symbolsToAddresses assembly (addVariableAddresses assembly symbols)
      let binary = catMaybes $ programToBinary program
      writeFile (inputFile ++ ".hack") $ foldr (\x -> (++) (x ++ "\n")) "" binary   