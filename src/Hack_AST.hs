module Hack_AST where

import qualified Data.Map as M

--
-- Data types
--

data Instruction a = AddrInstr (Address a) | ComputeInstr Function Dest Jump | LabelInstr String
    deriving (Show)
    
instance Functor Instruction where
    fmap f (AddrInstr addr) = AddrInstr $ fmap f addr
    fmap _ (ComputeInstr func dest jump) = ComputeInstr func dest jump
    fmap _ (LabelInstr label) = LabelInstr label

data Address a = Symbol a | FixedInt Integer
    deriving (Show)

instance Functor Address where
    fmap f (Symbol a) = Symbol (f a)
    fmap _ (FixedInt x) = FixedInt x

data Jump =  NOJ | JGT | JEQ | JGE | JLT | JNE | JLE | JMP
    deriving (Show)
    
data Dest = Dest { regA :: Bool
                 , regD :: Bool
                 , regM :: Bool
                 } deriving (Show)
                 
data Function =
      Const Param
    | Neg Param
    | Not Param
    | Add Param Param
    | Subst Param Param
    | And Param Param
    | Or Param Param
    deriving (Show)

data Param = Zero | One | A | D | M
    deriving (Show)

--
-- toString functions to print the hack instructions as they would be written in a Hack file
--

--Een lege destination zou alleen kunnen in combinatie met een constante en een jump instructie
--Verder mag geen van de waardes leeg zijn
--Dit wordt momenteel niet afgevangen
instrToString :: Instruction String -> String
instrToString (AddrInstr addr) = "@" ++ addressToString addr
instrToString (LabelInstr label) = "(" ++ label ++ ")"
instrToString (ComputeInstr func dest NOJ) = 
    destToString dest ++ "=" ++ functionToString func
instrToString (ComputeInstr func dest jump) = case destToString dest == "" of
    True -> functionToString func ++ ";" ++ jumpToString jump
    False -> destToString dest ++ "=" ++ functionToString func ++ ";" ++ jumpToString jump

addressToString :: Address String -> String
addressToString (Symbol a) = a
addressToString (FixedInt x) = show x

jumpToString :: Jump -> String
jumpToString NOJ = ""
jumpToString jmp = show jmp

destToString :: Dest -> String
destToString dest = 
    mIf (regA dest) "A" ++ mIf (regD dest) "D" ++ mIf (regM dest) "M"
    
-- van Merijn gejat voor omzetten destination naar string
-- geeft een vooraf opgegeven waarde als de conditie true is
-- geeft een lege monoid als de conditie false is
mIf :: Monoid m => Bool -> m -> m
mIf condition v
    | condition = v
    | otherwise = mempty
    
functionToString :: Function -> String
functionToString (Const x) = paramToString x
functionToString (Neg x) = "-" ++ paramToString x
functionToString (Not x) = "!" ++ paramToString x
functionToString (Add x y) = paramToString x ++ "+" ++ paramToString y
functionToString (Subst x y) = paramToString x ++ "-" ++ paramToString y
functionToString (And x y) = paramToString x ++ "&" ++ paramToString y
functionToString (Or x y) = paramToString x ++ "|" ++ paramToString y

paramToString :: Param -> String
paramToString Zero = "0"
paramToString One = "1"
paramToString p = show p

