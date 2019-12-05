{-# LANGUAGE OverloadedStrings #-}
module Intcode where
import Data.Maybe
import Debug.Trace

-- An Intcode computer consists of a sequential series of memory locations.
-- Some of the memory locations are opcodes.
-- Some of them contain data.

type Computer  = [Int]
data Opcode    = Add | Multiply | Store | Output | Halt deriving Show
data Mode      = Immediate | Indirect deriving (Eq, Show)
data Parameter = P Mode Int deriving Show
data Operation = O (Maybe Opcode) [Parameter] deriving Show

opcode :: Int -> Maybe Opcode
opcode 1  = Just Add
opcode 2  = Just Multiply
opcode 3  = Just Store
opcode 4  = Just Output
opcode 99 = Just Halt
opcode _  = Nothing

opsize :: Maybe Opcode -> Int
opsize (Just Add)      = 4
opsize (Just Multiply) = 4
opsize (Just Halt)     = 1
opsize (Just Store)    = 2
opsize (Just Output)   = 2
opsize Nothing         = 0

poke :: Computer -> Int -> Int -> Computer
poke cmp loc val =  take loc cmp ++ [val] ++ drop (loc + 1) cmp

peek :: Computer -> Int -> Int
peek cmp x = cmp !! x

getDigit :: Int -> Int -> Int
getDigit val place = (val `mod` (10 ^ (place+1))) `div` (10 ^ place)

fetchInstr :: Computer -> Int -> Operation
fetchInstr cmp x =  (O opc (take (opsize opc) ml))
  where n = peek cmp x
        m1  = intToMode (getDigit n 2)
        m2  = intToMode (getDigit n 3)
        m3  = intToMode (getDigit n 4)
        opc = (opcode (mod n 100))
        ml  = [P m1 (peek cmp (x+1)), P m2 (peek cmp (x+2)), P m3 (peek cmp (x+3))]

intToMode :: Int -> Mode
intToMode 0 = Indirect
intToMode 1 = Immediate

execute :: Computer -> Int -> Computer
execute cmp pc =
  case op of
    Just Add      -> execute (add cmp (plist !! 0) (plist !! 1) (plist !! 2)) next
    Just Multiply -> execute (multiply cmp (plist !! 0) (plist !! 1) (plist !! 2)) next
    Just Halt     -> cmp
    Just Store    -> execute (input cmp (plist !! 0) 1) next
    Just Output   -> execute (output cmp (plist !! 0)) next
    Nothing       -> error "PANIC: Invalid instruction."
    where all@(O op plist) = fetchInstr cmp pc
          next = pc + opsize op
add :: Computer -> Parameter -> Parameter -> Parameter -> Computer
add cmp (P mx x) (P my y) (P m3 dest) = poke cmp dest (xval + yval)
  where xval = if mx == Immediate then x else (peek cmp x)
        yval = if my == Immediate then y else (peek cmp y)

multiply :: Computer -> Parameter -> Parameter -> Parameter -> Computer
multiply cmp (P mx x) (P my y) (P m3 dest) = poke cmp dest (xval * yval)
  where xval = if mx == Immediate then x else (peek cmp x)
        yval = if my == Immediate then y else (peek cmp y)

output :: Computer -> Parameter -> Computer
output cmp (P mx x) = traceShow xval $ cmp
  where xval = (peek cmp x)

input :: Computer -> Parameter -> Int -> Computer
input cmp (P mx x) user = poke cmp x user

-- Used in exercise Day02
search :: Int -> Computer -> Int -> Int -> Int
search = undefined
--search target cmp x y
--  | (peek result 0) == target = ((100 * x) + y)
--  | otherwise =
--    if (x == 99)
--    then (search target cmp 0 (y + 1))
--    else (search target cmp (x + 1) y)
--    where
--      result = execute mutatedCmp 0
--      mutatedCmp = poke (poke cmp 1 x) 2 y

--- For historical info only:  here's a variant of "execute" that uses lenses.
-- execute: Given a memory map and a program counter, execute the instruction
-- at that program counter.
-- Assumes no invalid input. This may have to change for part 2...
--execute :: [Int] -> Int -> [Int]
--execute memory pc
--  | memory !! pc == 99 = memory -- If we find '99', we are done.
--  | memory !! pc == 1  = execute (memory & (element dest) .~ (x + y)) next
--  | memory !! pc == 2  = execute ( memory & (element dest) .~ (x * y)) next
--  | otherwise = traceShow "FATAL ERROR" $ memory
--  where x    = memory !! (memory !! ((fromIntegral pc) + 1))
--        y    = memory !! (memory !! ((fromIntegral pc) + 2))
--        dest = memory !! ((pc + 3))
--        next = pc + 4
