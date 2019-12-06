{-# LANGUAGE OverloadedStrings #-}
module Intcode where
import Data.Maybe
import Debug.Trace

-- An Intcode computer consists of a sequential series of memory locations.
-- Some of the memory locations are opcodes.
-- Some of them contain data.

type Computer  = [Int]
data Opcode    = Add | Multiply | Input | Output | Halt |
                 JNZ | JEZ | JLT | JEQ deriving Show
data Mode      = Immediate | Indirect deriving (Eq, Show)
data Parameter = P Mode Int deriving Show
data Operation = O (Maybe Opcode) [Parameter] deriving Show

opcode :: Int -> Maybe Opcode
opcode 1  = Just Add
opcode 2  = Just Multiply
opcode 3  = Just Input
opcode 4  = Just Output
opcode 5  = Just JNZ
opcode 6  = Just JEZ
opcode 7  = Just JLT
opcode 8  = Just JEQ
opcode 99 = Just Halt
opcode _  = Nothing

opsize :: Maybe Opcode -> Int
opsize (Just Add)      = 4
opsize (Just Multiply) = 4
opsize (Just Halt)     = 1
opsize (Just Input)    = 2
opsize (Just Output)   = 2
opsize (Just JNZ)      = 3
opsize (Just JEZ)      = 3
opsize (Just JLT)      = 4
opsize (Just JEQ)      = 4
opsize Nothing         = 0

poke :: Computer -> Int -> Int -> Computer
poke cmp loc val = take loc cmp ++ [val] ++ drop (loc + 1) cmp

peek :: Computer -> Int -> Int
peek cmp x = cmp !! x

getDigit :: Int -> Int -> Int
getDigit val place = (val `mod` (10 ^ (place+1))) `div` (10 ^ place)

fetchInstr :: Computer -> Int -> Operation
fetchInstr cmp x = O opc (take (opsize opc) ml)
  where
    n   = peek cmp x
    m1  = intToMode (getDigit n 2)
    m2  = intToMode (getDigit n 3)
    m3  = intToMode (getDigit n 4)
    opc = opcode (mod n 100)
    ml  = [P m1 (peek cmp (x + 1)), P m2 (peek cmp (x + 2)), P m3 (peek cmp (x + 3))]

intToMode :: Int -> Mode
intToMode 0 = Indirect
intToMode 1 = Immediate

execute :: Computer -> Int -> Computer
execute cmp pc =
  case op of
    Just Add -> execute (add cmp arg1 arg2 arg3) next
    Just Multiply -> execute (multiply cmp arg1 arg2 arg3) next
    Just Halt -> cmp
    Just Input -> execute (input cmp arg1 5) next
    Just Output -> execute (output cmp arg1) next
    Just JNZ ->
      execute
        cmp
        (if jnzDest == pc
           then next
           else jnzDest)
    Just JEZ ->
      execute
        cmp
        (if jezDest == pc
           then next
           else jezDest)
    Just JLT -> execute (jlt cmp arg1 arg2 arg3) next
    Just JEQ -> execute (jeq cmp arg1 arg2 arg3) next
    Nothing -> error "PANIC: Invalid instruction."
  where
    (O op plist) = fetchInstr cmp pc
    next = pc + opsize op
    jnzDest = jnz cmp arg1 arg2 pc
    jezDest = jez cmp arg1 arg2 pc
    arg1 = plist !! 0
    arg2 = plist !! 1
    arg3 = plist !! 2

add :: Computer -> Parameter -> Parameter -> Parameter -> Computer
add cmp x y (P m3 dest) = poke cmp dest (xval + yval)
  where xval = decode cmp x
        yval = decode cmp y

multiply :: Computer -> Parameter -> Parameter -> Parameter -> Computer
multiply cmp x y (P m3 dest) = poke cmp dest (xval * yval)
  where xval = decode cmp x
        yval = decode cmp y

output :: Computer -> Parameter -> Computer
output cmp arg = traceShow ("VALUE: " ++ show xval) $ cmp
  where
    xval = decode cmp arg

input :: Computer -> Parameter -> Int -> Computer
input cmp (P mx x) user =  poke cmp x user

jnz :: Computer -> Parameter -> Parameter -> Int -> Int
jnz cmp tst newPC pc =
  case value of
    0 -> pc
    _ -> decode cmp newPC
  where
    value = decode cmp tst

jez :: Computer -> Parameter -> Parameter -> Int -> Int
jez cmp tst newPC pc =
  case value of
    0 -> decode cmp newPC
    _ -> pc
  where
    value = decode cmp tst

-- Key: "Parameters that an instruction writes to will never be in immediate mode."
-- So even though our destinations are 'indirect', they are really the final destination.
jlt :: Computer -> Parameter -> Parameter -> Parameter -> Computer
jlt cmp x y (P _ dest)
  | (decode cmp x) < (decode cmp y) = poke cmp dest 1
  | otherwise = poke cmp dest 0

jeq :: Computer -> Parameter -> Parameter -> Parameter -> Computer
jeq cmp x y (P _ dest)
  | (decode cmp x) == (decode cmp y) = poke cmp dest 1
  | otherwise = poke cmp dest 0
  
decode :: Computer -> Parameter -> Int
decode _   (P Immediate x) = x
decode cmp (P Indirect  x) = peek cmp x
            
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
