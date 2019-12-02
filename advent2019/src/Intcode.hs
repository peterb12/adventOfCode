{-# LANGUAGE OverloadedStrings #-}
module Intcode where
import Data.Maybe
import Debug.Trace

-- An Intcode computer consists of a sequential series of memory locations.
-- Some of the memory locations are opcodes.
-- Some of them contain data.

type Computer = [Int]
data Opcode = Add | Multiply | Halt deriving Show

opcode :: Int -> Maybe Opcode
opcode 1  = Just Add
opcode 2  = Just Multiply
opcode 99 = Just Halt
opcode _  = Nothing

opsize :: Maybe Opcode -> Int
opsize (Just Add)      = 4
opsize (Just Multiply) = 4
opsize (Just Halt)     = 1
opsize Nothing         = 0

poke :: Computer -> Int -> Int -> Computer
poke cmp loc val =  take loc cmp ++ [val] ++ drop (loc + 1) cmp

peek :: Computer -> Int -> Int
peek = (!!)

fetchInstr :: Computer -> Int -> Maybe Opcode
fetchInstr cmp = (opcode . peek cmp)

execute :: Computer -> Int -> Computer
execute cmp pc =
  case op of
    Just Add      -> execute (add cmp x y dest) next
    Just Multiply -> execute (multiply cmp x y dest) next
    Just Halt     -> cmp
    Nothing       -> error "PANIC: Invalid instruction."
    where op = fetchInstr cmp pc
          next = pc + opsize op
          x    = peek cmp (pc + 1)
          y    = peek cmp (pc + 2)
          dest = peek cmp (pc + 3)

-- The default assumption here is that x and y are pointers.
add :: Computer -> Int -> Int -> Int -> Computer
add cmp x y dest = poke cmp dest ((peek cmp x) + (peek cmp y))

multiply :: Computer -> Int -> Int -> Int -> Computer
multiply cmp x y dest = poke cmp dest ((peek cmp x) * (peek cmp y))

search :: Int -> Computer -> Int -> Int -> Int
search target cmp x y
  | (peek result 0) == target = ((100 * x) + y)
  | otherwise =
    if (x == 99)
    then (search target cmp 0 (y + 1))
    else (search target cmp (x + 1) y)
    where
      result = execute mutatedCmp 0
      mutatedCmp = poke (poke cmp 1 x) 2 y

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
