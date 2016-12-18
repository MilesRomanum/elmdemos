import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Bitwise

import Text as Text
import List as List
import Signal as Signal
import Array exposing (..)

{- ----------------------------------------------------------------------------------------
 - CPU Visulatiozation Code
 - ----------------------------------------------------------------------------------------
 -}

boxSize     = 80
diagramSize = 800


-- register values diagram

registerDiagram : CPUState -> Element
registerDiagram (CPUState regs _ _ _) 
  = let
      (r1,r2,r3,r4,r5,r6,r7,r8) = regs
      regs'      = [r1,r2,r3,r4,r5,r6,r7,r8]

      regBox : RegisterValue -> Int -> Element
      regBox r n = color (rgba 100 100 250 0.7)
                      <| container boxSize boxSize middle 
                      <| flow down [show r,centered (Text.fromString <| "Reg" ++ toString n)]
    in container diagramSize diagramSize middle 
          <| flow right 
          <| List.map2 regBox regs' [1..8]

-- show current instruction in diagram

instrDiagram : CPUState -> Element
instrDiagram (CPUState regs i com halted)
  case fetchInstruction (CPUState regs i com halted) program of
      Just instr -> container diagramSize diagramSize midBottom <| show (i,instr)
      Nothing    -> container diagramSize diagramSize midBottom <| show (i,IllegalAddress)


-- diagram for visualizing current CPU state

memDiagram : Data -> Int -> Element
memDiagram dat ptr= let mem = slice (ptr) (ptr+10) dat
                        mn = [(ptr)..(ptr+10)]
                        membox m n =  
                        color (rgba 100 100 250 0.7)
                        <|container boxSize boxSize middle  
                        <|flow down [show m, centered (Text.fromString <| "Mem" ++ toString n)]
                     in
                        container diagramSize diagramSize midTop
                                  <| flow right
                                  <| List.map2 membox (toList mem) mn
                    


{--

cpuCond (CPUState regs i com halted) = case halted of Nothing -> 
                                                      Just halt -> 

--}

cpuDiagram : (CPUState,Data,Int) -> Element
cpuDiagram (cpuState,dat,ptr) = layers [instrDiagram cpuState, cpuCond cpuState, registerDiagram cpuState, (memDiagram dat ptr)]



-- update function, execute one instruction on signal update 
-- and return new cpu state

updateCPU : Bool -> (CPUState,Data,Int) -> (CPUState,Data,Int)
updateCPU update (cpuState,dat,ptr) = 
    case (update,fetchInstruction cpuState program) of
         ( False, _          ) -> (cpuState,dat,ptr)
         ( _    , Nothing    ) -> (cpuState,dat,ptr)
         ( _    , Just instr ) -> executeOne instr (cpuState,dat,ptr) 


-- render CPU Diagram to the screen using the `enter` keyboard signal
-- to increment through the program

renderCPUDiagram : Signal Element 
renderCPUDiagram = Signal.map cpuDiagram 
                <| Signal.foldp updateCPU (initialState,(Array.set 1023 0 initialData),0) (Keyboard.enter)

main : Signal Element
main = renderCPUDiagram

{- ----------------------------------------------------------------------------------------
 - Example Program
 - ----------------------------------------------------------------------------------------
 -}

program : Program
program = mkProgram code

code : List Instruction
code = 
  [
    LoadImmediate 1 1,        -- 0
    LoadImmediate 2 1,        -- 1
    LoadImmediate 3 10,       -- 2
    LoadImmediate 5 4,        -- 3
    Add           4 4 1,      -- 4
    Store         4 0 2,      -- 5
    Add           2 2 1,      -- 6
    Compare       2 3,        -- 7
    Branch        [LT,EQ] 5,  -- 8
    Halt                      -- 9
  ]
 

{- ----------------------------------------------------------------------------------------
 - CPU Model 
 - ----------------------------------------------------------------------------------------
 -}
 
-- the transitions for the CPUState are all instructions
-- "the CPU is a good listener"

type Instruction  = Load           RegisterNumber           -- put value here
                                   RegisterNumber           -- from address which is sum of this register value
                                   RegisterNumber           -- and this register value
  -- this is like reading a number in the margin of the Sudoku puzzle
                  | Store          RegisterNumber           -- store value in this register
                                   RegisterNumber           -- to address which is sum of this register value
                                   RegisterNumber           --   and this register value
  -- this is like writing something in the margin of the Sudoku puzzle
                  | LoadImmediate  RegisterNumber           -- put value here
                                   RegisterValue            -- the value
  -- this is like reading a number as part of a logic puzzle
                  | Add            RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to add
                                   RegisterNumber           -- second thing to add

                  | Multiply       RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to multiply
                                   RegisterNumber           -- second thing to multiply

                  | And            RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to and
                                   RegisterNumber           -- second thing to and

                  | Or             RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to or
                                   RegisterNumber           -- second thing to or

                  | Not            RegisterNumber           -- put result here
                                   RegisterNumber           -- reverse bits from here

                  | Rotate         RegisterNumber           -- put result here
                                   RegisterNumber           -- value to rotate
                                   Int                      -- rotate bits (left is positive)

                  | Compare        RegisterNumber           -- compare the value in this register
                                   RegisterNumber           -- to the value in this register (result goes in CPU State)

                  | Branch         (List ComparisonResult)  -- results (maybe all) which will cause branch
                                   RegisterNumber           -- instruction to branch to if true

                  | Halt

type alias Program = Int -> Maybe Instruction

type alias Data = Array Int
type alias Code = List Instruction

mkProgram : List Instruction -> Program
mkProgram instrs = let code = Array.fromList instrs
                   in  \ idx -> Array.get idx code
                   
type alias RegisterNumber   = Int
type alias RegisterValue    = Int
type alias ComparisonResult = Order


{- ----------------------------------------------------------------------------------------
 - CPU Simulation
 - ----------------------------------------------------------------------------------------
 -}
 
 
initialState = CPUState  (0,0,0,0,0,0,0,0)     -- all registers start with value zero
                         0                     -- start execution with the first instruction
                         EQ                    -- result of test starts as equal
                         Nothing
dataSize = 1024
initialData  : Array Int
initialData  = Array.initialize 1024 id

id n = 0

initialTinyData = Array.initialize 4 identity

fetchInstruction : CPUState -> Program -> Maybe Instruction
fetchInstruction (CPUState _ curInstr _ _) prog = prog curInstr


-- executeOne gives a semantics for the CPU instruction language
executeOne : Instruction -> (CPUState,Data,Int) -> (CPUState,Data,Int)
executeOne instr (cpuState,dat,ptr) =
  case instr of
    Load target addr1 addr2 -> let
                                 newCpuState = case (getRegisterVal addr1  cpuState
                                                    ,getRegisterVal addr2  cpuState) of
                                                 (Just addr1',Just addr2') -> case Array.get (addr1' + addr2') dat of
                                                                                     Just loadValue -> changeRegister target loadValue cpuState
                                                                                     Nothing -> illegalAddress cpuState
                                                 otherwise -> illegalRegister cpuState
                               in
                                 nextInstruction (newCpuState, dat, (addr1 + addr2))

    Store regNum addr1 addr2 -> nextInstruction <|
                                    case (getRegisterVal addr1  cpuState
                                         ,getRegisterVal addr2  cpuState
                                         ,getRegisterVal regNum cpuState) of
                                      (Just addr10, Just addr20, Just value3) -> if 0 <= addr10 + addr20 && addr10 + addr20 < dataSize
                                                                            then (cpuState, (Array.set (addr10 + addr20) value3 dat), (addr10 + addr20))
                                                                            else (illegalAddress cpuState, dat, ptr)
                                      otherwise -> (illegalRegister cpuState,dat,ptr)

    LoadImmediate target value -> nextInstruction
                                    (changeRegister target value cpuState, dat, ptr)

    Add target arg1 arg2 -> twoArgOp (cpuState,dat,ptr) target arg1 arg2 (+)
    Multiply target arg1 arg2 -> twoArgOp (cpuState,dat,ptr) target arg1 arg2 (*)
    And target arg1 arg2 -> twoArgOp (cpuState,dat,ptr) target arg1 arg2 Bitwise.and
    Or target arg1 arg2 -> twoArgOp (cpuState,dat,ptr) target arg1 arg2 Bitwise.or

    Not target arg1 -> (case getRegisterVal arg1 cpuState of
                          Just val  -> changeRegister target (Bitwise.complement val) cpuState
                          otherwise -> illegalRegister cpuState
                       , dat, ptr)
                       |> nextInstruction

    Rotate target arg1 shift -> (case getRegisterVal arg1 cpuState of
                                   Just val  -> changeRegister target (Bitwise.shiftLeft val (-shift)) cpuState
                                   otherwise -> illegalRegister cpuState
                                , dat, ptr)
                                |> nextInstruction

    Compare arg1 arg2 -> (case (getRegisterVal arg1 cpuState
                               ,getRegisterVal arg2 cpuState) of
                            (Just x,Just y) -> let (CPUState regs inst result halted) = cpuState
                                               in   CPUState regs inst (compare x y) halted
                            otherwise       -> illegalRegister cpuState
                         ,dat,ptr)
                         |> nextInstruction

    Branch branchOn targetAddress ->
      let (CPUState regs inst result halted) = cpuState
      in (case getRegisterVal targetAddress cpuState of
            Just nextInst -> if result `List.member` branchOn
                               then if 0 <= nextInst && nextInst < 4400
                                      then CPUState regs nextInst result halted
                                      else illegalInstrAddress cpuState
                               else CPUState regs (inst + 1) result halted
            otherwise     -> illegalRegister cpuState
         ,dat,ptr)
    Halt  ->
      let (CPUState regs inst result halted) = cpuState
      in  (CPUState regs inst result (Just ReachedHalt),dat,ptr)

twoArgOp (cpuState,dat,ptr) target arg1 arg2 op = (case (getRegisterVal arg1 cpuState
                                                    ,getRegisterVal arg2 cpuState) of
                                                 (Just x,Just y) -> changeRegister  target (x `op` y) cpuState
                                                 otherwise       -> illegalRegister cpuState
                                              , dat , ptr)
                                              |> nextInstruction


illegalAddress (CPUState regs currInstr comparedAs _)
              = CPUState regs currInstr comparedAs (Just IllegalAddress)

illegalInstrAddress (CPUState regs currInstr comparedAs _)
                   = CPUState regs currInstr comparedAs (Just IllegalInstrAddress)

illegalRegister (CPUState regs currInstr comparedAs _)
               = CPUState regs currInstr comparedAs (Just IllegalRegisterNum)

changeRegister  regNum
                newValue
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                           currentInstruction
                           comparedAs
                           maybeHalted
                )
  = case regNum of
      1 -> CPUState (newValue,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      2 -> CPUState (reg1,newValue,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      3 -> CPUState (reg1,reg2,newValue,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      4 -> CPUState (reg1,reg2,reg3,newValue,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      5 -> CPUState (reg1,reg2,reg3,reg4,newValue,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      6 -> CPUState (reg1,reg2,reg3,reg4,reg5,newValue,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      7 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,newValue,reg8)
                    currentInstruction comparedAs maybeHalted
      8 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,newValue)
                    currentInstruction comparedAs maybeHalted
      _ -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs (Just IllegalRegisterNum)

getRegisterVal  regNum
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                           currentInstruction
                           comparedAs
                           maybeHalted
                )
  = case regNum of
      0 -> Just 0
      1 -> Just reg1
      2 -> Just reg2
      3 -> Just reg3
      4 -> Just reg4
      5 -> Just reg5
      6 -> Just reg6
      7 -> Just reg7
      8 -> Just reg8
      _ -> Nothing


nextInstruction  (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                            currentInstruction
                            comparedAs
                            maybeHalted
                 ,dat,ptr)
  = (CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
              (case maybeHalted of
                 Nothing -> currentInstruction + 1
                 otherwise -> currentInstruction     -- don't go on if halted
              )
              comparedAs
              maybeHalted
    ,dat,ptr)

runProgram : Program -> (CPUState,Data,Int) -> (CPUState,Data,Int)
runProgram prog init= runProgram' prog init

runProgram' : Program -> (CPUState,Data,Int) -> (CPUState,Data,Int)
runProgram' prog (CPUState regs inst result halted,dat,ptr)
  = case halted of
      Nothing   -> case prog inst of
                     Just i  -> runProgram' prog (executeOne i (CPUState regs inst result halted,dat,ptr))
                     Nothing -> ((CPUState regs inst result (Just IllegalInstrAddress)),dat,ptr)
      otherwise -> ((CPUState regs inst result halted),dat,ptr)

type alias CurrentInstruction = Int

type HaltedBecause  = ReachedHalt
                    | IllegalRegisterNum
                    | IllegalAddress
                    | IllegalInstrAddress

type CPUState = CPUState  (RegisterValue, RegisterValue, RegisterValue, RegisterValue,
                           RegisterValue, RegisterValue, RegisterValue, RegisterValue)
                          CurrentInstruction
                          ComparisonResult
                          (Maybe HaltedBecause)

-- registers are like the working memory in our brains
--   to be useful, we need these numbers fast
--     because of physics students (and the speed of light) this means close
--     only so many things can be close, that's the limit on registers
-- when we run out of space, we put our numbers in main memory (RAM)
--   (there is an intermediate middle-sized memory called cache)
-- think of main memory as book, with lines and pages
--   number of these boxes is called a memory address

