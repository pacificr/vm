
import Control.Monad.State
import Data.Int
import Data.Bits
import Data.Sequence as Sq

r_0 = 0
r_1 = 1
r_2 = 2
r_3 = 3
r_4 = 4
r_5 = 5
r_6 = 6
r_7 = 7
r_pc = 8
r_cond = 9

op_br = 0
op_add = 1
op_ld = 2
op_st = 3
op_jsr = 4
op_and = 5
op_ldr = 6
op_str = 7
op_rti = 8
op_not = 9
op_ldi = 10
op_sti = 11
op_jmp = 12
op_res = 13
op_lea = 14
op_trap = 15

fl_pos = 1
fl_zro = shiftL 1 1 :: Int
fl_neg = shiftL 1 2 :: Int

data Machine = Machine {
  memory :: Seq Int16,
  registers :: Seq Int16
  }

initMachine :: Machine
initMachine = Machine (Sq.replicate 65536 0) (Sq.replicate 16 0)

readMem :: Int16 -> State Machine Int16
readMem i = do
  m <- get
  return $ index (memory m) (fromIntegral i)

writeMem :: Int16 -> Int16 -> State Machine ()
writeMem i x = do
  m <- get
  put $ Machine (update (fromIntegral i) x (memory m)) (registers m)

readReg :: Int8 -> State Machine Int16
readReg i = do
  m <- get
  return $ index (registers m) (fromIntegral i)

writeReg :: Int8 -> Int16 -> State Machine ()
writeReg i x = do
  m <- get
  put $ Machine (memory m) (update (fromIntegral i) x (registers m))

setup :: State Machine ()
setup = do
  writeReg r_pc 3000

main :: IO ()
main = do
  putStrLn "Start"
  let (_, m) = runState setup initMachine
  let (_, m') = runState nextInstr m
  putStrLn "End"

nextInstr :: State Machine ()
nextInstr = return ()
