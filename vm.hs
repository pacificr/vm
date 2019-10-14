
import Control.Monad.State
import Data.Int
import Data.Bits
import Data.Char
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

fl_pos = 1 :: Int16
fl_zro = shiftL 1 1 :: Int16
fl_neg = shiftL 1 2 :: Int16

trap_getc = 32 :: Int16
trap_out = 33 :: Int16
trap_puts = 34 :: Int16
trap_in = 35 :: Int16
trap_putsp = 36 :: Int16
trap_halt = 37 :: Int16

data Machine = Machine {
  memory :: Seq Int16,
  registers :: Seq Int16,
  currentIO :: IO Int16,
  running :: Bool
  }

--Machine Manipulation

readMem :: Int16 -> State Machine Int16
readMem i = do
  m <- get
  return $ index (memory m) (fromIntegral i)

writeMem :: Int16 -> Int16 -> State Machine ()
writeMem i x = do
  m <- get
  put $ Machine (update (fromIntegral i) x (memory m)) (registers m) (currentIO m) (running m)

readReg :: Int8 -> State Machine Int16
readReg i = do
  m <- get
  return $ index (registers m) (fromIntegral i)

writeReg :: Int8 -> Int16 -> State Machine ()
writeReg i x = do
  if i == r_cond
    then return ()
    else updateFlag x
  m <- get
  put $ Machine (memory m) (update (fromIntegral i) x (registers m)) (currentIO m) (running m)

updateFlag :: Int16 -> State Machine ()
updateFlag n
  | n == 0 = writeReg r_cond fl_zro
  | n > 0 = writeReg r_cond fl_pos
  | otherwise = writeReg r_cond fl_neg

setIO :: IO Int16 -> State Machine ()
setIO io = do
  m <- get
  put $ Machine (memory m) (registers m) io (running m)

setRunning :: Bool -> State Machine ()
setRunning r = do
  m <- get
  put $ Machine (memory m) (registers m) (currentIO m) r

--Utility

signExtend :: Int16 -> Int -> Int16
signExtend n l
  | testBit n (l-1) = n .|. (shift maxBound l)
  | otherwise = n

deconstruct8 :: Int16 -> Int -> Int -> Int8
deconstruct8 n s l = let m = 2^l - 1
  in fromIntegral $ shift n (-s) .&. m

deconstruct16 :: Int16 -> Int -> Int -> Int16
deconstruct16 n s l = let m = 2^l - 1
  in fromIntegral $ shift n (-s) .&. m

deconstructImmediate :: Int16 -> Int -> Int -> Int16
deconstructImmediate n s l = signExtend (deconstruct16 n s l) l

--Initiation

initMachine :: Machine
initMachine = Machine (Sq.replicate 65536 0) (Sq.replicate 16 0) (return (-1)) True

setup :: State Machine ()
setup = do
  writeReg r_pc 12288
  writeReg r_0 20000
  writeMem 20000 65
  writeMem 20001 66
  writeMem 20002 67
  writeMem 12288 (-4062) --puts
  writeMem 12289 (-4062) --puts
  writeMem 12290 (-4064) --get char
  writeMem 12291 (-4059) --halt

--Main

main :: IO ()
main = do
  putStrLn "Start"
  let m = snd $ runState setup initMachine
  m' <- mainLoop m
  let (r0,_) = runState (readReg r_0) m'
  let (cond,_) = runState (readReg r_cond) m'
  putStrLn $ "r0: " ++ (show r0)
  putStrLn $ "cond: " ++ (show cond)
  putStrLn "End"

mainLoop :: Machine -> IO Machine
mainLoop m = do
  let m1 = snd $ runState nextInstr m
  input <- currentIO m1
  let m2 = if input == -1
           then m1
           else snd $ runState (writeReg r_0 input) m1
  let m3 = snd $ runState (setIO (return (-1))) m2
  if running m3
    then mainLoop m3
    else return m3

nextInstr :: State Machine ()
nextInstr = do
  pc <- readReg r_pc
  writeReg r_pc $ pc + 1
  code <- readMem pc
  let opCode = deconstruct8 code 12 4
  readInstr opCode code
    where readInstr x code
            | x == op_br = return ()
            | x == op_add = addInstr code
            | x == op_ld = return ()
            | x == op_st = return ()
            | x == op_jsr = return ()
            | x == op_and = return ()
            | x == op_ldr = return ()
            | x == op_str = return ()
            | x == op_rti = return ()
            | x == op_not = return ()
            | x == op_ldi = loadIndirectInstr code
            | x == op_sti = return ()
            | x == op_jmp = return ()
            | x == op_res = return ()
            | x == op_lea = return ()
            | x == op_trap = trapInstr code

--Instructions
--  Add 0001

addInstr :: Int16 -> State Machine ()
addInstr code = do
  let dr = deconstruct8 code 9 3
  let sr1 = deconstruct8 code 6 3
  let imm = deconstruct8 code 5 1
  if imm == 0
    then let sr2 = deconstruct8 code 0 3
         in addReg dr sr1 sr2
    else let n2 = deconstructImmediate code 0 5
         in addImmediate dr sr1 n2

addReg :: Int8 -> Int8 -> Int8 -> State Machine ()
addReg dr sr1 sr2 = do
  n1 <- readReg sr1
  n2 <- readReg sr2
  writeReg dr $ n1 + n2

addImmediate :: Int8 -> Int8 -> Int16 -> State Machine ()
addImmediate dr sr1 n2 = do
  n1 <- readReg sr1
  writeReg dr $ n1 + n2

--  LDI 1010

loadIndirectInstr :: Int16 -> State Machine ()
loadIndirectInstr code = do
  let dr = deconstruct8 code 9 3
  let offset = deconstructImmediate code 0 9
  addImmediate dr r_pc offset

-- Trap 1111

trapInstr :: Int16 -> State Machine ()
trapInstr code = do
  let trapCode = deconstruct16 code 0 8
  readTrap trapCode
    where readTrap x
            | x == trap_getc = trapGetChar
            | x == trap_out = return ()
            | x == trap_puts = trapPuts
            | x == trap_in = return ()
            | x == trap_putsp = return ()
            | x == trap_halt = trapHalt

trapGetChar :: State Machine ()
trapGetChar = setIO $ fmap (fromIntegral . ord) getChar

trapPuts :: State Machine ()
trapPuts = do
  starting <- readReg r_0
  io <- trapPutsRead starting (return ())
  setIO (io >>= (\_ -> return (-1)))
    where trapPutsRead m io = do
            c <- readMem m
            if c == 0
              then return io
              else trapPutsRead (m+1) (io >>= (\_ -> putChar $ chr (fromIntegral c)))

trapHalt :: State Machine ()
trapHalt = setRunning False
