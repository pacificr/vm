
import Control.Monad.State
import Data.Int
import Data.Bits
import Data.Char
import qualified Data.Sequence as Sq
import System.Environment
import System.IO

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

trap_getc = 32
trap_out = 33
trap_puts = 34
trap_in = 35
trap_putsp = 36
trap_halt = 37

data Machine = Machine {
  memory :: Sq.Seq Int16,
  registers :: Sq.Seq Int16,
  running :: Bool
  }

type M = StateT Machine IO

--Machine Manipulation

readMem :: Int16 -> M Int16
readMem i = do
  m <- get
  return $ Sq.index (memory m) (fromIntegral i)

writeMem :: Int16 -> Int16 -> M ()
writeMem i x = do
  m <- get
  put $ Machine (Sq.update (fromIntegral i) x (memory m)) (registers m) (running m)

readReg :: Int8 -> M Int16
readReg i = do
  m <- get
  return $ Sq.index (registers m) (fromIntegral i)

writeReg :: Int8 -> Int16 -> M ()
writeReg i x = do
  if (i == r_cond) || (i == r_pc)
    then return ()
    else updateFlag x
  m <- get
  put $ Machine (memory m) (Sq.update (fromIntegral i) x (registers m)) (running m)

updateFlag :: Int16 -> M ()
updateFlag n
  | n == 0 = writeReg r_cond fl_zro
  | n > 0 = writeReg r_cond fl_pos
  | otherwise = writeReg r_cond fl_neg

setRunning :: Bool -> M ()
setRunning r = do
  m <- get
  put $ Machine (memory m) (registers m) r

getRunning :: M Bool
getRunning = do
  m <- get
  return $ running m

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
initMachine = Machine (Sq.replicate 65536 0) (Sq.replicate 16 0) True

setup :: M ()
setup = do
  writeReg r_pc 12288
  writeReg r_0 20000
  writeMem 20000 16706
  writeMem 20001 16706
  writeMem 20002 67
  writeMem 12289 (-4060) --output string
  writeMem 12290 (-4064) --get char
  writeMem 12291 (-4063) --put char
  writeMem 12292 (-4064)
  writeMem 12293 (-4061) --prompt
  writeMem 12294 (-4059) --halt

--Main

main :: IO ()
main = do
  fmap fst $ runStateT (setup >> mainLoop) initMachine

mainLoop :: M ()
mainLoop = do
  pc <- readReg r_pc
  writeReg r_pc $ pc + 1
  code <- readMem pc
  let opCode = deconstruct8 code 12 4
  readInstr opCode code
  running <- getRunning
  if running
    then mainLoop
    else return ()

readInstr :: Int8 -> Int16 -> M ()
readInstr x
  | x == op_br = branchInstr
  | x == op_add = addInstr
  | x == op_ld = loadInstr
  | x == op_st = storeInstr
  | x == op_jsr = jumpRegInstr
  | x == op_and = andInstr
  | x == op_ldr = loadRegInstr
  | x == op_str = storeRegInstr
  | x == op_rti = retFromInterruptInstr
  | x == op_not = notInstr
  | x == op_ldi = loadIndirectInstr
  | x == op_sti = storeIndirectInstr
  | x == op_jmp = jumpInstr
  | x == op_res = reservedInstr
  | x == op_lea = loadEffectiveAddrInstr
  | x == op_trap = trapInstr

--Instructions
-- Branch 0000

branchInstr :: Int16 -> M ()
branchInstr code = do
  let offset = deconstructImmediate code 0 9
  let cond_flag = deconstructImmediate code 9 3
  cond_flag_reg <- readReg r_cond
  pc <- readReg r_pc
  if cond_flag == cond_flag_reg
    then writeReg r_pc (pc + offset)
    else return ()

--  Add 0001

addInstr :: Int16 -> M ()
addInstr code = do
  let dr = deconstruct8 code 9 3
  let sr1 = deconstruct8 code 6 3
  let imm = deconstruct8 code 5 1
  if imm == 0
    then let sr2 = deconstruct8 code 0 3
         in addReg dr sr1 sr2
    else let n2 = deconstructImmediate code 0 5
         in addImmediate dr sr1 n2

addReg :: Int8 -> Int8 -> Int8 -> M ()
addReg dr sr1 sr2 = do
  n1 <- readReg sr1
  n2 <- readReg sr2
  writeReg dr $ n1 + n2

addImmediate :: Int8 -> Int8 -> Int16 -> M ()
addImmediate dr sr1 n2 = do
  n1 <- readReg sr1
  writeReg dr $ n1 + n2

-- Load 0010

loadInstr :: Int16 -> M ()
loadInstr code = do
  let offset = deconstructImmediate code 0 9
  let dr = deconstruct8 code 9 3
  pc <- readReg r_pc
  writeReg r_pc $ pc + offset

-- Store 0011

storeInstr :: Int16 -> M ()
storeInstr code = do
  let offset = deconstructImmediate code 0 9
  let sr = deconstruct16 code 9 3
  pc <- readReg r_pc
  writeMem (pc + offset) sr

-- Jump Register 0100

jumpRegInstr :: Int16 -> M ()
jumpRegInstr code = do
  let long_flag = deconstruct8 code 11 1
  let r1 = deconstruct8 code 6 3
  let long_offset = deconstructImmediate code 0 9
  pc <- readReg r_pc
  writeReg r_7 pc
  if long_flag == 1
    then writeReg r_pc long_offset
    else do newPc <- readReg r1
            writeReg r_pc newPc

-- And 0101

andInstr :: Int16 -> M ()
andInstr code = do
  let dr = deconstruct8 code 9 3
  let sr1 = deconstruct8 code 6 3
  let imm = deconstruct8 code 5 1
  if imm == 0
    then let sr2 = deconstruct8 code 0 3
         in addReg dr sr1 sr2
    else let n2 = deconstructImmediate code 0 5
         in addImmediate dr sr1 n2

andReg :: Int8 -> Int8 -> Int8 -> M ()
andReg dr sr1 sr2 = do
  n1 <- readReg sr1
  n2 <- readReg sr2
  writeReg dr $ n1 .&. n2

andImmediate :: Int8 -> Int8 -> Int16 -> M ()
andImmediate dr sr1 n2 = do
  n1 <- readReg sr1
  writeReg dr $ n1 .&. n2

-- Load Register 0110

loadRegInstr :: Int16 -> M ()
loadRegInstr code = do
  let offset = deconstructImmediate code 0 6
  let br = deconstruct8 code 6 3
  let dr = deconstruct8 code 9 3
  v <- readReg br
  writeReg dr $ v + offset

-- Store Register 0111

storeRegInstr :: Int16 -> M ()
storeRegInstr code = do
  let offset = deconstructImmediate code 0 6
  let br = deconstruct8 code 6 3
  let sr = deconstruct8 code 9 3
  br_v <- readReg br
  sr_v <- readReg sr
  writeMem (br_v + offset) sr_v

-- Return from Interrupt 1000

retFromInterruptInstr :: Int16 -> M ()
retFromInterruptInstr code = do
  liftIO $ putStrLn "\n--- Bad Op Code (rti)---\n"
  setRunning False

-- Not 1001

notInstr :: Int16 -> M ()
notInstr code = do
  let sr = deconstruct8 code 6 3
  let dr = deconstruct8 code 9 3
  v <- readReg sr
  writeReg dr $ complement v

-- LDI 1010

loadIndirectInstr :: Int16 -> M ()
loadIndirectInstr code = do
  let dr = deconstruct8 code 9 3
  let offset = deconstructImmediate code 0 9
  addImmediate dr r_pc offset

-- Store Indirect 1011

storeIndirectInstr :: Int16 -> M ()
storeIndirectInstr code = do
  let offset = deconstructImmediate code 0 9
  let sr = deconstruct8 code 9 3
  pc <- readReg r_pc
  v <- readReg sr
  writeMem (pc + offset) v

-- Jump 1100

jumpInstr :: Int16 -> M ()
jumpInstr code = do
  let br = deconstruct8 code 6 3
  v <- readReg br
  writeReg r_pc v

-- Reserved 1101

reservedInstr :: Int16 -> M ()
reservedInstr code = do
  liftIO $ putStrLn "\n--- Bad Op Code (res)---\n"
  setRunning False

-- Load Effective Address 1110

loadEffectiveAddrInstr :: Int16 -> M ()
loadEffectiveAddrInstr code = do
  let offset = deconstructImmediate code 0 9
  let dr = deconstruct8 code 9 3
  pc <- readReg r_pc
  writeReg dr $ pc + offset

-- Trap 1111

trapInstr :: Int16 -> M ()
trapInstr code = readTrapInstr $ deconstruct8 code 0 8

readTrapInstr :: Int8 -> M ()
readTrapInstr x
  | x == trap_getc = trapGetChar
  | x == trap_out = trapOut
  | x == trap_puts = trapPuts
  | x == trap_in = trapPromptGetChar
  | x == trap_putsp = trapOutputStr
  | x == trap_halt = trapHalt

trapGetChar :: M ()
trapGetChar = do
  c <- liftIO $ fmap (fromIntegral . ord) getChar
  writeReg r_0 c

trapOut :: M ()
trapOut = do
  c <- readReg r_0
  liftIO $ putChar $ chr $ fromIntegral c

trapPuts :: M ()
trapPuts = do
  starting <- readReg r_0
  trapPutsRead starting
    where trapPutsRead loc = do
            c <- readMem loc
            case c of
              0 -> return ()
              _ -> do
                liftIO $ putChar $ chr $ fromIntegral c
                trapPutsRead (loc+1)

trapPromptGetChar :: M ()
trapPromptGetChar = do
  liftIO $ putStr "Enter a character: "
  c <- liftIO getChar
  liftIO $ putChar c
  writeReg r_0 $ fromIntegral $ ord c

trapOutputStr :: M ()
trapOutputStr = do
  starting <- readReg r_0
  trapOutputStrRead starting
    where trapOutputStrRead loc = do
            cs <- readMem loc
            case cs of
              0 -> return ()
              _ -> do
                let first = deconstruct8 cs 0 8
                liftIO $ putChar $ chr $ fromIntegral first
                let second = deconstruct8 cs 8 8
                case second of
                  0 -> trapOutputStrRead (loc+1)
                  _ -> do
                    liftIO $ putChar $ chr $ fromIntegral second
                    trapOutputStrRead (loc+1)

trapHalt :: M ()
trapHalt = do
  liftIO $ putStrLn "HALT"
  setRunning False
