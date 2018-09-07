-- Brainfuck language interpreter
-- author Konrad Kania (plasma0)
-- license: as-is
import Data.Char
import System.IO
import System.Environment(getArgs)

insertat :: Int -> a -> [a] -> [a]
insertat p e lst = take p lst ++ [e] ++ drop (p+1) lst

getc :: IO Int
getc = do
    lin <- getLine
    return $ ord (head lin)

findendloop :: Int -> Int -> String -> Int
findendloop p s cs | s == 0 = p
                   | cs!!p == '[' = findendloop (p+1) (s+1) cs
                   | cs!!p == ']' = findendloop (p+1) (s-1) cs
                   | otherwise   = findendloop (p+1) s cs


type Registers = [Int]
type Pointer = Int

type Orders = [Char]
type LoopStack = [Int]
type Order = Int
type OrderLength = Int

type MachineState = (Registers,Pointer,Orders,Order,LoopStack,OrderLength)

machineinit :: String -> MachineState
machineinit cs = (repeat 0, 0, cs, 0, [], length cs)

machineexecute :: MachineState -> IO()
machineexecute (rs, p, cs, o, s, l) | o == l = putStrLn "\nBrainhuck:PROGRAM HAS ENDED"
                                      | otherwise = do
                                          case cs!!o of '>' -> machineexecute (rs, p+1, cs, o+1, s, l)
                                                        '<' -> machineexecute (rs, p-1, cs, o+1, s, l)
                                                        '+' -> machineexecute (insertat p ((rs!!p)+1) rs , p, cs, o+1, s, l)
                                                        '-' -> machineexecute (insertat p ((rs!!p)-1) rs , p, cs, o+1, s, l)
                                                        '.' -> do
                                                            putChar (chr  (rs!!p))
                                                            machineexecute (rs, p, cs, o+1, s, l)
                                                        ',' -> do
                                                            gotc <- getc
                                                            machineexecute (insertat p gotc rs , p, cs, o+1, s, l)
                                                        '[' | rs!!p == 0 -> machineexecute (rs, p, cs, (findendloop (o+1) 1 cs), s, l)
                                                        '[' | rs!!p /= 0 -> machineexecute (rs, p, cs, o+1, [o] ++ s, l)
                                                        ']' -> machineexecute (rs, p, cs, head s, tail s, l)
                                                        _   -> machineexecute (rs, p, cs, o+1, s, l)

main = do
    args <- getArgs
    code <- openFile (head $ args) ReadMode
    orders <- hGetContents code
    machine <- return $ machineinit orders
    machineexecute machine