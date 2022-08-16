
import Lib (setCursor, getHiddenChar, printChar)
import System.Random

data Game = Game { playerX :: Int
                 , playerY :: Int
                 , playerTrack :: [Char]
                 , candyX :: Int
                 , candyY :: Int
                 , playerScore :: Int
                 , randomGen :: StdGen } deriving (Show)

main :: IO ()
main = do
  setCursor 0 0
  printFieldSize1020
  setCursor 1 1
  printChar '\3'
  gen <- getStdGen
  setCursor 0 15
  createNewCandyCords gen
  (xCandy, yCandy, genCandy) <- createNewCandyCords gen
  let game = Game 1 1 ['n'] xCandy yCandy 0 genCandy
  going game
   where 
   going g = do
     setCursor 0 12
     symbol <- getHiddenChar
     case symbol of
        'w' -> if (playerY g - 1) > 0 && 's' /= head (playerTrack g) && 'S' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerY = playerY g - 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'W' -> if (playerY g - 1) > 0 && 's' /= head (playerTrack g) && 'S' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerY = playerY g - 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        's' -> if (playerY g + 1) < 11 && 'w' /= head (playerTrack g) && 'W' /= head (playerTrack g) then do 
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerY = playerY g + 1,playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'S' -> if (playerY g + 1) < 11 && 'w' /= head (playerTrack g) && 'W' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerY = playerY g + 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'a' -> if (playerX g - 1) > 0 && 'd' /= head (playerTrack g) && 'D' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerX = playerX g - 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'A' -> if (playerX g - 1) > 0 && 'd' /= head (playerTrack g) && 'D' /= head (playerTrack g) then do 
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerX = playerX g - 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'd' -> if (playerX g + 1) < 19 && 'a' /= head (playerTrack g) && 'A' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerX = playerX g + 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        'D' -> if (playerX g + 1) < 19 && 'a' /= head (playerTrack g) && 'A' /= head (playerTrack g) then do
                                 movePlayer (playerX g) (playerY g) symbol
                                 newG <- eatingCandy (g {playerX = playerX g + 1, playerTrack = symbol : (playerTrack g)})
                                 deleteTail $ findTail (playerX g, playerY g) (tail $ take (playerScore newG + 1) (playerTrack newG))
                                 going newG
                            else going g
        _   -> putStrLn "not WASD   " >> going g

eatingCandy g | (playerX g, playerY g) == (candyX g, candyY g) = do
                                                         plusCandy (playerScore g + 1)
                                                         (newXCandy, newYCandy, newGenCandy) <- createNewCandyCords (randomGen g) 
                                                         pure (g {candyX = newXCandy, candyY = newYCandy, playerScore = playerScore g + 1, randomGen = newGenCandy})
              | otherwise = do
                           plusCandy (playerScore g)
                           pure g

plusCandy score = setCursor 21 1 >> (putStrLn $ "Number of eaten candies: " ++ show score)

findTail :: (Int,Int) -> [Char] -> (Int,Int)
findTail (x,y) [] = (x,y)
findTail (x,y) (symb:symbs) | symb == 'w' || symb == 'W' = findTail (x, y+1) symbs
                            | symb == 's' || symb == 'S' = findTail (x, y-1) symbs
                            | symb == 'a' || symb == 'A' = findTail (x+1, y) symbs
                            | symb == 'd' || symb == 'D' = findTail (x-1, y) symbs
                            | otherwise = findTail (100, 100) symbs

deleteTail (x,y) = setCursor x y >> printChar ' '
 
movePlayer x y symb | symb == 'w' || symb == 'W' = putStrLn "Up       " >> printPlayer x (y-1)
                    | symb == 's' || symb == 'S' = putStrLn "Down     " >> printPlayer x (y+1)
                    | symb == 'a' || symb == 'A' = putStrLn "Left     " >> printPlayer (x-1) y
                    | otherwise = putStrLn "Right    " >> printPlayer (x+1) y

deletePlayer x y = setCursor x y >> printChar ' '

printPlayer x y = setCursor x y >> printChar '\3'

createNewCandyCords gen = do
      let (x, newGen) = randomR (1,18) gen
      let (y, newGen') = randomR (1,10) newGen
      setCursor x y
      printChar '\15'
      pure (x,y, newGen')

field row column = replicate' column '#' : replicate' row ("#" ++ replicate' (column - 2) ' ' ++ "#")
                ++ [replicate' column '#']

replicate' n x
     | n == 0 = []
     | otherwise = x:replicate' (n-1) x

printField :: [String] -> IO ()
printField = mapM_ putStrLn

printFieldSize3 = printField (field 3 7)

printFieldSize20 = printField (field 20 20)

printFieldSize1020 = printField (field 10 20)
