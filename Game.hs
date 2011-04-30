{-# LANGUAGE RecordWildCards,PackageImports,FlexibleInstances #-}
module Game where

import Control.Monad.Random
import "mtl" Control.Monad.State
import Control.Monad
import Data.Either
import Data.Maybe
import Control.Arrow

import Common

-- IO is an instance of MonadRandom

type SnakeM = StateT SnakeState IO

evalSnakeM :: SnakeState -> SnakeM a -> IO a 
evalSnakeM s0 m = evalStateT m s0 

type SnakeState = (World,[Player],Dirs)

modWorld :: (World -> World) -> SnakeState -> SnakeState
modWorld f (w,p,d) = (f w,p,d)

modScore :: ([Player] -> [Player]) -> SnakeState -> SnakeState
modScore f (w,p,d) = (w,f p,d)

modDirs :: (Dirs -> Dirs) -> SnakeState -> SnakeState
modDirs f (w,p,d) = (w,p,f d)

incrScore :: Id -> SnakeM ()
incrScore sid = modify $ modScore $ alter ((== sid) . playerId) $ \p -> p { score = score p + 1 }

newPlayer :: (Id,Name) -> SnakeM ()
newPlayer (pid,name) = modify $ modScore (Player pid 0 name:)

type Dirs   = [(Id,Dir)]
type Spawns = [(Id,Name)]
type Dead   = [Id]

getRandomPos :: Int -> Int -> SnakeM Pos
getRandomPos w h = do
    x <- lift $ getRandomR (1,w-2)
    y <- lift $ getRandomR (1,h-2)
    return (x,y)

getRandomDir :: SnakeM Dir
getRandomDir = do
    d <- lift $ getRandomR (0,3)
    return $ [U,D,L,R] !! d

setDir :: Id -> Dir -> Dirs -> Dirs
setDir i d = alter ((== i) . fst) $ \(_,d') -> if d `opposite` d' 
                                                    then (i,d')
                                                    else (i,d)

stepWorld :: Spawns -> Dirs -> SnakeM (Dead,Bool)
stepWorld spawns newdirs = do
    (World {..},players,dirs) <- get
    let m = (width,height)
        dirs' = foldr (uncurry setDir) dirs newdirs
    modify (modDirs (const dirs'))
    snakes' <- mapM stepSnake snakes
    let pickup  = bounty `elem` map (head . body) snakes'
        ps = concatMap body snakes'
        (dead,snakes'') = partitionEithers $ map (handleDead m ps) snakes'
        dirs'' = filter ((`notElem` dead) . fst) dirs'
    modify $ modScore $ filter ((`notElem` dead) . playerId)
    bounty' <- if pickup then retry $ do p <- getRandomPos width height
                                         return (p `notElem` ps, p)
                         else return bounty
    (newSnakes,newDirs) <- flip mapAndUnzipM spawns $ \(i,n) -> retry $ do
        p <- getRandomPos width height
        d <- getRandomDir
        return (spawnOk m p d ps && p /= bounty',(Snake i (grow [p] 3),(i,d)))
    mapM_ newPlayer spawns
    modify $ modWorld $ const $ World width height bounty' (snakes'' ++ newSnakes)
    modify $ modDirs $ const $ dirs' ++ newDirs
    return (dead,pickup || not (null newSnakes) || not (null dead))
  where
    handleDead :: Pos -> [Pos] -> Snake -> Either Id Snake
    handleDead m ss s@(Snake {..}) 
        | inside m (head body) && length (filter (== head body) ss) <= 1 = Right s
        | otherwise                                                      = Left snakeId


    spawnOk :: Pos -> Pos -> Dir -> [Pos] -> Bool
    spawnOk m p d ps = all (\x -> x `notElem` ps && inside m x) 
                             (bodyFromDirs p (replicate 5 d))

inside :: Pos -> Pos -> Bool
inside (width,height) (x,y) = x > 0 && x < width - 1 && y > 0 && y < height - 1

stepSnake :: Snake -> SnakeM Snake
stepSnake (Snake {..}) = do
    (World {..},_,dirs) <- get
    let body'  = advance body (fromMaybe (error "stepSnake; lookup dir") (lookup snakeId dirs))
        pickup = head body' == bounty
        body'' | pickup    = grow body' 4
               | otherwise = body'
    when pickup $ incrScore snakeId
    return $ Snake snakeId body''


advance :: [Pos] -> Dir -> [Pos]
advance s d = head s + dirToPos d : init s

grow :: [Pos] -> Int -> [Pos]
grow s n = s ++ replicate n (last s)

