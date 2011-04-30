{-# LANGUAGE FlexibleInstances,TupleSections,RecordWildCards #-}
module Common where

import Data.Binary
import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Internal (c2w,w2c)

import Control.Arrow
import Control.Monad

import Test.QuickCheck
import Graphics.Rendering.OpenGL.GL (GLdouble)

import System.IO.Unsafe

debug :: Monad m => String -> m ()
debug s = unsafePerformIO $ do
    putStrLn s
    return $ return ()

tod :: Int -> GLdouble
tod = realToFrac

alter :: (a -> Bool) -> (a -> a) -> [a] -> [a]
alter p f xs | any p xs = let (l,h:r) = break p xs
                          in  l ++ f h : r
             | otherwise = xs

retry :: Monad m => m (Bool,a) -> m a
retry m = do
    (c,v) <- m
    if c then return v else retry m

type Pos = (Int,Int)

instance Num a => Num (a,a) where
    (+) = liftT2 (+) 
    (-) = liftT2 (-)
    (*) = liftT2 (*)
    negate = liftT negate
    abs    = liftT abs
    signum = liftT signum
    fromInteger = undefined

liftT :: (a -> b) -> (a,a) -> (b,b)
liftT f (x,y) = (f x,f y)

liftT2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
liftT2 f (x1,y1) (x2,y2) = (f x1 x2,f y1 y2)

type Name = String

data Dir = U | D | L | R
    deriving (Eq,Ord,Show,Enum)

dirToPos :: Dir -> Pos
dirToPos U = (0,-1)
dirToPos D = (0,1)
dirToPos L = (-1,0)
dirToPos R = (1,0)

posToDir :: Pos -> Maybe Dir
posToDir = flip lookup $ map (dirToPos &&& Prelude.id) [U,D,L,R]

addDir :: Pos -> Dir -> Pos
addDir p d = p + dirToPos d

charToDir 'U' = U
charToDir 'D' = D
charToDir 'L' = L
charToDir 'R' = R
charToDir x   = error $ "charToDir: on " ++ show x

isDir :: Char -> Bool
isDir = (`elem` "UDLR")

opposite :: Dir -> Dir -> Bool
opposite d d' = dirToPos d `addDir` d' == (0,0)

opposing :: Dir -> Dir 
opposing U = D
opposing D = U
opposing L = R
opposing R = L

type Id = Int

data Snake = 
    Snake { snakeId :: Id
          , body    :: [Pos]
          }
  deriving (Eq,Show)

data Player =
    Player { playerId :: Id
           , score    :: Int
           , name     :: Name
           }
  deriving (Eq,Show)

data World = 
    World { width  :: Int
          , height :: Int
          , bounty :: Pos
          , snakes :: [Snake]
          }
  deriving (Eq,Show)

bodyFromDirs :: Pos -> [Dir] -> [Pos]
bodyFromDirs = scanl addDir 

dirsFromBody :: [Pos] -> (Pos,[Dir])
dirsFromBody body = (head body,unfoldr go body) 
    where go (x:x':xs) = (,x':xs) `fmap` posToDir (x' - x)
          go _         = Nothing

emptyWorld :: World
emptyWorld = World 60 60 (30,30) []

-- Packaging

put8 :: Int -> Put
put8 = putWord8 . toEnum

get8 :: Get Int
get8 = fromEnum `liftM` getWord8

instance Binary Snake where
    put (Snake {..}) = do put8 snakeId
                          let ((x,y),dirs) = dirsFromBody body
                          put8 x 
                          put8 y
                          put (genericLength dirs :: Word16)
                          putDirs dirs
      where
        putDirs :: [Dir] -> Put
        putDirs []   = return ()
        putDirs dirs = do
            let (ds,r) = splitAt 4 dirs
                muls = iterate (*4) 1
                word = sum $ zipWith (*) (map valueDir ds) muls
            putWord8 word 
            putDirs r
    
    get = do sid <- get8
             x <- get8
             y <- get8
             l <- get :: Get Word16
             dirs <- getDirs (fromEnum l)
             return $ Snake sid $ bodyFromDirs (x,y) dirs
      where
        getDirs :: Int -> Get [Dir]
        getDirs n 
            | n <= 0 = return []
            | otherwise = do
                w <- getWord8
                let bit x | testBit w x = 1
                          | otherwise   = 0
                    dirs = map (\x -> dirValue $ bit x + 2 * bit (x + 1)) [0,2,4,6]
                (take n dirs ++) `liftM` getDirs (n - 4)

instance Binary Player where
    put (Player {..}) = do put8 playerId
                           put score
                           put name

    get = do pid <- get8
             score <- get
             name <- get
             return $ Player pid score name

instance Binary World where
    put (World {..}) = do put8 width
                          put8 height
                          let (bx,by) = bounty
                          put8 bx
                          put8 by
                          put snakes

    get = do width <- get8
             height <- get8
             bx <- get8
             by <- get8
             snakes <- get
             return $ World width height (bx,by) snakes

valueDir :: Dir -> Word8
valueDir U = 0
valueDir D = 1
valueDir L = 2
valueDir R = 3

dirValue :: Word8 -> Dir
dirValue 0 = U
dirValue 1 = D
dirValue 2 = L
dirValue 3 = R

pkgWorld :: World -> String
pkgWorld = map w2c . B.unpack . encode

unPkgWorld :: String -> World
unPkgWorld = decode . B.pack . map c2w

pkgScores :: [Player] -> String
pkgScores = map w2c . B.unpack . encode

unPkgScores :: String -> [Player]
unPkgScores = decode . B.pack . map c2w

-- Testing

instance Arbitrary Dir where
    arbitrary = elements [U,D,L,R]

instance Arbitrary Snake where
    arbitrary = liftM2 Snake arbId arbPosList

instance Arbitrary Player where
    arbitrary = liftM3 Player arbId arbitrary arbName

instance Arbitrary World where
    arbitrary = do
        w <- choose (10,255)
        h <- choose (10,255)
        b <- arbPos
        n <- choose (0,24)
        s <- forM [0..n] (\i -> Snake i `liftM` arbPosList)
        return $ World w h b s

prop_dirBody :: Pos -> [Dir] -> Bool
prop_dirBody p d = (p,d) == dirsFromBody (bodyFromDirs p d)

arbId :: Gen Id
arbId = choose (0,24)

arbPos :: Gen Pos
arbPos = do
    let p = choose (20,230)
    liftM2 (,) p p

arbName :: Gen Name
arbName = choose (0,10) >>= flip replicateM (elements ['a'..'z'])

arbPosList :: Gen [Pos]
arbPosList = do
    dirs <- arbitrary
    pos <- arbPos
    return $ go pos dirs
  where
    go p (d:ds) = p : go (p `addDir` d) ds
    go p []     = p : []

prop_bodyDir :: Property
prop_bodyDir = forAll arbPosList $ \ps -> ps == uncurry bodyFromDirs (dirsFromBody ps)

prop_binarySnake :: Snake -> Bool
prop_binarySnake s = decode (encode s) == s

prop_binaryPlayer :: Player -> Bool
prop_binaryPlayer s = decode (encode s) == s

prop_binaryWorld :: World -> Bool
prop_binaryWorld s = decode (encode s) == s
