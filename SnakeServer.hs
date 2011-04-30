{-# LANGUAGE RecordWildCards,PackageImports,PatternGuards #-}
module Main where

import Network.Socket
import System.Environment
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Random
import "mtl" Control.Monad.State
import Data.List
import Data.Maybe
import Data.Word
import Data.Char

import qualified Graphics.UI.SDL as SDL

import Game
import Common

data Client = Client 
    { cId      :: Maybe Id
    , cName    :: Name
    , cAddress :: SockAddr
    }
   deriving (Show,Eq)

freeId :: [Client] -> Maybe Id
freeId cs = listToMaybe $ [0..24] \\ mapMaybe cId cs

setId :: SockAddr -> Id -> [Client] -> [Client]
setId addr id = alter ((== addr) . cAddress) (\c -> c {cId = Just id})

lookupId :: SockAddr -> [Client] -> Maybe Id
lookupId addr = cId <=< find ((== addr) . cAddress)

isSpectator :: SockAddr -> [Client] -> Bool
isSpectator = (isNothing .) . lookupId

lookupName :: SockAddr -> [Client] -> Name
lookupName addr = cName . fromJust . find ((== addr) . cAddress)

newClient :: Name -> SockAddr -> Client
newClient name  = Client Nothing (filter isAlphaNum name) 

getAvailable :: Chan a -> IO [a]
getAvailable c = do
    b <- isEmptyChan c
    if b then return []
         else liftM2 (:) (readChan c) (getAvailable c)

data Env = Env { chanDead    :: Chan Id
               , chanSpawns  :: Chan (Id,Name)
               , chanDirs    :: Chan (Id,Dir)
               , chanWorld   :: Chan World
               , chanScore   :: Chan [Player]
               , mVarClients :: MVar [Client]
               }

newEnv :: IO Env
newEnv = return Env 
    `ap` newChan 
    `ap` newChan 
    `ap` newChan 
    `ap` newChan 
    `ap` newChan 
    `ap` newMVar []

serve :: String -> IO ()
serve port = withSocketsDo $ do
    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    env <- newEnv
    forkIO $ sendWorld sock env
    forkIO $ sendScore sock env
    forkIO $ procMessages sock env
    evalSnakeM (emptyWorld,[],[]) $ stepGame env

procMessages :: Socket -> Env -> IO ()
procMessages sock env@(Env {..}) = forever $ do
    (msg,_,addr) <- recvFrom sock 1024
    dead <- getAvailable chanDead
    cs <- map (removeDead dead) `fmap` takeMVar mVarClients
    cs' <- processMessage msg addr cs env
    putMVar mVarClients cs'

removeDead :: [Id] -> Client -> Client
removeDead dead c = case cId c of
                        Just i | i `elem` dead -> c { cId = Nothing }
                        _                      -> c

processMessage :: String -> SockAddr -> [Client] -> Env -> IO [Client]
processMessage msg addr cs (Env {..}) = case msg of
    'L':':':name 
        | addr `notElem` map cAddress cs -> return (newClient name addr:cs)
        | otherwise                      -> mystery
    "Play" 
        | isSpectator addr cs, Just n <- freeId cs -> do
            writeChan chanSpawns (n,lookupName addr cs)
            return $ setId addr n cs
    [d] | isDir d, Just n <- lookupId addr cs -> do
            writeChan chanDirs (n,charToDir d)
            return cs
    "Ping" -> return cs
    "Exit" -> return $ filter ((/= addr) . cAddress) cs
    _      -> mystery
  where
    mystery = do putStrLn $ "Mystery message: " ++ msg ++ " from " ++ show addr
                 return cs

stepGame :: Env -> SnakeM ()
stepGame (Env {..}) = every 100 $ do
    spawns          <- liftIO $ getAvailable chanSpawns
    dirs            <- liftIO $ getAvailable chanDirs
    (dead,upd)      <- stepWorld spawns dirs
    (world,score,_) <- get
    liftIO $ do 
        writeList2Chan chanDead dead
        writeChan chanWorld world
        when upd $ writeChan chanScore score

sendWorld :: Socket -> Env -> IO ()
sendWorld sock (Env {..}) = forever $ do
    world <- readChan chanWorld
    let wstr = pkgWorld world
    cs <- readMVar mVarClients
    mapM_ (\c -> sendTo sock ('W':pref (cId c):wstr) (cAddress c)) cs
  where
    pref (Just x) = chr x
    pref Nothing  = chr 255

sendScore :: Socket -> Env -> IO ()
sendScore sock (Env {..}) = forever $ do
    score <- readChan chanScore
    let sstr = 'S':pkgScores score
    cs <- readMVar mVarClients
    mapM_ (sendTo sock sstr . cAddress) cs
  where

every :: MonadIO m => Word32 -> m a -> m b
every d m = forever $ do
    t0 <- liftIO SDL.getTicks
    liftIO $ print t0
    m
    t <- liftIO SDL.getTicks
    liftIO $ threadDelay $ fromEnum ((d - (t - t0)) * 1000)

main :: IO ()
main = do
    [port] <- getArgs
    SDL.init [SDL.InitTimer]
    serve port
