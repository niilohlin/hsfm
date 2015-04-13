{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import Graphics.Vty
import Data.Default (def)
import Control.Monad.RWS
import Data.List.PointedList hiding (length)
import Data.List (sort)
import qualified Data.Map as M

type KeyMap = M.Map Event (FileManagerAction ())
-- FileManagerState CurrentDirectory (Contents with cursor)
data FMState = FMState FilePath (PointedList FilePath)

pwd :: FMState -> FilePath
pwd (FMState path _) = path

ls :: FMState -> PointedList FilePath
ls (FMState _ content) = content

type FileManagerAction = RWST Vty () FMState IO

updateState :: IO FMState
updateState = do
    path <- getCurrentDirectory
    content <- getCurrentDirectory >>= getDirectoryContents
    let sortedContent = sort content
    case fromList sortedContent of
        Nothing -> return $ FMState path (PointedList [] "." [".."])
        Just p  -> return $ FMState path p

main :: IO ()
main = do
    vty <- mkVty def
    state' <- updateState
    (_finalState, ()) <- execRWST update' vty state'
    shutdown vty

update' :: FileManagerAction ()
update' = do
    updateDisplay
    done <- processEvent
    unless done update'

moveCursor
  :: (PointedList FilePath -> Maybe (PointedList FilePath))
     -> FMState -> FMState
moveCursor moveFn (FMState path dir) = case moveFn dir of
    Nothing -> FMState path dir
    Just dir' -> FMState path dir'

goDown :: FMState -> FMState
goDown = moveCursor next

goUp :: FMState -> FMState
goUp = moveCursor previous

moveToDir :: FileManagerAction ()
moveToDir  = do
    (FMState path (PointedList _ dir _)) <- get
    let newPath = path ++ "/" ++ dir
    isDir <- liftIO $ doesDirectoryExist newPath
    when isDir $ do
        liftIO $ setCurrentDirectory newPath
        liftIO updateState >>= put

movePrevious :: FileManagerAction ()
movePrevious = do
    (FMState path _) <- get
    liftIO $ setCurrentDirectory $ path ++ "/.."
    liftIO updateState >>= put




defKeyMap :: KeyMap
defKeyMap = M.fromList [
      (EvKey KDown [], modify goDown)
    , (EvKey KUp   [], modify goUp)
    , (EvKey (KChar 'j') [],  modify goDown)
    , (EvKey (KChar 'k') [],  modify goUp)
    , (EvKey KEnter [], moveToDir)
    , (EvKey KRight [], moveToDir)
    , (EvKey KLeft  [], movePrevious)
    ]

processEvent :: FileManagerAction Bool
processEvent = do
    k <- ask >>= liftIO . nextEvent
    if k == EvKey KEsc []
        then return True
        else do
            case M.lookup k defKeyMap of
                Nothing -> return ()
                Just action -> action
            return False


drawDir :: PointedList FilePath -> [Image]
drawDir (PointedList l p r) = left ++ [drawCursor] ++ right
    where drawCursor = translate 1 (length l) $ string (defAttr `withForeColor` blue) p
          left = map (\(y, fpath) -> translate 1 y (string defAttr fpath)) (zip [0..] (reverse l))
          right = map (\(y, fpath) -> translate 1 (y + length l + 1) (string defAttr fpath)) (zip [0..] r)

drawFMState :: FMState -> Picture
drawFMState (FMState path dir) = picForLayers $ (translate 0 0 (string defAttr path)) : (map (translate 0 1) (drawDir dir))

updateDisplay :: FileManagerAction ()
updateDisplay = do
    -- (w,h) <- asks outputIface >>= liftIO . displayBounds

    fmstate <- get
    vty <- ask

    let pic = drawFMState fmstate

    liftIO $ update vty pic


