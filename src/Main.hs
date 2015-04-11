
import System.Directory
import Graphics.Vty
import Data.Default (def)
import Control.Monad.RWS
import Control.Monad
import Data.List (intercalate)
import Data.List.PointedList hiding (length)

-- FileManagerState CurrentDirectory (Contents with cursor)
data FMState = FMState FilePath (PointedList FilePath)

pwd :: FMState -> FilePath
pwd (FMState path _) = path

ls :: FMState -> PointedList FilePath
ls (FMState _ content) = content

type FileManagerAction = RWST Vty () FMState IO

getDefState :: IO FMState
getDefState = do
    path <- getCurrentDirectory
    content <- getCurrentDirectory >>= getDirectoryContents
    case fromList content of
        Nothing -> return $ FMState path (PointedList [] "." [".."])
        Just p  -> return $ FMState path p

main :: IO ()
main = do
    vty <- mkVty def
    state <- getDefState
    (_finalState, ()) <- execRWST update' vty state
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

processEvent :: FileManagerAction Bool
processEvent = do
    k <- ask >>= liftIO . nextEvent
    if k == EvKey KEsc []
        then return True
        else do
            case k of
                -- Add keys here.
                EvKey KDown [] -> modify goDown
                EvKey KUp   [] -> modify goUp
                _ -> return ()
            return False


drawDir :: PointedList FilePath -> [Image]
drawDir (PointedList l p r) = left ++ [drawCursor] ++ right
    where drawCursor = translate 1 (length l) $ string (defAttr `withBackColor` blue) p
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


