
import System.Directory
import Graphics.Vty
import Data.Default (def)
import Control.Monad.RWS
import Control.Monad

data FMState = FMState FilePath [FilePath]

getPath :: FMState -> FilePath
getPath (FMState path _) = path

type FileManagerAction = RWST Vty () FMState IO

ls :: IO [FilePath]
ls = getCurrentDirectory >>= getDirectoryContents

getDefState :: IO FMState
getDefState = do
    path <- getCurrentDirectory
    return $ FMState path []

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

processEvent :: FileManagerAction Bool
processEvent = do
    k <- ask >>= liftIO . nextEvent
    if k == EvKey KEsc []
        then return True
        else do
            case k of
                -- Add keys here.
                _ -> return False

updateDisplay :: FileManagerAction ()
updateDisplay = do
    let info = string defAttr "Use ESC to exit"
    (w,h) <- asks outputIface >>= liftIO . displayBounds
    path <- gets getPath

    let pathPic = translate 0 1 (string defAttr path)
    let pic = picForLayers $ [info , pathPic]
    vty <- ask
    liftIO $ update vty pic


