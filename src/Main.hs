module Main where
import SDL
import qualified Data.Text as Text
import qualified Data.Map as Map

type KeyMap = Map.Map Keycode Bool

{----- Draw Stuf -----}
skyColor = V4 0 185 185 255
groundColor = V4 0 150 0 255
playerColor = V4 255 255 255 255

player = Rectangle (P $ V2 100 420) (V2 50 80)
ground = Rectangle (P $ V2 0 500) (V2 800 100)

render :: Renderer -> IO ()
render renderer = do
    rendererDrawColor renderer $= skyColor
    clear renderer
    rendererDrawColor renderer $= playerColor
    fillRect renderer (Just player)
    rendererDrawColor renderer $= groundColor
    fillRect renderer (Just ground)
    present renderer

{----- SDL Window -----}
windowConfig = defaultWindow {-
    windowMode = FullscreenDesktop
-}

main :: IO ()
main = do
    initializeAll
    window <- createWindow (Text.pack "Haskell Game") windowConfig
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer Map.empty

appLoop :: Renderer -> KeyMap -> IO ()
appLoop renderer input = do
    events <- pollEvents
    let newInput = processInput input events
    render renderer
    if (isKeyDown KeycodeQ newInput) || isQuit events
        then return ()
        else appLoop renderer newInput

{--------------- User Input Stuff -------------}
getKeycode = keysymKeycode . keyboardEventKeysym

-- checks if any events are WindowClosedEvents
isQuit :: [Event] -> Bool
isQuit events = not $ null [ x | x@(Event _ (WindowClosedEvent _)) <- events ]

-- Updates the keymap from the list of events
processInput :: KeyMap -> [Event] -> KeyMap
processInput keys events = Map.union newKeys keys
    where eventPayloads = (map eventPayload events)
          keyboardEvents = [k | KeyboardEvent k <- eventPayloads]
          keyStates = [(getKeycode k, keyboardEventKeyMotion k == Pressed)
                       | k <-keyboardEvents]
          newKeys = Map.fromList keyStates

isKeyDown :: Keycode -> KeyMap -> Bool
isKeyDown code keys = case Map.lookup code keys of
                        Just True -> True
                        _ -> False
