import Control.Monad (when)
import qualified Data.Map as Map

-- Define the Game State
data GameState = GameState
    { currentRoom :: String
    , inventory :: [String]
    }

-- Define the Room structure
data Room = Room
    { description :: String
    , exits :: Map.Map String String
    , items :: [String]
    } deriving (Show)

-- Define the rooms
rooms :: Map.Map String Room
rooms = Map.fromList
    [ ("Entrance", Room "You are at the entrance of the cave. There are doors to the north." (Map.fromList [("north", "Hallway")]) [])
    , ("Hallway", Room "You are in a long hallway. There are doors to the south and east." (Map.fromList [("south", "Entrance"), ("east", "Treasure Room")]) [])
    , ("Treasure Room", Room "You've entered a room filled with treasure!" (Map.fromList [("west", "Hallway")]) ["gold", "silver"])
    ]

-- Display the current room description and items
displayRoom :: GameState -> IO ()
displayRoom state = do
    let room = Map.lookup (currentRoom state) rooms
    case room of
        Just r -> do
            putStrLn (description r)
            when (not . null $ items r) $ do
                putStrLn "You see the following items:"
                mapM_ putStrLn (items r)
        Nothing -> putStrLn "You are lost!"

-- Display help information
displayHelp :: IO ()
displayHelp = do
    putStrLn "Available commands:"
    putStrLn "  look         - Look around the current room."
    putStrLn "  go <direction> - Move in a specified direction (north, south, east, west)."
    putStrLn "  take <item>  - Take an item from the room."
    putStrLn "  inventory    - Check your current inventory."
    putStrLn "  help         - Show this help message."
    putStrLn "  quit         - Exit the game."

-- Handle player commands
handleCommand :: String -> GameState -> IO GameState
handleCommand cmd state
    | cmd == "look" = do
        displayRoom state
        return state
    | cmd == "inventory" = do
        putStrLn "Your inventory contains:"
        mapM_ putStrLn (inventory state)
        return state
    | cmd == "help" = do
        displayHelp
        return state
    | cmd == "quit" = do
        putStrLn "Thanks for playing!"
        return state
    | otherwise = do
        let (action:arg) = words cmd
        case action of
            "go" -> goTo (unwords arg) state
            "take" -> takeItem (unwords arg) state
            _ -> do
                putStrLn "Invalid command. Try again."
                return state

-- Move to another room
goTo :: String -> GameState -> IO GameState
goTo direction state = do
    let room = Map.lookup (currentRoom state) rooms
    case room of
        Just r -> case Map.lookup direction (exits r) of
            Just nextRoom -> do
                putStrLn $ "You go " ++ direction ++ "."
                return state { currentRoom = nextRoom }
            Nothing -> do
                putStrLn "You can't go that way."
                return state
        Nothing -> return state

-- Take an item
takeItem :: String -> GameState -> IO GameState
takeItem item state = do
    let room = Map.lookup (currentRoom state) rooms
    case room of
        Just r -> if item `elem` items r
            then do
                putStrLn $ "You take the " ++ item ++ "."
                let newInventory = item : inventory state
                let newRoom = r { items = filter (/= item) (items r) }
                saveRoom (currentRoom state) newRoom
                return state { inventory = newInventory }
            else do
                putStrLn "That item is not here."
                return state
        Nothing -> return state

-- Save the updated room back to the map
saveRoom :: String -> Room -> IO ()
saveRoom roomName room = do
    let updatedRooms = Map.insert roomName room rooms
    -- Print the updated room details
    putStrLn $ "Updated room: " ++ roomName ++ " with items: " ++ show (items room)

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
    displayRoom state
    putStr "> " 
    cmd <- getLine
    newState <- handleCommand cmd state
    when (cmd /= "quit") $ gameLoop newState

-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to the Text-Based Adventure Game!"
    let initialState = GameState { currentRoom = "Entrance", inventory = [] }
    gameLoop initialState
