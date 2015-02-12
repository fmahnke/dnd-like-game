-- Be the Dungeon Master

{-# LANGUAGE DeriveDataTypeable #-}

module DM
    where

import Data.List
import Data.Maybe
import Text.JSON.Generic

import Menu

data Sequence = Sequence {
    from :: String,
    to :: String
} deriving (Show, Data, Typeable)

data Dialogue = Dialogue {
    question :: String,
    answer :: String
} deriving (Show, Data, Typeable)

data Npc = Npc {
    name :: String,
    dialogues :: [Dialogue]
} deriving (Show, Data, Typeable)

data Scene = Scene {
    sceneId :: String,
    sceneName :: String,
    links :: [String],
    narration :: String,
    npcs :: [Npc]
} deriving (Show, Data, Typeable)

data Adventure = Adventure {
    sequences :: [Sequence],
    scenes :: [Scene]
} deriving (Show, Data, Typeable)

describe :: Scene -> String
describe (Scene {sceneName = s, narration = n}) =
    s ++ "\n\n" ++ n

dialogueString :: Dialogue -> String
dialogueString dialogue =
    "Question:" ++ " " ++ (question dialogue) ++ "\n" ++ "Answer:" ++ " " ++ (answer dialogue)

-- Dialog command.
dialogCommand :: Scene -> [String] -> String
dialogCommand scene args
    | length args == 0 = unlines $ menu $ map dialogueString (dialogues ((npcs scene) !! 0))
    | otherwise = answer ((dialogues ((npcs scene) !! npcIndex)) !! dialogueIndex)
    where npcIndex = (read $ args !! 0 :: Int)
          dialogueIndex = (read $ args !! 1 :: Int)

nextScene :: String -> Scene -> Adventure -> Scene
nextScene command scene adventure
    | command == "/next" = Data.Maybe.fromMaybe scene $ Data.List.find (\x -> (sceneName x == nextSceneName)) (scenes adventure)
    | otherwise = scene
    where nextSceneName = (links scene !! 0)

-- Links command.
linksCommand :: Scene -> String
linksCommand scene = unlines $ menu $ links scene

-- Process a Dungeon Master command string.
processDMCommand :: String -> String -> Scene -> Adventure -> IO (Scene)
processDMCommand command params currentScene adventure = do
    let args = words params
    
    case (command) of
        -- Describe current scene.
        "/sc" -> putStrLn $ describe currentScene
        -- List all dialogues, optionally by NPC index.
        "/di" -> putStrLn $ dialogCommand currentScene args
        -- Show NPC menu.
        "/np" -> putStrLn $ unlines $ menu $ map name (npcs currentScene)
        -- Show links.
        "/li" -> putStrLn $ linksCommand currentScene
        _ -> putStr ""

    return $ nextScene command currentScene adventure
            

loadAdventure filename = do
    text <- readFile filename
    let adventure = decodeJSON text :: Adventure
    return adventure

