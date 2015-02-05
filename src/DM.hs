-- Be the Dungeon Master

{-# LANGUAGE DeriveDataTypeable #-}

module DM
    where

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
    id :: String,
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

-- Process a Dungeon Master command string.
processDMCommand command params currentScene = do
    let args = words params
    case (command) of
        "/l" -> putStrLn $ describe currentScene
        --"/d" -> putStrLn $ answer ((dialogues ((npcs currentScene) !! 0)) !! 0)
        "/d" -> putStrLn $ dialogCommand currentScene args

loadAdventure filename = do
    text <- readFile filename
    let adventure = decodeJSON text :: Adventure
    return adventure

