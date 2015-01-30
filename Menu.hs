module Menu
    where

-- Create a menu item from a string.
menuItem :: (Char, String) -> String
menuItem item = [fst item] ++ ") " ++ snd item

-- Create a menu from a list of strings.
menu :: [String] -> [String]
menu items = do
    map menuItem $ zip (take (length items) ['0'..]) items

