import Data.Char (toUpper)

shoutBack :: IO ()
shoutBack = do
    putStrLn "Enter text (or 'exit' to quit):"
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            let shouting = map toUpper input
            putStrLn shouting
            shoutBack