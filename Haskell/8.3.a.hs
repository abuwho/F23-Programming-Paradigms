foreverIO :: IO a -> IO b
foreverIO action = do
    action
    foreverIO action