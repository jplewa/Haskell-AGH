nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
    action
    nTimes (n-1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
    1 -> \name -> putStrLn ("Good morning, " ++ name)
    2 -> \name -> putStrLn ("Good afternoon, " ++ name)
    3 -> \name -> putStrLn ("Good night, " ++ name)
    _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList =    [ioActionFactory 1 "Ben",
                 ioActionFactory 2 "Joe",
                 ioActionFactory 3 "Ally"]

sequence' :: [IO ()] -> IO ()
sequence' [] = return ()
sequence' (a:as) =  do
                    a
                    sequence' as

sequence'' :: [IO ()] -> IO ()
sequence'' =  foldr (>>) (return ())

sequence''' :: [IO ()] -> IO ()
sequence''' =  foldr (\a b -> a >> b) (return ())

sequenceRev :: [IO ()] -> IO ()
sequenceRev = foldr (\a b -> b >> a) (return ())

sequenceRev' :: [IO ()] -> IO ()
sequenceRev' a = foldr (>>) (return ()) (reverse a)