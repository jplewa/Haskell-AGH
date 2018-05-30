fun = do
    putStrLn "Podaj imie: "
    s <- getLine
    putStrLn $ "Witaj, " ++ s

fun2 =
    putStrLn "Podaj imie: " >>
    getLine >>=
    \s -> putStrLn $ "Witaj, " ++ s