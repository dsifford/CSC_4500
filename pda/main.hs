-- Recursive Deterministic PDA implemented in Haskell
-- Grammar = a^nb^n {a,b}
-- Derek P Sifford

data State = S | F

pda :: State -> String -> String -> Bool
pda S ('a':xs) ('a':ys) = pda S xs ("aa" ++ ys)
pda S ('b':xs) ('a':ys) = pda F xs ys
pda S ('a':xs) [] = pda S xs "a"
pda F ('b':xs) ('a':ys) = pda F xs ys
pda F ('b':xs) [] = False
pda F [] [] = True
pda S _ _ = False
pda F _ _ = False

main = do
    line <- getLine
    if null line
        then return ()
        else do
            let check = pda S line ""
            if check
                then putStrLn "Valid!"
            else putStrLn "Invalid!"
            main
