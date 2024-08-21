import Lib

main :: IO ()
main = do
    newState <- chessRunner chessInitial
        [ Touch White (4, 6), Touch White (4, 4)
        , Touch Black (0, 1), Touch Black (0, 2)
        , Touch White (4, 4), Touch White (4, 3)
        , Touch Black (3, 1), Touch Black (3, 3)
        , Touch White (4, 3), Touch White (3, 2)
        , Touch Black (0, 2), Touch Black (0, 3)
        , Touch White (3, 2), Touch White (2, 1)] 

    putStrLn "All good!"
