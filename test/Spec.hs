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



testEnPassant :: IO ()
testEnPassant = do
    newState <- chessRunner chessInitial
        [ Touch White (4, 6), Touch White (4, 4)
        , Touch Black (0, 1), Touch Black (0, 2)
        , Touch White (4, 4), Touch White (4, 3)
        , Touch Black (3, 1), Touch Black (3, 3)
        , Touch White (4, 3), Touch White (3, 2)
        , Touch Black (0, 2), Touch Black (0, 3)
        , Touch White (3, 2), Touch White (2, 1)]

    let targetBoard = parseFEN "rnbqkbnr/1pP1pppp/8/p7/8/8/PPPP1PPP/RNBQKBNR"
    let targetState = chessInitial {
        _board = fromRight undefined targetBoard,
        _turn  = 7,
        _enpassant = Just (4,(3,2),(3,3))
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("

testLongCastle :: IO ()
testLongCastle = do
    newState <- chessRunner chessInitial
        [ Touch White (3, 6), Touch White (3, 5)
        , Touch Black (2, 1), Touch Black (2, 2)
        , Touch White (1, 7), Touch White (2, 5)
        , Touch Black (3, 1), Touch Black (3, 2)
        , Touch White (2, 7), Touch White (4, 5)
        , Touch Black (4, 1), Touch Black (4, 2)
        , Touch White (3, 7), Touch White (3, 6)
        , Touch Black (5, 1), Touch Black (5, 2)
        , Touch White (4, 7), Touch White (2, 7)]

    let targetBoard = parseFEN "rnbqkbnr/pp4pp/2pppp2/8/8/2NPB3/PPPQPPPP/2KR1BNR"
    let targetState = chessInitial {
        _board = fromRight undefined targetBoard,
        _turn  = 9,
        _castling = S.fromList [(0, 0), (4, 0), (7, 0), (7, 7)]
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("

testPromotion :: IO ()
testPromotion = do
    newState <- chessRunner chessInitial
        [ Touch White (3, 1), Touch White (3, 0) , Promote White "Q" ]

    let targetBoard = parseFEN "2R5/3P4/8/8/8/4kp2/P7/5K2"
    let targetState = chessInitial {
        _board = fromRight undefined targetBoard,
        _turn  = 9
    }

    if newState == targetState then
        putStrLn "All good!"
    else
        putStrLn ":("