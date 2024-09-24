import websockets.sync.client as ws
import json
import time
import chess
import re


def make_player(name):
    socket = ws.connect("ws://localhost:12345")

    socket.send(json.dumps({"contents": ["The room", name], "tag": "Register"}))

    return socket

def touch(p, x, y):
    msg = {"contents": [x, y], "tag": "TouchMsg"}
    p.send(json.dumps(msg))

    try:
        while (r := p.recv(timeout=1)):
            print(r)
    except:
        ...

white = make_player("White")
black = make_player("Black")

board = chess.Board()

pgn = """1. e4 d5 2. exd5 Qxd5 3. Nf3 Bg4 4. Be2 Bxf3 5. Bxf3 Qc5 6. Bxb7 g5 7. Nc3 Kd7
8. Bxa8 Nf6 9. d3 h6 10. Bd2 Nc6 11. Bxc6+ Qxc6 12. Qf3 Qxf3 13. gxf3 Bg7 14.
O-O-O h5 15. h3 g4 16. hxg4 hxg4 17. Rxh8 Bxh8 18. fxg4 Nxg4 19. Ne4 f5 20. Ng3
c6 21. f3 Nh2 22. f4 e5 23. Nxf5 Nf3 24. Rh1 Nxd2 25. Kxd2 Bf6 26. Rh7+ Ke6 27.
Rxa7 Kxf5 28. Rc7 exf4 29. Rxc6 f3 30. Ke1 Bxb2 31. Kf1 Kg5 32. c3 Ba3 33. c4
Kg4 34. c5 Bxc5 35. Rxc5 Kg3 36. d4 Kg4 37. d5 Kf4 38. d6 Ke4 39. d7 Kd4 40. Rc8
Ke3"""
#d8=Q f2 42. Rc7 Kf3 43. Re7 Kg4 44. Qg8+ Kf4 45. Qf8+ Kg3 46. Rg7+ Kh4
#47. Qh8#"""

pgn = re.sub("\s+", " ", pgn)

x = pgn.split(" ")


white_moves = x[1::3]
black_moves = x[2::3]

import itertools as itr

print(board)
for move_white, move_black in itr.zip_longest(white_moves, black_moves):
    str_white = str(board.push_san(move_white))

    a, b = str_white[:2], str_white[2:]
    ax, ay = ord(a[0]) - ord('a'), 8 - int(a[1])
    bx, by = ord(b[0]) - ord('a'), 8 - int(b[1])

    print(str_white)
    print((ax, ay), (bx, by))
    touch(white, ax, ay)
    touch(white, bx, by)
    print(board)

    if move_black:
        
        str_black = str(board.push_san(move_black))
        
        a, b = str_black[:2], str_black[2:]
        ax, ay = ord(a[0]) - ord('a'), 8 - int(a[1])
        bx, by = ord(b[0]) - ord('a'), 8 - int(b[1])
        
        print(str_black)
        print((ax, ay), (bx, by))  
        touch(black, ax, ay)
        touch(black, bx, by)
        print(board)

