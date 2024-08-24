import websockets.sync.client as ws
import json
import time


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
