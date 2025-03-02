import websockets.sync.client as ws
import json
import time
import re


socket = ws.connect("ws://localhost:58846")

#socket.send(json.dumps({"contents": ["The room", name], "tag": "Register"}))
