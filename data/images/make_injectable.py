import re
import os


for fn in os.listdir():
    print(fn)
    if fn.endswith(".svg"):
        out = ""
        
        with open(fn) as f:
            for i, line in enumerate(f.readlines()):
                if i == 0 and "xml" in line:
                    continue

                new = re.sub("fill:.*?;", "", line)
                out += new

        if out:
            with open(fn, mode="w") as f:
                f.write(out)
