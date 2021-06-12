#!/bin/python3
import os

# Alternative implementation in dune_gen.ml

names=""
for f  in os.listdir("."):
    if f.endswith(".ml"):
        names=names+ " "+f.rstrip(".ml")

with open("dune", "w") as f:
    f.write("""(executables
(names {names}))""".format(names=names))
