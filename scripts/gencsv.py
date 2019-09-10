from pathlib import Path
import csv
import random

testloc = Path("..") / "testbig2.csv"
total_count = 1_000_000_000

with open(testloc, "w") as out_fp:
    writer = csv.writer(out_fp, delimiter=";")
    for i in range(total_count):
        if i % 10_000_000: print(".", end="")
        writer.writerow(["x", "y", "bla  bla", random.randint(0, total_count), "''''''||more__"])
