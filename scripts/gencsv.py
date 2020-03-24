import string
import csv
import random
import sys

testloc ="../testbig1.csv"
write_count = 0

with open(testloc, "w") as out_fp:
    writer = csv.writer(out_fp, delimiter=";")
    while True:
        if (write_count % 1_000_000) == 0:
            print("x", end="", flush=True)
        try:
            writer.writerow([
                "x",
                random.uniform(-1000, 1000),
                random.randint(-100000, 100000),
                ''.join(random.choices(string.ascii_uppercase, k=random.randint(15, 25))),
                "`*-><\"''\"''\"''||#more_\"_"
            ])
            write_count += 1
        except KeyboardInterrupt:
            print(f"Written {write_count} rows")
            sys.exit()