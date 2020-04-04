import csv

# Note that there could be differences between the python-data-types here
# and Java's datatypes used in Exort (currently)
testloc = "../testbig1_done.tsv"
#testloc = "/tmp/exsort_JjBHvyyh1A1610421042881013882/p1-p5.tsv"
locs = [2, 3, 1]
types = [int, str, float]
compares = ["lt", "gt", "lt"]

assert len(locs) == len(types)
assert len(locs) == len(compares)


def testrows(old, new, rownr):
    for i, loc in enumerate(locs):
        dtype = types[i]
        oldval = dtype(old[loc])
        newval = dtype(new[loc])
        if compares[i] == "lt":
            if oldval > newval:
                print(
                    f"Error in row {rownr} with old:{oldval} and new:{newval}"
                    f" @ step {i}",
                    flush=True,
                )
                return
            elif oldval == newval:
                continue
            else:
                return
        elif compares[i] == "gt":
            if oldval < newval:
                print(
                    f"Error in row {rownr} with old:{oldval} and new:{newval}"
                    f" @ step {i}",
                    flush=True,
                )
                return
            elif oldval == newval:
                continue
            else:
                return


with open(testloc, newline="") as test_fp:
    testreader = csv.reader(test_fp, delimiter="\t", quotechar='"')
    oldrow = next(testreader)
    rownr = 1
    for row in testreader:
        testrows(oldrow, row, rownr)
        oldrow = row
        rownr += 1
