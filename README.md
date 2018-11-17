# `wichteln`

1. Install stack https://docs.haskellstack.org/en/stable/README/#how-to-install
2. Put a newline separated list of names into `names.txt`
3. `$ stack ./wichteln.hs`

This will assign each name in `names.txt` a Secret Santa partner. Example:

```
$ cat names.txt
Bert
Ernie
Tiffy
$ stack ./wichteln.hs
...
$ cat Bert.txt
Ernie
```

Meaning that Bert has to think of a gift for Ernie.

---

The script avoids cycles of up to length 2 in the permutation, so it avoids
cases where Ernie would gift himself or where Ernie would gift Bert in the above
scenario.

You can tweak this by setting `minCycleLength` in the script accordingly.

N.B.: Revealing that Bert wichtels Ernie entails that Ernie wichtels Tiffy and
Tiffy wichtels Bert, because there are only two possible permutations with cycle
length at least 3 and the orientation of one edge determines which cycle.

## Seed

The seed for RNG is stored in `seed.txt`, read and overwritten after each run.
Saving the seed file allows for deterministic reproductions.

## Constraints

You can put extra constraints in `assignment.txt`, specifying which edges in
the permuation graph are *not* allowed. Example:

```
("Bert", "Ernie")
```

This specifies that Bert doesn't want to gift Ernie and makes sure that he won't
have to wichtel Ernie again.

Note that after a run, this file gets overwritten with the new assignment, so
that there won't be any similar assignments as the result of consecutive runs of
the script.