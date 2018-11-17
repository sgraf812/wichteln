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

You can put extra constraints in `constraints.txt`, specifying which edges in
the bipartite graph corresponding to the permutation are *not* allowed, indexed
by year (because, who knows). Example:

```
(1999, "Bert", "Ernie")
```

This specifies that in 1999, Bert had to gift Ernie and makes sure that in i.e.
2000 he won't have to wichtel Ernie again.