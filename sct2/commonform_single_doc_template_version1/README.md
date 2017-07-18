# The Python Bit

## Installing the Python Piece

Do this once from this directory:
```
mkdir generated
cd ../3rdparties; for c in commonform commonaccord; do (cd $c; ../git-multi-repos); done
cd commonform
npm init --yes
npm install --only=dev
npm i -g commonform-cli # 0.27.0
```

## Running the Python Piece

From this directory:
```
python gen_SAFE
```

Look in `generated/` for output.

# The Javascript Piece

## Installing the Javascript Piece

Do this once from this directory:

```
make build
```

## Running the Javascript Piece

```
cd build
node read_SAFE.js ../generated/SAFE-MFN.gen.commonform
```

Should produce a bunch of STDOUT.

# TODO

Stitch together the JS bit with the Python bit to produce a complete toolchain.


# SYNTAX

The syntax of SAFE-combined.txt is pretty self-explanatory except for the lines that start with * or some combination of bcdm. 
-* means output the string for all 4 versions.
-c means output the string for the cap version.
-d means output the string for the discount version.
-b (both) means output the string for the cap discount version.
-m means output the string for the MFN version.

SAFE-combined.txt is the combined template with a few minor edits.
-Changed an occurrence of "having" to "which will have", for consistency between the three documents that define SAFE Preferred Stock.
-There was one occurrence of Standard Preferred Stock that was marked up as a reference <Standard Preferred Stock>.
-There is one whitespace difference as a result of one of the commonform templates having a line identical to others but with a space at the end.

SAFE-combined.no_edits.txt is the combined template without those edits.
