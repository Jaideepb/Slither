#! /bin/bash
# Runs the necessary commands to work
# as a trampoline for sbcl

if [ $# -eq 0 ]
then
    echo "Usage ./slither.sh INPUT_PUZZLE"
else
    if [ $# -eq 1]
    then
	sbcl --noinform --load config-parser.fasl --load sudoko.fasl --eval "(progn (slither:backtracking \"$1\") (quit))"
    else
	sbcl --noinform --load config-parser.fasl --load sudoko.fasl --eval "(progn (slither:backtracking \"$1\" \"$2\") (quit))"
    fi
fi
	
