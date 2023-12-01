#!/usr/bin/env bash

SCRIPTDIR=$(dirname $0)

wget https://adventofcode.com/2023/day/$1/input --user-agent "https://github.com/benjamingeiger/advent-of-code/blob/main/2023/get-input.sh" --header "Cookie: $(cat $SCRIPTDIR/cookie.txt)" -O input.real.txt && cp input.real.txt input.txt
