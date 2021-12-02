#!/usr/bin/env bash

SCRIPTDIR=$(dirname $0)

wget https://adventofcode.com/2021/day/$1/input --header "Cookie: $(cat $SCRIPTDIR/cookie.txt)" -O input.real.txt && cp input.real.txt input.txt
