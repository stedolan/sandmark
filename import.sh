#!/bin/sh

exec git subtree pull --squash -P packages/"$1" "$2" "$3" -m "Import $2 $3 as packages/$1"
