#!/usr/bin/env bash

input=$(sed ":loop
N
s/\n/\\\\n/
t loop")

i=$(sed "s/\"/'/g" <<< "$input")

# echo "$input"
# echo "$i"

cat main.js > main2.js
echo "main(this, \"$i\")" >> main2.js
#
node main2.js
