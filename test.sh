#!/usr/bin/env bash

input=$(sed ":loop
N
s/\n/\\\\n/
t loop")

i=$(sed "s/\"/'/g" <<< "$input")

# echo "$input"
# echo "$i"
elm make src/Main.elm --output=main.js

head -n -1 main.js > main2.js
echo "elm\$json\$Json\$Decode\$succeed(_Utils_Tuple0))(0)}}); author\$project\$Ini\$parseIni(input_text);});" >> main2.js
sed -i "1s/.*/var main = (function(scope, input_text){/" main2.js
echo "main(this, \"$i\")" >> main2.js
node main2.js
