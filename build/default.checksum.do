redo-ifchange $2

extension="${2##*.}"
filename="${2%.*}"

printf "%s.%s.%s" $filename $(shasum -a 256 $2 | head -c 16) $extension >$3

redo-stamp <$3
