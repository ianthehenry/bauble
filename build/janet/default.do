redo-ifchange janet-version

version=$(cat janet-version)

curl -ss -L "https://github.com/janet-lang/janet/releases/download/$version/$1" -o "$3"
