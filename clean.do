redo-always
files=$(git ls-files build/ --ignored --exclude-standard -o)

if [[ $files != "" ]]; then
  rm -- $files
fi

while (find build -type d -empty -print0 -exec rmdir {} \+ | grep -qz .); do
:
done
