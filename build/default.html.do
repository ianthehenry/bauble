source=$2
page=$source
toc=normal
if [[ $source = "all" ]]; then
  source=all.md
  toc=local
elif [[ $source = "index" ]]; then
  source=../content/intro.md
else
  chapter=$(basename $source)
  page=$chapter
  source=../content/chapters/$chapter.md
fi

redo-ifchange mode all.js.checksum main.css.checksum chapters parse.mjs ../yarn.lock "$source"

mode=$(cat mode)

js=$(< all.js.checksum)
css=$(< main.css.checksum)
mkdir -p $(dirname $2)

node parse.mjs \
  --js $js \
  --css $css \
  --mode $mode \
  --chapters chapters \
  --page $page \
  --toc $toc \
  <$source
