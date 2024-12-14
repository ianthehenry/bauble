#!/usr/bin/env bash

# I don't think that embedding the full editor is going to be
# an officially-supported thing, so this is a little jank...
# really this is really pretty specific to wanting to embed
# in my own blog.

deps="vars.css mixed.css editor.css"
redo-ifchange $deps

cat <<EOF
.bauble-root {
  all: revert;
  & * {
    margin: unset;
  }
  & pre {
    all: unset;
  }
  * {
    box-sizing: content-box;
  }
  *:focus {
    outline: none;
  }
}
EOF

(
  cat $deps

  # obviously shouldn't duplicate these but sedding into
  # shape is too annoying
  cat <<EOF
  @media (prefers-color-scheme: dark) {
    .light-theme :root {
      --foreground: #4d4d4c;
      --background: #ffffff;
      --popover-background: #ffffffa0;
      --selection: #d6d6d6;
      --line: #efefef;
      --comment: #8e908c;
      --red: #c82829;
      --orange: #f5871f;
      --yellow: #eab700;
      --green: #718c00;
      --aqua: #3e999f;
      --blue: #4271ae;
      --purple: #8959a8;
      --window: #efefef;
    }
  }

  .dark-theme :root {
    --foreground: #c5c8c6;
    --background: #1d1f21;
    --popover-background: #1d1f21a0;
    --selection: #373b41;
    --line: #282a2e;
    --comment: #969896;
    --red: #cc6666;
    --orange: #de935f;
    --yellow: #f0c674;
    --green: #b5bd68;
    --aqua: #8abeb7;
    --blue: #81a2be;
    --purple: #b294bb;
    --window: #4d5057;
  }
EOF

  # e.g. these rules are incredibly specific to the way my blog happens to look right now:
  cat <<EOF
  .bauble-placeholder, pre.example {
    font-size: 13px;
    border-left: none;
    border-right: none;
    margin-top: 1em;
    border-radius: 0;
    padding: 0;
  }

EOF
) | sed -E \
  -e 's/\.bauble-placeholder/.bauble-root/g' \
  -e 's/pre\.example/pre:has(code.language-bauble)/g' \
  -e 's/(\.[a-z-]+ )?:root/\1 .bauble-root, \1 pre:has(code.language-bauble)/g' \
  -e 's/\b256px\b/384px/g' \
  -e 's/\b512px\b/768px/g' \
  #
