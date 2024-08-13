(declare-project
  :name "bauble-tests"
  :description "test suite for Bauble"
  :dependencies [
  "https://github.com/ianthehenry/jaylib.git"
  "https://github.com/ianthehenry/judge.git"
  "https://github.com/ianthehenry/cmd.git"
  "https://github.com/andrewchambers/janet-sh.git"

  # we don't actually depend on these. these are transient dependencies from jlsl.
  # there's no way good way for multiple janet libraries to cohabitate in a single
  # git repo so here we are.
  "https://github.com/ianthehenry/pat.git"
  "https://github.com/ianthehenry/janet-module.git"
  ])
