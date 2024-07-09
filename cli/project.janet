(declare-project
  :name "bauble-cli"
  :description "CLI for Bauble"
  :dependencies [
  "https://github.com/ianthehenry/jaylib.git"
  "https://github.com/ianthehenry/judge.git"
  "https://github.com/ianthehenry/cmd.git"
  ])

(declare-executable
 :name "main"
 :description "bauble CLI"
 :entry "main.janet")
