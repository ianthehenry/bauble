(declare-project
  :name "bauble"
  :description "signed distance function playground"
  :dependencies [
  "https://github.com/ianthehenry/jaylib.git"
  "https://github.com/ianthehenry/judge.git"
  "https://github.com/ianthehenry/cmd.git"
  "https://github.com/ianthehenry/janet-module.git"
  "https://github.com/ianthehenry/pat.git"
  ])

(declare-executable
 :name "bauble"
 :description "Bauble CLI"
 :entry "cli/init.janet")
