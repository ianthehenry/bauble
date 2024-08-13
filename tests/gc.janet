(use sh)

(def marked @{})
(each filename (os/dir "./refs")
  (put marked
    (string/slice (os/readlink (string "refs/" filename)) (length "../snapshots/"))
    true))

(each filename (os/dir "./snapshots")
  (if (marked filename)
    (do
      (printf "optimizing %q" filename)
      # pngcrush seems to just ignore -q, so we drop stderr
      ($ pngcrush -ow -q ./snapshots/ ^ ,filename >[stderr :null]))
    (do
      (printf "rm %q" filename)
      (os/rm (string "./snapshots/" filename)))))
