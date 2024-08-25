(use ./import)

(defn split-axis [axis]
  (sugar (case axis
    x [p.x p.yz]
    y [p.y p.xz]
    z [p.z p.xy]
    (errorf "unknown axis %q" (jlsl/show axis)))))
