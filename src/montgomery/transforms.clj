(ns montgomery.transforms)

(defn returns->values
  ([returns] (reductions * 1.0 (map inc returns)))
  ([seed [& returns]] (reductions * seed (map inc returns))))

