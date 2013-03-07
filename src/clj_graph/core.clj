(ns 
  ^{:author "John Kane"
    :doc "Graph data structure"}
  clj-graph.core)

(defn increment-2d-array [a]
  (let [arrity (count a)
        extend-existing-rows (vec (map #(conj % nil) a))
        append-new-row (conj extend-existing-rows (vec (repeat (inc arrity) nil)))]
    append-new-row))

(defn graph []
  {:vertexes {} :edges []})

(defn add-vertex [g vertex]
  (let [old-vertexes (:vertexes g)
        new-vertexes (assoc old-vertexes vertex (count old-vertexes))
        old-edges (:edges g)
        new-edges (increment-2d-array old-edges)]
    {:vertexes new-vertexes :edges new-edges}))

(defn add-edge [g from-vertex to-vertex]
  (let [from-vertex-index (get-in g [:vertexes from-vertex])
        to-vertex-index   (get-in g [:vertexes to-vertex])]
    (-> g 
      (assoc-in [:edges from-vertex-index to-vertex-index] to-vertex)
      (assoc-in [:edges to-vertex-index from-vertex-index] from-vertex)))) 

(defn edge? [g from-vertex to-vertex]
  (and (get-in g [:vertexes from-vertex])
       (get-in g [:vertexes to-vertex])
       (get-in g [:edges (get-in g [:vertexes from-vertex]) (get-in g [:vertexes to-vertex])])))

(defn neighbours [g v]
  (let [vertex-index (get-in g [:vertexes v])
        row (get-in g [:edges vertex-index])
        neighbours (keep-indexed #(if %2 %2) row)]
    neighbours))
