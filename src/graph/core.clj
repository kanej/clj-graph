(ns graph.core)

(defn increment-2d-array [a]
  (let [arrity (count a)
        extend-existing-rows (vec (map #(conj % nil) a))
        append-new-row (conj extend-existing-rows (vec (repeat (inc arrity) nil)))]
    append-new-row))

(defn new-graph []
  {:nodes {} :edges []})

(defn add-node [g node]
  (let [old-nodes (:nodes g)
        new-nodes (assoc old-nodes node (count old-nodes))
        old-edges (:edges g)
        new-edges (increment-2d-array old-edges)]
    {:nodes new-nodes :edges new-edges}))

(defn add-edge [g from-node to-node]
  (let [from-node-index (get-in g [:nodes from-node])
        to-node-index   (get-in g [:nodes to-node])]
    (-> g 
      (assoc-in [:edges from-node-index to-node-index] to-node)
      (assoc-in [:edges to-node-index from-node-index] from-node)))) 

(defn edge? [g from-node to-node]
  (and (get-in g [:nodes from-node])
       (get-in g [:nodes to-node])
       (get-in g [:edges (get-in g [:nodes from-node]) (get-in g [:nodes to-node])])))

(defn neighbours [g v]
  (let [node-index (get-in g [:nodes v])
        row (get-in g [:edges node-index])
        neighbours (keep-indexed #(if %2 %2) row)]
    neighbours))
