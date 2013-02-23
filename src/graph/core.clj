(ns graph.core)

(defn increment-2d-array [a]
  (let [arrity (count a)
        extend-existing-rows (vec (map #(conj % false) a))
        append-new-row (conj extend-existing-rows (vec (repeat (inc arrity) false)))]
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
      (assoc-in [:edges from-node-index to-node-index] true)
      (assoc-in [:edges to-node-index from-node-index] true)))) 

(defn edge? [g from-node to-node]
  (and (get-in g [:nodes from-node])
       (get-in g [:nodes to-node])
       (get-in g [:edges (get-in g [:nodes from-node]) (get-in g [:nodes to-node])])))

