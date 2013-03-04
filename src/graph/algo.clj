(ns graph.algo
  (:use graph.core))

(defn unseen-neighbours [graph vertex vertex-state]
  (let [neighbours (neighbours graph vertex)]
    (filter #(= :white (get vertex-state %)) neighbours)))

(defn depth-first-search 
  ([graph]
    (let [vertex (first (keys (:nodes graph)))]
      (if (nil? vertex)
        [] 
        (depth-first-search graph vertex))))
  ([graph start-vertex] 
    (let [next-vertexes [start-vertex]
          vertex-state (apply hash-map (mapcat #(vector % :white) (keys (:nodes graph)))) 
          walk []]
      (depth-first-search graph vertex-state next-vertexes walk)))
  ([graph vertex-state next-vertexes walk]
    (if (empty? next-vertexes)
      walk
      (let [current-vertex (first next-vertexes)
            updated-walk (conj walk current-vertex)
            next-vertexes (rest next-vertexes)
            updated-vertex-state (assoc vertex-state current-vertex :gray)
            white-neighbours (unseen-neighbours graph current-vertex updated-vertex-state)
            next-vertexes (concat white-neighbours next-vertexes)] 
        (recur graph updated-vertex-state next-vertexes updated-walk)))))
