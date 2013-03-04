(ns graph.algo
  (:use graph.core))

(defn unseen-neighbours [graph vertex vertex-state]
  (let [neighbours (neighbours graph vertex)]
    (filter #(= :white (get vertex-state %)) neighbours)))

(defn depth-first-search 
  ([graph]
    (let [v (first (keys (:nodes graph)))]
      (if (nil? v)
        [] 
        (depth-first-search graph v))))
  ([graph start-vertex] 
    (let [next-vertexes []
          vertex-state (apply hash-map (mapcat #(vector % :white) (keys (:nodes g)))) 
          walk []]
      (depth-first-search graph start-vertex vertex-state next-vertexes walk)))
  ([graph start-vertex vertex-state next-vertexes walk]
    (let [white-neighbours (unseen-neighbours graph start-vertex vertex-state)] 
          (if (empty? white-neighbours)
            (conj walk start-vertex)
            (concat [start-vertex] white-neighbours)))))
