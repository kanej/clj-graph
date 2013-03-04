(ns graph.algo
  (:use graph.core))

(defn- unseen-neighbours [graph vertex vertex-state]
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
            white-neighbours (unseen-neighbours graph current-vertex vertex-state)

            ;; Remove the current vertex from the list now that we are 
            ;; processing it and add it to the walk
            next-vertexes (rest next-vertexes)
            updated-walk (conj walk current-vertex)

            ;; Update the vertex state marking the current vertex as black
            ;; and the neighbours as seen i.e. gray
            updated-vertex-state (assoc vertex-state current-vertex :black)
            updated-vertex-state (reduce #(assoc %1 %2 :gray) updated-vertex-state white-neighbours)

            ;; Push the unseen neighbours onto the next vertexes list
            next-vertexes (concat white-neighbours next-vertexes)] 
        (recur graph updated-vertex-state next-vertexes updated-walk)))))
