(ns clj-graph.algo
  ^{:author "John Kane"
    :description "Text book graph algorithms."}
  (:use clj-graph.core))

(declare unseen-neighbours)

(defn depth-first-search 
  "Performs a depth first traversal on the given graph.
  
   Currently it only finds the vertexes in the connected
   graph starting at the given vertex."
  ([graph]
    (let [vertex (first (keys (:vertexes graph)))]
      (if (nil? vertex)
        [] 
        (depth-first-search graph vertex))))
  ([graph start-vertex] 
    (let [next-vertexes [start-vertex]
          vertex-state (apply hash-map (mapcat #(vector % :white) (keys (:vertexes graph)))) 
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

            ;; Push the unseen neighbours onto the next vertexes list
            next-vertexes (concat white-neighbours next-vertexes)

            ;; Update the vertex state marking the current vertex as black
            ;; and the neighbours as seen i.e. grey
            updated-vertex-state (assoc vertex-state current-vertex :black)
            updated-vertex-state (reduce #(assoc %1 %2 :grey) updated-vertex-state white-neighbours)] 
        (recur graph updated-vertex-state next-vertexes updated-walk)))))

(defn- unseen-neighbours 
  "Find the neighbours of a vertex that have noot been seen before
   as defined by having a colour of White rather than Grey or Black."
  [graph vertex vertex-state]
  (let [neighbours (neighbours graph vertex)]
    (filter #(= :white (get vertex-state %)) neighbours)))
