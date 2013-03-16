(ns clj-graph.algo
  ^{:author "John Kane"
    :description "Text book graph algorithms."}
  (:use clj-graph.core))

(declare choose-vertex)
(declare initialise-vertex-state-from)
(declare unseen-neighbours) 
(declare mark-vertex-colour) 
(declare traversal) 

(defprotocol VertexQueue
  "A queue for storing vertexes"
  (enqueue [vq vertexes] "Add the given vertexes to the queue.")
  (dequeue [vq] "Return a 2 value tuple the first value is the queue with the first vertex removed, the second value is the first vertex; the definition of first vertex is left to the implementation.")) 

(extend-protocol VertexQueue
  clojure.lang.IPersistentVector
  (enqueue [this vertexes]
    (vec (concat vertexes this)))
  (dequeue [this]
    [(vec (rest this)) (first this)])) 

(extend-protocol VertexQueue
  clojure.lang.PersistentQueue
  (enqueue [this vertexes]
    (reduce #(conj %1 %2) this vertexes))
  (dequeue [this]
    [(pop this) (peek this)]))

(defn depth-first-search 
  "Performs a depth first traversal on the given graph.

   Currently it only finds the vertexes in the connected
   graph starting at the given vertex."
  ([graph]
    (let [vertex (choose-vertex graph)]
      (if (nil? vertex)
        [] 
        (depth-first-search graph vertex))))
  ([graph start-vertex] 
    (let [next-vertexes (conj [] start-vertex);;(conj clojure.lang.PersistentQueue/EMPTY start-vertex) 
          vertex-state (initialise-vertex-state-from graph) 
          walk []]
      (traversal graph vertex-state next-vertexes walk)))) 

(defn breadth-first-search 
  "Performs a breadth first traversal on the given graph, reaching the 
   connected vertexes from the given vertex."
  ([graph]
    (let [vertex (choose-vertex graph)]
      (if (nil? vertex)
        [] 
        (breadth-first-search graph vertex))))
  ([graph start-vertex] 
    (let [next-vertexes (conj clojure.lang.PersistentQueue/EMPTY start-vertex) 
          vertex-state (initialise-vertex-state-from graph) 
          walk []]
      (traversal graph vertex-state next-vertexes walk)))) 

(defn- choose-vertex [graph]
  (first (keys (:vertexes graph))))

(defn- initialise-vertex-state-from [graph]
  (apply hash-map (mapcat #(vector % :white) (keys (:vertexes graph)))))

(defn- traversal 
  [graph vertex-state next-vertexes walk]
  (if (empty? next-vertexes)
    walk
    (let [;; Pull the first vertex from the queue,
          ;; creating a new smaller queue in the process
          [next-vertexes-excluding-current current-vertex] (dequeue next-vertexes)

          ;; Add the current vertex to the walk, as we are processing it
          updated-walk (conj walk current-vertex)

          ;; Push the unseen neighbours onto the next vertexes list
          white-neighbours (unseen-neighbours graph current-vertex vertex-state)
          updated-vertexes (enqueue next-vertexes-excluding-current white-neighbours)

          ;; Update the vertex state marking the current vertex as black
          ;; and the neighbours as seen i.e. grey
          updated-vertex-state (-> vertex-state 
                                   (mark-vertex-colour [current-vertex] :black)
                                   (mark-vertex-colour white-neighbours :grey))] 
      (recur graph updated-vertex-state updated-vertexes updated-walk))))

(defn- mark-vertex-colour 
  "Return the vertex state map after updating each of the given vertexes
   to have the given colour."
  [vertex-state vertexes colour]
  (reduce #(assoc %1 %2 :grey) vertex-state vertexes))

(defn- unseen-neighbours 
  "Find the neighbours of a vertex that have not been seen before
   as defined by having a colour of White rather than Grey or Black."
  [graph vertex vertex-state]
  (let [neighbours (neighbours graph vertex)]
    (filter #(= :white (get vertex-state %)) neighbours)))
