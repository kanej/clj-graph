(ns clj-graph.algo
  ^{:author "John Kane"
    :description "Text book graph algorithms."}
  (:require [clj-graph.core :as g])
  (:require [clj-graph.vertex-queue :as vq]))

(declare choose-vertex)
(declare initialise-vertex-state-from)
(declare unseen-neighbours) 
(declare mark-vertex-colour) 
(declare traversal) 

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
    (let [vertex-state (initialise-vertex-state-from graph) 
          vertex-queue (conj [] start-vertex)
          walk []]
      (traversal graph vertex-state vertex-queue walk)))) 

(defn breadth-first-search 
  "Performs a breadth first traversal on the given graph, reaching the 
   connected vertexes from the given vertex."
  ([graph]
    (let [vertex (choose-vertex graph)]
      (if (nil? vertex)
        [] 
        (breadth-first-search graph vertex))))
  ([graph start-vertex] 
    (let [vertex-state (initialise-vertex-state-from graph) 
          vertex-queue (conj clojure.lang.PersistentQueue/EMPTY start-vertex) 
          walk []]
      (traversal graph vertex-state vertex-queue walk)))) 

(defn- choose-vertex [graph]
  (first (keys (:vertexes graph))))

(defn- initialise-vertex-state-from [graph]
  (apply hash-map (mapcat #(vector % :white) (keys (:vertexes graph)))))

(defn- traversal 
  [graph vertex-state vertex-queue walk]
  (if (empty? vertex-queue)
    walk
    (let [;; Pull the first vertex from the queue,
          ;; creating a new smaller queue in the process
          [vertex-queue-excluding-current current-vertex] (vq/dequeue vertex-queue)

          ;; Add the current vertex to the walk, as we are processing it
          updated-walk (conj walk current-vertex)

          ;; Push the unseen neighbours onto the next vertexes list
          white-neighbours (unseen-neighbours graph current-vertex vertex-state)
          updated-vertex-queue (vq/enqueue vertex-queue-excluding-current white-neighbours)

          ;; Update the vertex state marking the current vertex as black
          ;; and the neighbours as seen i.e. grey
          updated-vertex-state (-> vertex-state 
                                   (mark-vertex-colour [current-vertex] :black)
                                   (mark-vertex-colour white-neighbours :grey))] 
      (recur graph updated-vertex-state updated-vertex-queue updated-walk))))

(defn- mark-vertex-colour 
  "Return the vertex state map after updating each of the given vertexes
   to have the given colour."
  [vertex-state vertexes colour]
  (reduce #(assoc %1 %2 :grey) vertex-state vertexes))

(defn- unseen-neighbours 
  "Find the neighbours of a vertex that have not been seen before
   as defined by having a colour of White rather than Grey or Black."
  [graph vertex vertex-state]
  (let [neighbours (g/neighbours graph vertex)]
    (filter #(= :white (get vertex-state %)) neighbours)))
