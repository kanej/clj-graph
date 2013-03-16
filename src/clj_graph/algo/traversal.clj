(ns clj-graph.algo.traversal
  ^{:author "John Kane"
    :description "Text book graph algorithms."}
  (:require [clj-graph.core :as g])
  (:require [clj-graph.algo.vertex-queue :as vq]))

(declare choose-vertex)
(declare initialise-vertex-state-from)
(declare unseen-neighbours) 
(declare mark-vertex-colour) 
(declare traversal) 

(defn depth-first-search 
  "Performs a depth first traversal on the given graph.

   Currently it only finds the vertexes in the connected
   graph starting at the given vertex."
  ([graph start-vertex] 
    (let [fifo-vertex-queue vq/FIFO]
      (traversal graph start-vertex fifo-vertex-queue)))) 

(defn breadth-first-search 
  "Performs a breadth first traversal on the given graph, reaching the 
   connected vertexes from the given vertex."
  ([graph start-vertex] 
    (let [filo-vertex-queue vq/FILO]
      (traversal graph start-vertex filo-vertex-queue)))) 

(defn- traversal
  "Perform a traversal of the given graph, returning an ordered
   array of the vertexes encountered in the walk. Only vertexes
   connected to vertexes in the vertex queue will be reached
   by this traversal.

   The type of traversal is controlled by the vertex queue,
   a FIFO queue for depth first traversal and a FILO queue
   for breadth first traversal. Any vertex queue must
   conform to the VertexQueue protocol."
  ([graph start-vertex vertex-queue]
    (let [updated-vertex-queue (conj vertex-queue start-vertex)
          vertex-state (initialise-vertex-state-from graph) 
          walk []]
      (traversal graph vertex-state updated-vertex-queue walk)))
  ([graph vertex-state vertex-queue walk] 
    (cond 
      (empty? (:vertexes graph)) []
      (empty? vertex-queue) walk
      :default
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
        (recur graph updated-vertex-state updated-vertex-queue updated-walk)))))

(defn- initialise-vertex-state-from 
  "Creates a Vertex State map given a graph. All vertexes
   are initally coloured white."
  [graph]
  (apply hash-map (mapcat #(vector % :white) (keys (:vertexes graph)))))

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
