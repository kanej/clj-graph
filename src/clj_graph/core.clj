(ns 
  ^{:author "John Kane"
    :doc "Graph data structures"}
  clj-graph.core
  (:refer-clojure :exclude [inc])
  (:require [clj-graph.edge-matrix :as em]))

(defn graph []
  {:vertexes {} :edges (em/edge-matrix)})

(defn add-vertex [{:keys [vertexes edges] :as graph} vertex]
  (let [new-vertexes (assoc vertexes vertex (count vertexes))
        new-edges (em/inc edges)]
    (-> graph
        (assoc :vertexes new-vertexes)
        (assoc :edges new-edges))))

(defn add-edge [{:keys [vertexes edges] :as graph} from-vertex to-vertex]
  (let [from-index (from-vertex vertexes)
        to-index   (to-vertex vertexes)
        updated-edges (-> edges
                          (em/update from-index to-index to-vertex)
                          (em/update to-index from-index from-vertex))]
    (assoc graph :edges updated-edges))) 

(defn edge? [{:keys [vertexes edges] :as graph} from-vertex to-vertex]
  (and 
    ; The given from vertex exists
    (from-vertex vertexes) 
    ; The given to vertex exists
    (to-vertex vertexes)
    ; There is an edge within the matrix
    (em/lookup edges (from-vertex vertexes) (to-vertex vertexes))))

(defn neighbours [{:keys [vertexes edges]} from-vertex]
  (let [vertex-index (from-vertex vertexes) 
        row (get edges vertex-index) 
        neighbours (vec (remove nil? row))]
    neighbours))
