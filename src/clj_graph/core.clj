(ns 
  ^{:author "John Kane"
    :doc "Graph data structures"}
  clj-graph.core)

(declare edge-matrix)
(declare increment-edge-matrix)

(defn graph []
  {:vertexes {} :edges (edge-matrix)})

(defn add-vertex [{:keys [vertexes edges] :as graph} vertex]
  (let [new-vertexes (assoc vertexes vertex (count vertexes))
        new-edges (increment-edge-matrix edges)]
    (-> graph
        (assoc :vertexes new-vertexes)
        (assoc :edges new-edges))))

(defn add-edge [{:keys [vertexes] :as graph} from-vertex to-vertex]
  (let [from-index (from-vertex vertexes)
        to-index   (to-vertex vertexes)]
    (-> graph 
      (assoc-in [:edges from-index to-index] to-vertex)
      (assoc-in [:edges to-index from-index] from-vertex)))) 

(defn edge? [{:keys [vertexes edges] :as graph} from-vertex to-vertex]
  (and 
    ; The given from vertex exists
    (from-vertex vertexes) 
    ; The given to vertex exists
    (to-vertex vertexes)
    ; There is an edge within the matrix
    (get-in edges [(from-vertex vertexes) (to-vertex vertexes)])))

(defn neighbours [{:keys [vertexes edges]} from-vertex]
  (let [vertex-index (from-vertex vertexes) 
        row (get edges vertex-index) 
        neighbours (into [] (remove nil? row))]
    neighbours))

;; Edge Matrix Functions

(defn edge-matrix []
  [])

(defn increment-edge-matrix [a]
  (let [arrity (count a)
        extend-existing-rows (vec (map #(conj % nil) a))
        append-new-row (conj extend-existing-rows (vec (repeat (inc arrity) nil)))]
    append-new-row))
