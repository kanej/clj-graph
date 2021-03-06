(ns clj-graph.algo.edge-collection
  (:refer-clojure :exclude [inc]))

(defprotocol EdgeCollection
  "A matrix for representing edge connections in a graph."
  (inc [em] "Increase the size of the matrix by one.")
  (lookup [em x y] "What is the value at index co-ordinate [x y]?")
  (update [em x y value] "Set the value at index co-ordinate [x y] to value."))

(extend-protocol EdgeCollection
  clojure.lang.IPersistentVector
  (inc [this] 
    (let [arrity (count this)
        extend-existing-rows (vec (map #(conj % nil) this))
        append-new-row (conj extend-existing-rows (vec (repeat (clojure.core/inc arrity) nil)))]
      append-new-row))
  (lookup [this x y]
    (get-in this [x y]))
  (update [this x y value]
    (assoc-in this [x y] value)))

(defn edge-matrix []
  [])
