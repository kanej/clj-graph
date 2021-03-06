(ns clj-graph.algo.vertex-queue)

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
    (reduce conj this vertexes))
  (dequeue [this]
    [(pop this) (peek this)]))

(def ^{:doc "First in first out empty queue implementing the VertexQueue protocol."}
  FIFO clojure.lang.PersistentVector/EMPTY)

(def ^{:doc "First in last out empty queue implementing the VertexQueue protocol."}
  FILO clojure.lang.PersistentQueue/EMPTY)
