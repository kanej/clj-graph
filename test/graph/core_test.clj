(ns graph.core-test
  (:use clojure.test
        graph.core))

(defn increment-2d-array [a]
  (let [arrity (count a)
        extend-existing-rows (vec (map #(conj % false) a))
        append-new-row (conj extend-existing-rows (vec (repeat (inc arrity) false)))]
    append-new-row))

(defn new-graph []
  {:nodes {} :edges []})

(defn add-node [g node]
  (let [old-nodes (:nodes g)
        new-nodes (assoc old-nodes node (count old-nodes))
        old-edges (:edges g)
        new-edges (increment-2d-array old-edges)]
    {:nodes new-nodes :edges new-edges}))

(defn add-edge [g from-node to-node]
  (let [from-node-index (get-in g [:nodes from-node])
        to-node-index   (get-in g [:nodes to-node])]
    (-> g 
      (assoc-in [:edges from-node-index to-node-index] true)
      (assoc-in [:edges to-node-index from-node-index] true)))) 

(defn edge? [g from-node to-node]
  (and (get-in g [:nodes from-node])
       (get-in g [:nodes to-node])
       (get-in g [:edges (get-in g [:nodes from-node]) (get-in g [:nodes to-node])])))

(deftest arrays-of-2d
  (testing "Incrementing the array"
    (let [empty-array []]
      (is (= [[false]] (increment-2d-array empty-array)))
      (is (= [[false false] [false false]] (increment-2d-array (increment-2d-array empty-array)))))))

(deftest creating-graphs
  (testing "Can create a graph"
    (is (not (nil? (new-graph))))
    (is (zero? (count (:nodes (new-graph))))))
  (testing "Can add a a node"
    (let [g (new-graph)]
      (is (= {:a 0} (:nodes (add-node g :a))))
      (is (= {:a 0 :b 1} (:nodes (add-node (add-node g :a) :b))))))
  (testing "can query for an edge"
    (let [g (-> (new-graph)
                (add-node :a)
                (add-node :b))]
      (is (not (edge? g :a :b)))
      (is (not (edge? g :non-existant :b)))
      (is (not (edge? g :a :non-existant)))
      (is (not (edge? g :b :a)))
      (is (edge? (add-edge g :a :b) :a :b)))
    (let [g (-> (new-graph)
                (add-node :a)
                (add-node :b)
                (add-node :c)
                (add-edge :a :b))] 
      (is (edge? g :a :b))
      (is (edge? g :b :a))

      (is (not (edge? g :a :c)))
      (is (not (edge? g :c :a)))

      (is (not (edge? g :b :c)))
      (is (not (edge? g :c :b)))

      (is (not (edge? g :a :a)))
      (is (not (edge? g :b :b)))
      (is (not (edge? g :c :c))))))

