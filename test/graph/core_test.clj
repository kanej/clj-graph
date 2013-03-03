(ns graph.core-test
  (:use clojure.test
        graph.core))

(deftest arrays-of-2d
  (testing "Incrementing the array"
    (let [empty-array []]
      (is (= [[nil]] (increment-2d-array empty-array)))
      (is (= [[nil nil] [nil nil]] (increment-2d-array (increment-2d-array empty-array)))))))

(deftest creating-graphs
  (testing "Can create a graph"
    (is (not (nil? (graph))))
    (is (zero? (count (:nodes (graph))))))
  (testing "Can add a a node"
    (let [g (graph)]
      (is (= {:a 0} (:nodes (add-node g :a))))
      (is (= {:a 0 :b 1} (:nodes (add-node (add-node g :a) :b))))))
  (testing "can query for an edge"
    (let [g (-> (graph)
                (add-node :a)
                (add-node :b))]
      (is (not (edge? g :a :b)))
      (is (not (edge? g :non-existant :b)))
      (is (not (edge? g :a :non-existant)))
      (is (not (edge? g :b :a)))
      (is (edge? (add-edge g :a :b) :a :b)))
    (let [g (-> (graph)
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

;; neighbours

(deftest finding-neighbours
  (testing "can determine neighbours in a simple graph"
    (let [g (-> (graph)
                (add-node :a)
                (add-node :b)
                (add-node :c)
                (add-edge :a :b)
                (add-edge :a :c))]
      (is (= [:b :c] (neighbours g :a)))
      (is (= [:a] (neighbours g :b)))
      (is (= [:a] (neighbours g :c))))))
