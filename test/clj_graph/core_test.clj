(ns clj-graph.core-test
  (:use clojure.test)
  (:use [clj-graph.core] :reload))

(deftest arrays-of-2d
  (testing "Incrementing the array"
    (let [empty-array []]
      (is (= [[nil]] (increment-edge-matrix empty-array)))
      (is (= [[nil nil] [nil nil]] (increment-edge-matrix (increment-edge-matrix empty-array)))))))

(deftest creating-graphs
  (testing "Can create a graph"
    (is (not (nil? (graph))))
    (is (zero? (count (:vertexes (graph))))))
  (testing "Can add a a vertex"
    (let [g (graph)]
      (is (= {:a 0} (:vertexes (add-vertex g :a))))
      (is (= {:a 0 :b 1} (:vertexes (add-vertex (add-vertex g :a) :b))))))
  (testing "can query for an edge"

    (let [g (-> (graph)
                (add-vertex :a)
                (add-vertex :b))]
      ;; There are no edges until an edge is added 
      (is (not (edge? g :a :b)))
      (is (not (edge? g :b :a)))
      ;; Non-existant vertexes return false as well
      (is (not (edge? g :non-existant :b)))
      (is (not (edge? g :a :non-existant)))
      ;; There is an edge once it has been added
      (is (edge? (add-edge g :a :b) :a :b)))

    (let [g (-> (graph)
                (add-vertex :a)
                (add-vertex :b)
                (add-vertex :c)
                (add-edge :a :b))] 

      ;; Correctly detects the expected edges
      (is (edge? g :a :b))
      (is (edge? g :b :a))

      ;; detects that there is no edge from a to c
      ;; in either direction
      (is (not (edge? g :a :c)))
      (is (not (edge? g :c :a)))

      ;; Similarly for b to c
      (is (not (edge? g :b :c)))
      (is (not (edge? g :c :b)))

      ;; There should be no edges from a vertex
      ;; to itself
      (is (not (edge? g :a :a)))
      (is (not (edge? g :b :b)))
      (is (not (edge? g :c :c))))))

;; neighbours

(deftest finding-neighbours
  (testing "can determine neighbours in a simple graph"
    (let [g (-> (graph)
                (add-vertex :a)
                (add-vertex :b)
                (add-vertex :c)
                (add-edge :a :b)
                (add-edge :a :c))]
      (is (= [:b :c] (neighbours g :a)))
      (is (= [:a] (neighbours g :b)))
      (is (= [:a] (neighbours g :c))))))
